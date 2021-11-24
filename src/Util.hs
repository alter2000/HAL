-- | Random useful functions most probably used as glue between other modules
module Util
    where

import Control.Monad
import Control.Monad.Trans
import Data.Functor
import System.Exit ( exitWith, ExitCode(..) )
import System.IO
import Control.Exception
import System.Console.Haskeline
import Data.Char

import Types.Exceptions ( HALError )
import Types.AST
import Parser.AST
import Lib.AST
import Parser.ParseError

-- | needs more flesh, usable while inside 'Control.Monad.Except.ExceptT'
pExcept :: (String -> IO ()) -> ParseError -> IO ()
pExcept f = f . displayException

-- | separate from 'parserExcept' to carry over elsewhere
replExcept :: HALError -> IO ()
replExcept = hPutStrLn stderr . displayException

-- | catch /any/ exception
except :: SomeException -> IO ()
except = hPutStrLn stderr . displayException

-- | actually stop the program
halt :: SomeException -> IO a
halt e = except e >> exitWith (ExitFailure 84)

settings :: Settings IO
settings = Settings
  { complete = completeFilename -- TODO: custom from primEnv <> curEnv?
  , historyFile = Just "./hal_history"
  , autoAddHistory = True
  }

repl :: Env -> IO ()
repl env = runInputT settings $ till . fmap snd $ do
  input <- getInputLine "><> fishy :: "
  case input of
    Nothing -> pure (env, False)
    Just a | filter (not . isSpace) a == "" -> pure (env, True)
           | otherwise -> replStep env a

replStep :: Env -> String -> InputT IO (Env, Bool)
replStep env a = getExternalPrint >>= \pp -> liftIO $ case parse a of
  Left ex -> pExcept pp ex >> pp "unpoggers" >> pure (env, True)
  Right ast -> handle (\e -> halExcept e >> pure (env, True)) $
    runStep ast env >>= \(a', e') -> pp (show a') >> pure (e', True)


-- maybe needs ErrorT to actually return useful value (env + AST')
interpretFile :: FilePath -> IO AST'
interpretFile f = do
  _ss' <- readFile f <&> either (pExcept $ hPutStrLn stderr) evalStmts . parseFile f
  putStrLn "poggers"
  pure $ atom "poggers"
  where evalStmts = undefined

-- TODO: function that
-- gets string
-- gets Env
-- parses string
-- works in either Either or ErrorT smth
-- evaluates single expression in Env
-- returns single expression result + new Env

{-
-- | interpret list of files, then return resulting env and return value
interpret :: Monad m =>
     String -- ^ file input
  -> Env    -- ^ base environment
  -> m (AST', Env)
interpret = undefined
-}

defaulting :: Foldable f => b -> (f a -> b) -> f a -> b
defaulting d f xs
  | null xs   = d
  | otherwise = f xs

till :: Monad m => m Bool -> m ()
till p = go where go = p >>= flip when go
