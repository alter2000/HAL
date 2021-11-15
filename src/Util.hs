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

import Types.Exceptions ( HALError )
import Types.AST
import Parser.AST
-- import Lib.AST
import Parser.ParseError

import Text.Pretty.Simple

-- | needs more flesh, usable while inside 'Control.Monad.Except.ExceptT'
halExcept :: HALError -> IO ()
halExcept = hPutStrLn stderr . displayException

-- | needs more flesh, usable while inside 'Control.Monad.Except.ExceptT'
parserExcept :: ParseError -> IO ()
parserExcept = hPutStrLn stderr . displayException

-- | catch /any/ exception
except :: SomeException -> IO a
except e = hPutStrLn stderr (displayException e) >> exitWith (ExitFailure 84)

settings :: Settings IO
settings = Settings
  { complete = completeFilename
  , historyFile = Just "./hal_history"
  , autoAddHistory = True
  }

repl :: IO ()
repl = runInputTBehavior defaultBehavior settings $ till $ do
  _nput <- do getInputLine "repl poggers :: "
              >>= maybe (liftIO $ putStrLn "no user input")
              (either (liftIO . parserExcept) pPrint
              . parse "<stdin>")
  pure False

interpretFile :: FilePath -> IO AST'
interpretFile f = do        -- maybe needs ErrorT to actually return useful value (env + AST')
  _ss' <- readFile f <&> either parserExcept evalStmts . parseFile f
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
