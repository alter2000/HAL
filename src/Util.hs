-- | Random useful functions most probably used as glue between other modules
module Util
    where

import System.Exit ( exitWith, ExitCode(..) )
import System.IO
import Control.Exception
import System.Console.Haskeline

import Control.Monad
import Control.Monad.Trans
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

-- | TODO: custom from primEnv <> curEnv?
-- will need to grab 'Env' from 'Interp', therefore build @InputT Interp a@
settings :: Settings IO
settings = Settings
  { complete = completeFilename
  , historyFile = Just "./hal_history"
  , autoAddHistory = True
  }

repl :: Env -> IO ()
repl env = runInputT settings $ till . fmap snd $ getInputLine "><> fishy :: "
  >>= \input -> case input of
    Nothing -> pure (env, False)
    Just i | filter (not . isSpace) i == "" -> pure (env, True)
           | otherwise -> getExternalPrint >>= \pp -> liftIO $ case parse i of
      Left ex -> pExcept pp ex >> pure (env, True)
      Right ast -> handle (\e -> except e >> pure (env, True)) $
        runStep ast env >>= \(a', e') -> pp (show a') >> pure (e', True)


-- maybe needs ErrorT to actually return useful value (env + AST')
interpretFile :: Env -> FilePath -> IO (AST', Env)
interpretFile env f = readFile f >>= either
  (\pe -> pExcept (hPutStrLn stderr) pe >> exitWith (ExitFailure 84))
  (evalFile env) . parseFile f

evalFile :: Env -> [AST'] -> IO (AST', Env)
evalFile env [] = pure (list [], env)
evalFile env (ast:asts) = runStep ast env >>= flip evalFile asts . snd

-- | interpret list of files, then return resulting env and return value
interpret :: Env -> [String] -> IO (AST', Env)
interpret env [] = pure (list [], env)
interpret env [fp] = interpretFile env fp
interpret env (fp:fps) = interpretFile env fp >>= flip interpret fps . snd

defaulting :: Foldable f => b -> (f a -> b) -> f a -> b
defaulting d f xs
  | null xs   = d
  | otherwise = f xs

till :: Monad m => m Bool -> m ()
till p = go where go = p >>= flip when go
