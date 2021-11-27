-- | Random useful functions most probably used as glue between other modules
module Util
    where

import System.Exit ( exitWith, ExitCode(..) )
import System.IO
import Control.Exception
import System.Console.Haskeline
import Control.Arrow

import Control.Monad
import Control.Monad.Trans
import Data.Char
import qualified Data.Map as M

import Types.Exceptions ( HALError )
import Types.AST
import Parser.AST
import Lib.AST
import Parser.ParseError

-- | needs more flesh, usable while inside 'Control.Monad.Except.ExceptT'
pExcept :: (String -> IO ()) -> IO a -> ParseError -> IO a
pExcept f = flip $ displayException >>> f >>> (>>)

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
repl env = runInputT settings $ till . fmap snd $ getInputLine "><> :: "
  >>= \input -> case input of
    Nothing -> pure (env, False)
    Just ":env" -> prettyPrintEnv env >> pure (env, True)
    Just i -> handleInput i env

handleInput :: MonadIO m => [Char] -> Env -> InputT m (Env, Bool)
handleInput i env | filter (not . isSpace) i == "" = pure (env, True)
  | otherwise = do
    pp <- getExternalPrint
    liftIO $ case parse i of
      Left ex -> pExcept pp (pure (env, True)) ex
      Right ast -> handle (except >>> (>> pure (env, True))) $ do
        (a', e') <- runStep ast env
        pp (show a') >> pure (e', True)

prettyPrintEnv :: Env -> InputT IO ()
prettyPrintEnv (Env e) = liftIO . mapM_ putStrLn . showKeyVal $ M.toList e
  where showKeyVal :: [(VarName, AST')] -> [String]
        showKeyVal = fmap $ \(a, b) -> a <> " : "<> show b


-- maybe needs ErrorT to actually return useful value (env + AST')
interpretFile :: Env -> FilePath -> IO (AST', Env)
interpretFile env f = readFile f >>= either
  (pExcept (hPutStrLn stderr) (exitWith (ExitFailure 84)))
  (evalFile env) . parseFile f

evalFile :: Env -> [AST'] -> IO (AST', Env)
evalFile env [] = pure (list [], env)
evalFile env [ast] = runStep ast env
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
