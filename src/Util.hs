-- | Random useful functions most probably used as glue between other modules
module Util
    where

import Control.Monad
import System.Exit ( exitWith, ExitCode(..) )
import System.IO
import Control.Exception
import System.Console.Haskeline

import Types.Exceptions ( HALError )
import Types.AST
import Parser.Parser
-- import Eval

-- | needs more flesh, usable while inside 'Control.Monad.Except.ExceptT'
halExcept :: HALError -> IO ()
halExcept = hPutStrLn stderr . displayException

-- | catch /any/ exception
except :: SomeException -> IO a
except e = hPutStrLn stderr (displayException e) >> exitWith (ExitFailure 84)

settings :: Settings IO
settings = Settings
  { complete = completeFilename
  , historyFile = Just "./hal_history"
  , autoAddHistory = True
  }

repl :: Either String Behavior -> IO ()
repl (Left fpath) = runInputTBehavior (useFile fpath) settings $ till $ do
  undefined
repl (Right b) = runInputTBehavior b settings $ till $ do
  undefined

{-
interpretFile :: FilePath -> IO (AST', Env)
interpretFile = readFile >=> pure . parse >=> evalStmts

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
till p = loop where loop = p >>= flip when loop
