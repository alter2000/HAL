module Types.Interp
  where

import Control.Monad.Reader as R
import Control.Monad.State

import Types.AST

-- | Lexical scope captured by lambda
type Scope = ReaderT Env IO
-- NO TYPE SAFETY BECAUSE TOO LAZY TO MANUALLY REWRITE EVERYTHING
-- THANKS EPITECH WHEN GeneralizedNewtypeDeriving ?????????????????
-- newtype Scope f = Scope { unScope :: ReaderT Env (ExceptT HALError IO) f }
--   deriving (Functor, Applicative, Monad, MonadReader Env, MonadError HALError, MonadIO)

runInterp :: Interp f -> Env -> Scope (f, Env)
runInterp = runStateT

resolveScope :: Scope f -> Env -> IO f
resolveScope = runReaderT
