module Types.Interp
  where

import Control.Monad.Except as E
import Control.Monad.Reader as R
import Control.Monad.State

import Types.AST
import Types.Exceptions

type Interp = StateT Env Scope

-- | Lexical scope captured by lambda
type Scope = ReaderT Env (ExceptT HALError IO)
-- NO TYPE SAFETY BECAUSE TOO LAZY TO MANUALLY REWRITE EVERYTHING
-- THANKS EPITECH WHEN GeneralizedNewtypeDeriving ?????????????????
-- newtype Scope f = Scope { unScope :: ReaderT Env (ExceptT HALError IO) f }
--   deriving (Functor, Applicative, Monad, MonadReader Env, MonadError HALError, MonadIO)

runInterp :: Interp f -> Env -> Scope (f, Env)
runInterp = runStateT
