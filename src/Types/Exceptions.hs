-- | All exceptions and error types used in the project
module Types.Exceptions
  ( HALError(..)
  )
  where

import Control.Exception ( Exception(..) )

import Types.Pos ( Pos(..) )

type VarName = String

-- | TODO:
-- - what are the possible failure cases?
-- - needs any docs?
data HALError
  = UndefinedSymbol   { pos :: Pos , msg :: String  }
  | UndefinedProperty { pos :: Pos , msg :: String  }
  | MismatchedParens  { pos :: Pos , msg :: String  }
  | BadArguments      { pos :: Pos , have :: Int, need :: Int }
  | BadSpecialForm    { pos :: Pos , msg :: String  }
  | UnboundVar        { pos :: Pos , got :: VarName }
  | TypeMismatch      { pos :: Pos , got :: VarName }
  deriving (Show)

instance Exception HALError where
  displayException e = case e of
    UndefinedSymbol   at what -> sl at "undefined symbol: " <> what
    UndefinedProperty at what -> sl at "undefined property: " <> what
    MismatchedParens  at what -> sl at "mismatched parentheses: " <> what
    BadArguments      at h  n -> sl at "bad argument: needs " <> show n
      <> " arguments but got " <> show h
    BadSpecialForm    at what -> sl at "bad special form: " <> what
    UnboundVar        at what -> sl at "unbound symbol: " <> what
    TypeMismatch      at t    -> sl at "type error: " <> t

sl :: Pos -> String -> String
sl _at str =  " -- " <> str
