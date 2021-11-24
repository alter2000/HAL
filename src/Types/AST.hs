-- | Everything getting us from the language into an AST
-- and from an AST into a better AST
module Types.AST
  where


import Data.Functor.Classes
import Control.Arrow
import Data.List as L
import qualified Data.Map as M

import RecursionSchemes ( Fix(..) )
import Types.Cofree as C
-- import Types.Pos
import Control.Monad.State
import Control.Monad.Reader

type VarName = String
type Interp = StateT Env (ReaderT Env IO)

newtype Env = Env { getEnv :: M.Map VarName AST' }

-- | whole AST definition
data ASTF r = Atom !VarName
            | Int  !Integer
            | Bool !Bool
            -- | Real !Rational
            | Str  !String
            | List ![r]
            | DottedList ![r] !r
            -- lambda grabs closure from current env (a la python)
            | Builtin (Func (StateT Env (ReaderT Env IO)))
            | Lambda { fnArgs :: ![VarName]
                     , fnBody :: ![r]
                     , fnEnv  :: Env }

newtype Func m = Func { getFn :: [AST'] -> m AST' }

instance Show (Func m) where show _ = "#<func>"

-- Instances {{{
instance Functor ASTF where
  fmap _ (Atom a) = Atom a
  fmap _  (Str a) = Str  a
  fmap _ (Bool a) = Bool a
  fmap _  (Int a) = Int a
  fmap f (List r) = List $ f <$> r
  fmap f (DottedList r h) = DottedList (f <$> r) (f h)
  fmap f (Lambda p b c)   = Lambda p (f <$> b) c
  fmap _ (Builtin p) = Builtin p
  -- fmap _ (Real a) = Real a

instance Foldable ASTF where
  foldMap _ (Atom _) = mempty
  foldMap _  (Str _) = mempty
  foldMap _ (Bool _) = mempty
  foldMap _  (Int _) = mempty
  foldMap f (List r) = foldMap f r
  foldMap f (DottedList r h) = foldMap f r <> f h
  foldMap f (Lambda _ b _)   = foldMap f b
  foldMap _ (Builtin _) = mempty
  -- foldMap _ (Real _) = mempty

instance Traversable ASTF where
  traverse _ (Atom a) = pure $ Atom a
  traverse _  (Str a) = pure $ Str a
  traverse _ (Bool a) = pure $ Bool a
  traverse _  (Int a) = pure $ Int a
  traverse f (List r) = List <$> traverse f r
  traverse f (DottedList xs x) = DottedList <$> traverse f xs <*> f x
  traverse f (Lambda ps b ctx) = flip (Lambda ps) ctx <$> traverse f b
  traverse _ (Builtin p) = pure $ Builtin p
  -- traverse _ (Real a) = pure $ Real a

instance Show1 ASTF where
  liftShowsPrec _ _ _ (Atom a) = showString a
  liftShowsPrec _ _ _  (Int s) = shows s
  liftShowsPrec _ _ _  (Str s) = showChar '"' . showString (concatMap
    (\a -> if a == '"' then "\\\"" else [a]) s) . showChar '"'
  liftShowsPrec _ _ _ (Bool a) = showString $ if a then "#t" else "#f"
  liftShowsPrec f _ p (List a) = showChar '(' . showList' (showChar ')') f p a
  liftShowsPrec pf _ p (DottedList a h) = showChar '(' . showList'
    (showString " . " . pf p h . showChar ')') pf p a
  liftShowsPrec _ _ _ Lambda{} = showString "#<procedure>"
  liftShowsPrec _ _ _ Builtin{} = showString "#<primitive>"
  -- liftShowsPrec _ _ _ (Real s) = shows (fromRational s :: Double)

showList' :: ShowS -> (a -> b -> ShowS) -> a -> [b] -> ShowS
showList' end pf p =
  fmap (pf p) >>> L.intersperse (showChar ' ') >>> foldr (.) end
-- }}}

type AST' = Fix ASTF

-- Smart constructors {{{

list :: [AST'] -> AST'
list = Fix . List

atom :: VarName -> AST'
atom = Fix . Atom

str :: String -> AST'
str = Fix . Str

int :: Integer -> AST'
int = Fix . Int

bool :: Bool -> AST'
bool = Fix . Bool

mkBuiltin :: Func Interp -> AST'
mkBuiltin = Fix . Builtin

mkQuote :: AST' -> AST'
-- mkQuote (Fix(List as)) = list $ atom "quote" :as
mkQuote a = list [atom "quote", a]

dlist :: [AST'] -> AST' -> AST'
dlist as = Fix . DottedList as

func :: [VarName] -> [AST'] -> Env -> AST'
func ps body ctx = Fix $ Lambda ps body ctx

-- }}}
