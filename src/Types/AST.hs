-- | Everything getting us from the language into an AST
-- and from an AST into a better AST
module Types.AST
  where


import Data.Functor.Classes
import Data.List as L
import qualified Data.Map as M

import RecursionSchemes ( Fix(..) )
import Types.Cofree as C
import Types.Pos
-- import Types.Exceptions ( HALError )

type VarName = String

-- | whole AST definition
data ASTF r = Atom !VarName
            | Int  !Integer
            | Bool !Bool
            -- | Real !Rational
            | Str  !String
          -- DUPES?
            | List ![r]
-- Instances {{{
instance Functor ASTF where
  -- TODO: handle every constructor
  fmap _ (Atom a) = Atom a
  fmap _  (Str a) = Str  a
  fmap _ (Bool a) = Bool a
  fmap _  (Int a) = Int a
  fmap f (List r) = List $ f <$> r

instance Foldable ASTF where
  foldMap _ (Atom _) = mempty
  foldMap _  (Str _) = mempty
  foldMap _ (Bool _) = mempty
  foldMap _  (Int _) = mempty
  foldMap f (List r) = foldMap f r
instance Traversable ASTF where
  traverse _ (Atom a) = pure $ Atom a
  traverse _  (Str a) = pure $ Str a
  traverse _ (Bool a) = pure $ Bool a
  traverse _  (Int a) = pure $ Int a
  traverse f (List r) = List <$> traverse f r

instance Show f => Show (ASTF f) where showsPrec = showsPrec1

instance Show1 ASTF where
  liftShowsPrec _ _ _ (Atom a) = showString a
  liftShowsPrec _ _ _  (Int s) = shows s
  liftShowsPrec _ _ _  (Str s) = showChar '"' . showString (concatMap
    (\a -> if a == '"' then "\\\"" else [a]) s) . showChar '"'
  liftShowsPrec _ _ _ (Bool a) = showString $ if a then "#t" else "#f"
  liftShowsPrec f _ p (List a) = showChar '(' . showList' (showChar ')') f p a

showList' :: ShowS -> (a -> b -> ShowS) -> a -> [b] -> ShowS
showList' end pf p a = foldr (.) end
  (L.intersperse (showChar ' ') $ pf p <$> a)
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
-- }}}
