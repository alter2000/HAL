-- | @Validation@ but without a package
module Types.Valid
  ( Valid(..)
  )
  where


import Control.Applicative

-- | Like Either, but instead of returning after first error,
-- wait and collect every error into `e` then end
data Valid e r = Invalid e | Valid r
  deriving (Read, Show, Ord, Eq)

instance Semigroup e => Semigroup (Valid e a) where
  s@Valid{} <> _ = s
  _ <> s@Valid{} = s
  Invalid e1 <> Invalid e2 = Invalid $ e1 <> e2

instance Functor (Valid e) where
    fmap _ (Invalid e) = Invalid e
    fmap f (Valid a) = Valid $ f a

instance Semigroup e => Applicative (Valid e) where
  pure = Valid
  Invalid e <*> Invalid n = Invalid $ e <> n
  Invalid e <*> Valid _   = Invalid e
  Valid _   <*> Invalid e = Invalid e
  Valid f   <*> Valid b   = Valid   $ f b

instance Monoid e => Alternative (Valid e) where
  empty = Invalid mempty
  Invalid a <|> Invalid b = Invalid $ a <> b
  s@Valid{} <|> _ = s
  _ <|> s@Valid{} = s

instance Foldable (Valid e) where
  foldr f p (Valid a) = f a p
  foldr _ p Invalid{} = p

instance Traversable (Valid e) where
  sequenceA (Invalid e) = pure $ Invalid e
  sequenceA (Valid r)   = Valid <$> r
