-- | The dual to @Free@, a Contravariant Functor usable as a Store
-- (of e.g. 'Types.Pos.Pos')
module Types.Cofree
  ( Cofree(..)
  , dep
  , Comonad(..)
  , ComonadTrans(..)
  )
  where

import Data.Functor.Classes
import Control.Applicative ( Alternative(..) )
import Control.Monad ( ap )

class Functor c => Comonad c where
  extract :: c a -> a

  duplicate :: c a -> c (c a)
  duplicate = extend id

  extend, (<<=) :: (c a -> b) -> c a -> c b
  extend f = fmap f . duplicate
  (<<=) = extend

  (=>>) :: c a -> (c a -> b) -> c b
  (=>>) = flip extend

class ComonadTrans t where
  lower :: t w r -> w r

-- | 'Cofree' Attribute wrapper similar to 'RecursionSchemes.Fix'.
data Cofree f a = a :< f (Cofree f a)

-- | The flipside of @'Control.Comonad.Comonad.extract'@.
dep :: Cofree f a -> f (Cofree f a)
dep (_ :< f) = f

instance Functor f => Functor (Cofree f) where
  fmap f (a :< r) = f a :< fmap (f <$>) r

instance Functor f => Comonad (Cofree f) where
  extend f w = f w :< fmap (extend f) (dep w)
  duplicate w = w :< fmap duplicate (dep w)
  extract (a :< _) = a

instance Alternative f => Applicative (Cofree f) where
  pure a = a :< empty
  (<*>) = ap

instance Alternative f => Monad (Cofree f) where
  (a :< m) >>= k = case k a of b :< n -> b :< (n <|> fmap (>>= k) m)

instance (Eq1 f) => Eq1 (Cofree f) where
  liftEq eq = go where go (a :< as) (b :< bs) = eq a b && liftEq go as bs

instance (Eq1 f, Eq m) => Eq (Cofree f m) where (==) = eq1

instance (Ord1 f) => Ord1 (Cofree f) where
  liftCompare c = go where go (a :< as) (b :< bs) =
                              c a b <> liftCompare go as bs

instance (Ord1 f, Ord m) => Ord (Cofree f m) where compare = compare1

instance (Show1 f) => Show1 (Cofree f) where
  liftShowsPrec sp sl = go
    where go d (a :< as) = showParen (d > 5) $ sp 6 a . showString " :< "
            . liftShowsPrec go (liftShowList sp sl) 5 as

instance (Show1 f, Show m) => Show (Cofree f m) where showsPrec = showsPrec1
