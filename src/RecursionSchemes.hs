-- | All the recursion
-- Really every kind of algorithm we'll ever need is built out of
-- a recursion scheme. Use these for hands-off, proved-correct reductions.
-- __TODO__: actually implement proper 'CVAlg'
module RecursionSchemes
  ( Fix(..)
  , cata
  , cataM

  , CVAlg
  , histo
  , histo'
  , histoA
  , para
  , paraA
  )
  where


import Data.Functor.Classes
import Data.Function (on)
import Control.Arrow ( (>>>), Arrow((&&&)) )
import Control.Monad ( (<=<) )

import Types.Cofree


-- Plain Fixpoint Functor, cata/anamorphisms {{{
-- | Fixpoint functor, grabs a free functor and spills it out when unwrapped
newtype Fix f = Fix { outF :: f (Fix f) }

instance Show1 f => Show (Fix f) where
  showsPrec d (Fix a) = liftShowsPrec showsPrec showList d a

instance Ord1 f => Ord (Fix f) where compare = compare1 `on` outF

instance  Eq1 f =>  Eq (Fix f) where (==) = eq1 `on` outF

-- | Catamorphism, i.e. right fold, i.e. reduction
cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = c where c = f . fmap c . outF

-- | 'cata' but giving out an effectful value
cataM :: (Monad m, Traversable t) => (t a -> m a) -> Fix t -> m a
cataM f = c where c = f <=< (traverse c . outF)
-- }}}

-- Histo/futumorphisms {{{
-- | CV-algebra, the type of function that will be called
-- to collapse the structure
type CVAlg f a = f (Cofree f a) -> a

-- | Histomorphism, can:
--
-- * recurse into a single item (like 'cata')
-- * look into the current step's fold (can optimize structure)
-- * look into every previous step (is a memoizer + can check if structure
--   optimizations cannot be carried out or if there's unbounded recursion
--   during optimization)
histo :: Functor f => CVAlg f a -> Fix f -> a
histo h = c >>> extract
  where c = outF >>> fmap c >>> h &&& id >>> uncurry (:<)
{-# INLINE histo #-}

histo' :: Functor f => CVAlg f a -> Fix f -> a
histo' h = outF >>> fmap c >>> h
  where c = histo' h &&& fmap c . outF >>> uncurry (:<)
{-# INLINE histo' #-}

-- | Histomorphism but with effectful values, maybe not necessary
histoA :: (Applicative a, Functor f) => CVAlg f (a r) -> Fix f -> a r
histoA = histo
-- }}}

-- Paramorphisms {{{
-- | Paramorphism, histomorphism minus the "can look at previous steps"
para :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
para f = histo $ fmap c >>> f
  where c (a :< h) = (Fix $ (c >>> fst) <$> h, a)

-- | paramorphism but with effectful values
paraA :: (Functor f, Applicative a)
      => (f (Fix f, a r) -> a r) -> Fix f -> a r
paraA = para
-- }}}
