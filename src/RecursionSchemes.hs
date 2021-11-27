-- | All the recursion
-- Really every kind of algorithm we'll ever need is built out of
-- a recursion scheme. Use these for hands-off, proved-correct reductions.
-- __TODO__: actually implement proper 'CVAlg'
module RecursionSchemes
  ( Fix(..)
  , cata
  , MAlg
  , cataM
  , cataMCps

  , CVAlg
  , histo
  , histo'
  , RAlg
  , para
  , paraM
  , RMAlg
  , revCataM
  )
  where


import Data.Functor.Classes
import Data.Function (on)
import Control.Arrow ( (>>>), Arrow((&&&)) )
import Control.Monad.Cont

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
cataM :: (Traversable t, Monad m) => MAlg t m a -> Fix t -> m a
cataM f = c where c = f <=< (traverse c . outF)

cataMCps :: (Traversable t, Monad m) => MAlg t m a -> Fix t -> m a
cataMCps f x = runContT (rec f x) pure
  where
    rec :: (Traversable f, Monad m) => (f a -> m a) -> Fix f -> ContT a m a
    rec alg (Fix ex) = mapM (rec alg) ex >>= ContT . (>>=) . alg

type MAlg f m a = f a -> m a

-- | catamorphism reversed, i.e root-to-leaf, NOT anamorphism
genRevCata :: (Traversable f, Monad m)
        => (t -> m a) -- ^ algebra to use
        -> Fix f      -- ^ structure to catamorph
        -> (a -> m a) -- ^ continuation
        -> t          -- ^ still not sure but thanks GHC
        -> m a
genRevCata f = cataMCps (pure c)
  where c k z = f z >>= k

-- | Utility function to avoid using generator counterpart for normal
-- structures. Unwraps structure once to appease the type gods.
revCataM :: (Traversable f, Monad m)
         => (f (Fix f) -> m a) -- ^ algebra
         -> Fix f              -- ^ structure
         -> m a
revCataM alg x = genRevCata alg x pure (outF x)

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
-- }}}

-- Paramorphisms {{{
type RAlg f a = f (Fix f, a) -> a
-- | Paramorphism, histomorphism minus the "can look at previous steps"
para :: Functor f => RAlg f a -> Fix f -> a
para f = histo $ fmap c >>> f
  where c (a :< h) = (Fix $ (c >>> fst) <$> h, a)

type RMAlg f m a = f (Fix f, a) -> m a
paraM :: (Monad m, Traversable t, Foldable t)
      => RMAlg t m a -> Fix t -> m a
paraM f = c where c   = f <=< traverse bundle . outF
                  bundle t = (,) t <$> c t
-- }}}
