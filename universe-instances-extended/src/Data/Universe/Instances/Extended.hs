{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
-- Data.Coerce is Unsafe
#if __GLASGOW_HASKELL__ >=704 && !MIN_VERSION_base(4,7,0)
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >=702
{-# LANGUAGE Trustworthy #-}
#endif
module Data.Universe.Instances.Extended (
  -- | Instances for 'Universe' and 'Finite' for function-like functors and the empty type.
  Universe(..), Finite(..)
  ) where

import Control.Comonad.Trans.Traced (TracedT (..))
import Data.Functor.Contravariant (Op (..), Predicate (..))
import Data.Functor.Rep (Representable (..), Co (..))
import Data.Map (Map)
import Data.Set (Set)
import Data.Universe.Class (Universe (..), Finite (..))
import Data.Universe.Helpers (retag, Tagged, Natural)

import qualified Data.Map as M
import qualified Data.Set as S

#if MIN_VERSION_base(4,7,0)
import Data.Coerce (coerce)
#endif

-- $setup
--
-- >>> import Data.Int (Int8)
-- >>> import Data.Functor.Contravariant (Predicate (..))
-- >>> import Data.Universe.Helpers (retag, Tagged, Natural)
--
-- -- Show (a -> b) instance (in universe-reverse-instances, but cannot depend on it here).
-- >>> instance (Finite a, Show a, Show b) => Show (a -> b) where showsPrec n f = showsPrec n [(a, f a) | a <- universeF]
--
-- >>> :set -XStandaloneDeriving
-- >>> deriving instance (Finite a, Show a) => Show (Predicate a)

-- | We could do this:
--
-- @
-- instance Universe (f a) => Universe (Co f a) where universe = map Rep universe
-- @
--
-- However, since you probably only apply Rep to functors when you want to
-- think of them as being representable, I think it makes sense to use an
-- instance based on the representable-ness rather than the inherent
-- universe-ness.
--
-- Please complain if you disagree!
--
instance (Representable f, Finite (Rep f), Ord (Rep f), Universe a)
  => Universe (Co f a)
  where universe = map tabulate universe
instance (Representable f, Finite s, Ord s, Finite (Rep f), Ord (Rep f), Universe a)
  => Universe (TracedT s f a)
  where universe = map tabulate universe

instance (Universe a, Finite b, Ord b) => Universe (Op a b) where
#if MIN_VERSION_base(4,7,0)
   universe = coerce (universe :: [b -> a])
#else
   universe = map Op universe
#endif
instance (Finite a, Ord a) => Universe (Predicate a) where
  universe = map (Predicate . flip S.member) universe

instance (Representable f, Finite (Rep f), Ord (Rep f), Finite a)
  => Finite (Co f a)
  where universeF = map tabulate universeF; cardinality = retag (cardinality :: Tagged (Rep (Co        f) -> a) Natural)
instance (Representable f, Finite s, Ord s, Finite (Rep f), Ord (Rep f), Finite a)
  => Finite (TracedT s f a)
  where universeF = map tabulate universeF; cardinality = retag (cardinality :: Tagged (Rep (TracedT s f)) Natural)

instance (Finite a, Finite b, Ord b) => Finite (Op a b) where
  cardinality = retag (cardinality :: Tagged (b -> a) Natural)

-- |
--
-- >>> mapM_ print (universe :: [Predicate Ordering])
-- Predicate {getPredicate = [(LT,False),(EQ,False),(GT,False)]}
-- Predicate {getPredicate = [(LT,True),(EQ,False),(GT,False)]}
-- Predicate {getPredicate = [(LT,False),(EQ,True),(GT,False)]}
-- Predicate {getPredicate = [(LT,True),(EQ,True),(GT,False)]}
-- Predicate {getPredicate = [(LT,False),(EQ,False),(GT,True)]}
-- Predicate {getPredicate = [(LT,True),(EQ,False),(GT,True)]}
-- Predicate {getPredicate = [(LT,False),(EQ,True),(GT,True)]}
-- Predicate {getPredicate = [(LT,True),(EQ,True),(GT,True)]}
--
-- Beware, function type universes are large...
--
-- >>> cardinality :: Tagged (Predicate Int8) Natural
-- Tagged 115792089237316195423570985008687907853269984665640564039457584007913129639936
--
-- ... but thanks to laziness, you can expect at least few:
--
-- >>> let Predicate f : _ = universe :: [Predicate Int8]
-- >>> f 0
-- False
--
instance (Finite a, Ord a) => Finite (Predicate a) where
  cardinality = retag (cardinality :: Tagged (Set a) Natural)
