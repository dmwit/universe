{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Data.Universe.Some (
  UniverseSome (..),
  FiniteSome (..),
  ) where

import Data.Functor.Sum (Sum (..))
import Data.List (genericLength)
import Data.Some (Some (..))
import Data.Universe.Class (Universe (..), Finite (..))
import Data.Universe.Helpers (Tagged (..), Natural, (+++))

#if MIN_VERSION_base(4,7,0)
import Data.Type.Equality ((:~:) (..))
#else
import Data.GADT.Compare ((:=) (..))
#endif

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- | Auxiliary class to power @'Universe' ('Some' f)@ instance.
-- (There are no good reasons to use @FlexibleInstances@).
--
-- Laws are induced via @'Universe' ('Some' f)@ instance. For example:
--
-- @
-- 'elem' x 'universe' ==> 'elem' ('Some' f) 'universeSome'
-- @
--
-- As 'Data.GADT.Compare.GEq' cannot be written for phantom 'Functor's, e.g.
-- 'Control.Applicative.Const' or 'Data.Proxy.Proxy', they cannot have
-- 'UniverseSome' instance either.
--
class UniverseSome f where
  universeSome :: [Some f]

class UniverseSome f => FiniteSome f where
  universeFSome :: [Some f]
  universeFSome = universeSome

  cardinalitySome :: Tagged (Some f) Natural
  cardinalitySome = Tagged (genericLength (universeFSome :: [Some f]))

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

#if MIN_VERSION_dependent_sum(0,5,0)
mkSome :: f a -> Some f
mkSome = Some

mapSome :: (forall x. f x -> g x) -> Some f -> Some g
mapSome nt (Some f) = Some (nt f)
#else
mkSome :: f a -> Some f
mkSome = This

mapSome :: (forall x. f x -> g x) -> Some f -> Some g
mapSome nt (This f) = This (nt f)
#endif

-------------------------------------------------------------------------------
-- Instances for Some
-------------------------------------------------------------------------------

instance UniverseSome f => Universe (Some f) where
  universe = universeSome

instance FiniteSome f => Finite (Some f) where
  universeF   = universeFSome
  cardinality = cardinalitySome

-------------------------------------------------------------------------------
-- Type equality is singleton
-------------------------------------------------------------------------------

#if MIN_VERSION_base(4,7,0)
instance UniverseSome ((:~:) a) where
  universeSome = [mkSome Refl]

instance FiniteSome ((:~:) a) where
  cardinalitySome = 1
#else
instance UniverseSome ((:=) a) where
  universeSome = [mkSome Refl]

instance FiniteSome ((:=) a) where
  cardinalitySome = 1
#endif

-------------------------------------------------------------------------------
-- Functors
-------------------------------------------------------------------------------

instance (UniverseSome f, UniverseSome g) => UniverseSome (Sum f g) where
  universeSome = map (mapSome InL) universeSome +++ map (mapSome InR) universeSome

instance (FiniteSome f, FiniteSome g) => FiniteSome (Sum f g) where
  universeFSome = map (mapSome InL) universeFSome ++ map (mapSome InR) universeFSome

-- Note: Product instance is tricky, we could for special cases.
-- e.g. '(GEq f, f ~ g) => UnvierseSome (Product f g)', but this is boring
-- instance as we'd 'Pair' equal instances only.
