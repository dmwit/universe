{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
-- Data.Type.Equality is Trustworthy since base-4.9
#if __GLASGOW_HASKELL__ >=704 && MIN_VERSION_base(4,9,0)
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >=702
{-# LANGUAGE Trustworthy #-}
#endif
module Data.Universe.Some (
  UniverseSome (..),
  FiniteSome (..),
  ) where

import Data.Functor.Sum (Sum (..))
import Data.List (genericLength)
import Data.Some (Some, mapSome, mkSome, foldSome)
import Data.Type.Equality ((:~:) (..))
import Data.Universe.Class (Universe (..), Finite (..))
import Data.Universe.Helpers (Tagged (..), Natural, (+++))

import qualified Data.Some.GADT as G
import qualified Data.Some.Newtype as N
import qualified Data.Some.Church as C

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
-- /Note:/ The 'Some' type is imported from "Data.Some", i.e. maybe
-- either from "Data.Some.Newtype" (default) or "Data.Some.GADT" modules.
--
class UniverseSome f where
  universeSome :: [Some f]

class UniverseSome f => FiniteSome f where
  universeFSome :: [Some f]
  universeFSome = universeSome

  cardinalitySome :: Tagged (Some f) Natural
  cardinalitySome = Tagged (genericLength (universeFSome :: [Some f]))

-------------------------------------------------------------------------------
-- Instances for Some
-------------------------------------------------------------------------------

instance UniverseSome f => Universe (N.Some f) where
  universe = map (foldSome N.mkSome) universeSome

instance FiniteSome f => Finite (N.Some f) where
  universeF   = map (foldSome N.mkSome) universeFSome
  cardinality = retagSome cardinalitySome

instance UniverseSome f => Universe (G.Some f) where
  universe =  map (foldSome G.mkSome) universeSome

instance FiniteSome f => Finite (G.Some f) where
  universeF   = map (foldSome G.mkSome) universeFSome
  cardinality = retagSome cardinalitySome

instance UniverseSome f => Universe (C.Some f) where
  universe =  map (foldSome C.mkSome) universeSome

instance FiniteSome f => Finite (C.Some f) where
  universeF   = map (foldSome C.mkSome) universeFSome
  cardinality = retagSome cardinalitySome

retagSome :: Tagged (Some f) Natural -> Tagged (some f) Natural
retagSome (Tagged n) = Tagged n

-------------------------------------------------------------------------------
-- Type equality is singleton
-------------------------------------------------------------------------------

instance UniverseSome ((:~:) a) where
  universeSome = [mkSome Refl]

instance FiniteSome ((:~:) a) where
  cardinalitySome = 1

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
