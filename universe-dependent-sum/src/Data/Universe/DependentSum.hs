{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Universe.DependentSum () where

import Data.Universe.Class (Universe (..), Finite (..))
import Data.Universe.Some (UniverseSome (..), FiniteSome (..))
import Data.Universe.Helpers (Tagged (..), Natural, (+++))

import           "some"          Data.Some (Some (..), foldSome)
import qualified "dependent-sum" Data.Some as DS

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

#if MIN_VERSION_dependent_sum(0,5,0)
mkSome :: f a -> DS.Some f
mkSome = DS.Some
#else
mkSome :: f a -> DS.Some f
mkSome = DS.This
#endif

-------------------------------------------------------------------------------
-- Instances for Some
-------------------------------------------------------------------------------

instance UniverseSome f => Universe (DS.Some f) where
  universe = map (foldSome mkSome) universeSome

instance FiniteSome f => Finite (DS.Some f) where
  universeF   = map (foldSome mkSome) universeFSome
  cardinality = retagSome cardinalitySome

retagSome :: Tagged (Some f) Natural -> Tagged (some f) Natural
retagSome (Tagged n) = Tagged n
