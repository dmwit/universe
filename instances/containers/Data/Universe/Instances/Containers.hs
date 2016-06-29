{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Instances for types from @containers@ package.
--
-- * @instance (Ord a, 'Universe' a) => 'Universe' (Set a)@
module Data.Universe.Instances.Containers (
    -- | Instances of 'Universe' and 'Finite' for built-in types.
    Universe(..), Finite(..)
    ) where

import Data.Universe.Class
import Data.Universe.Helpers

import qualified Data.Set as Set
-- import qualified Data.Map as Map

instance (Ord a, Universe a, Show a) => Universe (Set.Set a) where
    universeUniv = univCons Set.empty $ Univ $ go universe
      where
        go []     = []
        go (x:xs) = Set.singleton x : inter (go xs)
          where
            -- Probably more efficient than using (+++)
            inter []     = []
            inter (y:ys) = y : Set.insert x y : inter ys


instance (Ord a, Finite a, Show a) => Finite (Set.Set a)

-- This is tricky
-- instance (Ord k, Universe k, Universe v) => Universe (Map.Map k v)
--     universe = ...
