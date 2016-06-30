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
    universeUniv = univCons Set.empty $ go universeUniv
      where
        go :: Ord b => Univ b -> Univ (Set.Set b)
        go u = case univUncons u of
            Nothing -> emptyUniv
            Just (x, xs) -> univCons (Set.singleton x) $ inter (go xs)
              where
                -- Probably more efficient than using (+++)
                -- TODO: add to Helpers
                inter v = case univUncons v of
                    Nothing     -> emptyUniv
                    Just (y,ys) -> univCons y $ univCons (Set.insert x y) $
                        inter ys

instance (Ord a, Finite a, Show a) => Finite (Set.Set a)

-- This is tricky
-- instance (Ord k, Universe k, Universe v) => Universe (Map.Map k v)
--     universe = ...
