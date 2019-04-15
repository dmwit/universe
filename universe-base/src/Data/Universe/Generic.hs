{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Universe.Generic where

import GHC.Generics

import Data.Universe.Class
import Data.Universe.Helpers

-- $setup
-- >>> :set -XDeriveGeneric -XEmptyDataDeriving
 
class GUniverse f where
  guniverse :: [f a]

instance GUniverseSum f => GUniverse (M1 i c f) where
  guniverse = map M1 $ interleave $ guniverseSum

class GUniverseSum f where
  guniverseSum :: [[f a]]

instance GUniverseSum V1 where
  guniverseSum = []

instance (GUniverseSum f, GUniverseSum g) => GUniverseSum (f :+: g) where
  guniverseSum = map (map L1) guniverseSum ++ map (map R1) guniverseSum

instance GUniverseProduct f => GUniverseSum (M1 i c f) where
  guniverseSum = [map M1 guniverseProduct]

class GUniverseProduct f where
  guniverseProduct :: [f a]

instance GUniverseProduct U1 where
  guniverseProduct = [U1]

-- This is not completely fair; but enough.
instance (GUniverseProduct f, GUniverseProduct g) => GUniverseProduct (f :*: g) where
  guniverseProduct = cartesianProduct (:*:) guniverseProduct guniverseProduct

instance GUniverseProduct f => GUniverseProduct (M1 i c f) where
  guniverseProduct = map M1 guniverseProduct

instance Universe a => GUniverseProduct (K1 r a) where
  guniverseProduct = map K1 universe

-- |
--
-- >>> data Zero deriving (Show, Generic)
-- >>> universeGeneric :: [Zero]
-- []
--
-- >>> data One = One deriving (Show, Generic)
-- >>> universeGeneric :: [One] 
-- [One]
--
-- >>> data Big = B0 Bool Bool | B1 Bool deriving (Show, Generic)
-- >>> universeGeneric :: [Big]
-- [B0 False False,B1 False,B0 False True,B1 True,B0 True False,B0 True True]
--
-- >>> universeGeneric :: [Maybe Ordering]
-- [Nothing,Just LT,Just EQ,Just GT]
--
-- >>> take 10 (universeGeneric :: [Either Integer Integer])
-- [Left 0,Right 0,Left 1,Right 1,Left (-1),Right (-1),Left 2,Right 2,Left (-2),Right (-2)]
--
-- >>> take 10 (universeGeneric :: [(Integer, Integer, Integer)])
-- [(0,0,0),(0,0,1),(1,0,0),(0,1,0),(1,0,1),(-1,0,0),(0,0,-1),(1,1,0),(-1,0,1),(2,0,0)]
--
universeGeneric :: (Generic a, GUniverse (Rep a)) => [a]
universeGeneric = map to guniverse 
