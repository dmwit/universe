-- | A 'Read' instance for functions, given the input is 'Finite' and
-- 'Ord' and both the input and output are 'Read'.
module Data.Universe.Instances.Read () where

import Data.Universe.Class (Finite (..))

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Monoid as Mon

instance (Finite a, Ord a, Read a, Read b) => Read (a -> b) where
  readsPrec n s =
    [ ((m Map.!), s')
    | (v, s') <- readsPrec n s
    , let m = Map.fromList v
    , Map.keysSet m == Set.fromList universeF
    ]

instance (Finite a, Ord a, Read a) => Read (Mon.Endo a) where
  readsPrec d = readParen (d > 10) $ \s0 ->
    [ (Mon.Endo f, s2)
    | ("Endo", s1) <- lex s0
    , (f, s2) <- readsPrec 11 s1
    ]
