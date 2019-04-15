module Data.Universe.Instances.Ord (
  -- | An 'Ord' instance for functions, given the input is 'Finite' and the
  -- output is 'Ord'. Compares pointwise, with higher priority to inputs
  -- that appear earlier in 'universeF'.
  Ord(..)
  ) where

import qualified Data.Monoid as Mon
import Data.Universe.Class (Finite (..))
import Data.Universe.Instances.Eq

instance (Finite a, Ord b) => Ord (a -> b) where
  f `compare` g = Mon.mconcat [f x `compare` g x | x <- universeF]

instance (Finite a, Ord a) => Ord (Mon.Endo a) where
  compare (Mon.Endo f) (Mon.Endo g) = compare f g
