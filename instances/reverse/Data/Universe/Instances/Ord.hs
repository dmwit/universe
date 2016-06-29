{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Universe.Instances.Ord (
    -- | An 'Ord' instance for functions, given the input is 'Finite' and the
    -- output is 'Ord'. Compares pointwise, with higher priority to inputs
    -- that appear earlier in 'universeF'.
    Ord(..)
    ) where

import Data.Monoid
import Data.Universe.Class
import Data.Universe.Instances.Eq ()
import Prelude

instance (Finite a, Ord b) => Ord (a -> b) where
    f `compare` g = mconcat [f x `compare` g x | x <- universeF]
