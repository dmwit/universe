module Data.Universe.Instances.Eq (
	-- | An 'Eq' instance for functions, given the input is 'Finite' and the
	-- output is 'Eq'. Compares pointwise.
	) where

import Data.Monoid
import Data.Universe

instance (Finite a, Eq b) => Eq (a -> b) where
	f == g = and [f x == g x | x <- universeF]
