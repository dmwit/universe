module Data.Universe.Instances.Show (
	-- | A 'Show' instance for functions, given the input is 'Finite' and both
	-- the input and output are 'Show'.
	Show(..)
	) where

import Data.Universe.Class
import Data.Universe.Instances.Base

instance (Finite a, Show a, Show b) => Show (a -> b) where
	showsPrec n f = showsPrec n [(a, f a) | a <- universeF]
