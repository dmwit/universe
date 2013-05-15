module Data.Universe.Instances.Traversable (
	-- | A 'Foldable' instance for functions, given the input is 'Finite', and
	-- a 'Traversable' instance for functions, given the input is 'Ord' and
	-- 'Finite'.
	) where

import Control.Applicative
import Data.Foldable
import Data.Map ((!), fromList)
import Data.Monoid
import Data.Traversable
import Data.Universe

instance Finite e => Foldable ((->) e) where
	foldMap f g = mconcat $ map (f . g) universeF

instance (Ord e, Finite e) => Traversable ((->) e) where
	sequenceA f = (!) . fromList <$> sequenceA [(,) x <$> f x | x <- universeF]
