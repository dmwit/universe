{-# LANGUAGE ScopedTypeVariables #-}
module Data.Universe.Instances.Enum (
	Enum(..), EnumLoop(..)
	) where

import Data.Universe.Instances.Base

instance (Finite a, EnumLoop b) => Enum (a, b) where
	succ (a, b) = let (looped, b') = succLoop b in (if looped then succ a else a, b')
	pred (a, b) = let (looped, b') = predLoop b in (if looped then pred a else a, b')
	toEnum n = case n `divMod` cardinality ([] :: [a]) of
		(d, m) = (universeF `genericIndex` m, toEnum d)
	fromEnum (a, b) = fromEnum b * cardinality [a] + head (findIndices (==a) universeF)


-- | The methods of this class are similar to the methods of the 'Enum' class
-- in behavior, except that instead of ending their result lists when they
-- reach the end of their value supply, they do a suitable loop back to the
-- beginning, producing infinite lists.
class Enum a => EnumLoop a where
	enumFromLoop     :: a -> [a]
	enumFromThenLoop :: a -> a -> [a]

-- | A default for 'enumFromLoop' when the 'Finite' instance happens to match
-- the 'Enum' instance.
enumFromLoopDefault :: (Eq a, Finite a) => a -> [a]
enumFromLoopDefault f = dropWhile (/=f) (cycle universeF)

-- | A default for 'enumFromThenLoop' when the 'Finite' instance happens to
-- match the 'Enum' instance.
enumFromThenLoopDefault :: forall a. (Eq a, Finite a) => a -> a -> [a]
enumFromThenLoopDefault f t = cycle (f:toCycle) where
	loop    = enumFromLoopDefault f
	len     = length (takeWhile (/=t) loop)
	heads   = (>>= take 1)
	toCycle = takeWhile (/=f) . heads . drop 1 . iterate (drop len) $ loop
