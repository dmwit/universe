-- WHEN EDITING THIS FILE:
-- edit ../../nodefsigs/Data/Universe.hs in tandem!
{-# LANGUAGE DefaultSignatures #-}
module Data.Universe.Class where

-- | Creating an instance of this class is a declaration that your type is
-- recursively enumerable (and that 'universe' is that enumeration). In
-- particular, you promise that any finite inhabitant has a finite index in
-- 'universe', and that no inhabitant appears at two different finite indices.
class Universe a where
	universe :: [a]
	default universe :: (Enum a, Bounded a) => [a]
	-- WHEN EDITING THIS DEFINITION:
	-- edit ../../Data/Universe/Helpers.hs:universeDef in tandem!
	universe = [minBound .. maxBound]

-- | Creating an instance of this class is a declaration that your 'universe'
-- eventually ends. Minimal definition: no methods defined. By default,
-- @universeF = universe@, but for some types (like 'Either') the 'universeF'
-- method may have a more intuitive ordering.
class Universe a => Finite a where
	universeF :: [a]
	universeF = universe
