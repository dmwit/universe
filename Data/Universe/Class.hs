{-# LANGUAGE CPP #-}
#ifdef DEFAULT_SIGNATURES
{-# LANGUAGE DefaultSignatures #-}
#endif
module Data.Universe.Class where

#ifdef DEFAULT_SIGNATURES
import Data.Universe.Helpers (universeDef)
#endif

-- | Creating an instance of this class is a declaration that your type is
-- recursively enumerable (and that 'universe' is that enumeration). In
-- particular, you promise that any finite inhabitant has a finite index in
-- 'universe', and that no inhabitant appears at two different finite indices.
class Universe a where
	universe :: [a]
#ifdef DEFAULT_SIGNATURES
	default universe :: (Enum a, Bounded a) => [a]
	universe = universeDef
#endif

-- | Creating an instance of this class is a declaration that your 'universe'
-- eventually ends. Minimal definition: no methods defined. By default,
-- @universeF = universe@, but for some types (like 'Either') the 'universeF'
-- method may have a more intuitive ordering.
class Universe a => Finite a where
	universeF :: [a]
	universeF = universe
