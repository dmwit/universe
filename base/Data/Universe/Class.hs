{-# LANGUAGE CPP #-}
#ifdef DEFAULT_SIGNATURES
{-# LANGUAGE DefaultSignatures #-}
#endif
module Data.Universe.Class
    ( -- | Bottoms are ignored for this entire module: only fully-defined inhabitants are considered inhabitants.
      Universe(..)
    , Finite(..)
    , Univ(..)
    ) where

import Data.Universe.Helpers

-- | Creating an instance of this class is a declaration that your type is
-- recursively enumerable (and that 'universe' is that enumeration). In
-- particular, you promise that any finite inhabitant has a finite index in
-- 'universe', and that no inhabitant appears at two different finite indices.
--
-- Well-behaved instance should produce elements lazily.
--
-- /Laws:/
--
-- @
-- 'elem' x 'universe'                    -- any inhabitant has a finite index
-- let pfx = 'take' n 'universe'          -- any finite prefix of universe has unique elements
-- in 'length' pfx = 'length' (nub pfx)
-- @
class Universe a where
    -- | Memoised CAF. Use with care as the expanded value will live forever.
    --
    -- See <http://stackoverflow.com/questions/6090932/how-to-make-a-caf-not-a-caf-in-haskell>
    universe :: [a]
    universe = getUniv universeUniv

    -- | 'Univ' containing all values of @a@.
    universeUniv :: Univ a
#ifdef DEFAULT_SIGNATURES
    default universeUniv :: (Enum a, Bounded a) => Univ a
    universeUniv = universeDef
#endif

-- | Creating an instance of this class is a declaration that your 'universe'
-- eventually ends. Minimal definition: no methods defined. By default,
-- @universeF = universe@, but for some types (like 'Either') the 'universeF'
-- method may have a more intuitive ordering.
--
-- /Laws:/
--
-- @
-- 'elem' x 'universeF'                       -- any inhabitant has a finite index
-- 'length' ('filter' (== x) 'universeF') == 1  -- should terminate
-- @
--
-- /Note:/ @'elemIndex' x 'universe' == 'elemIndex' x 'universeF'@
-- may not hold for all types, though the laws imply that `universe`
-- is a permutation of `universeF`.
--
-- @
-- >>> elemIndex (Left True :: Either Bool Bool) universe
-- Just 2
-- >>> elemIndex (Left True :: Either Bool Bool) universeF
-- Just 1
-- @
class Universe a => Finite a where
    universeF :: [a]
    universeF = getUniv universeUnivF

    universeUnivF :: Univ a
    universeUnivF = universeUniv
