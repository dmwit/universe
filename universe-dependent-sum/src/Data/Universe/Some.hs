{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Universe.Some where

import Data.List (genericLength)
import Data.Some (Some (..))
import Data.Universe.Class (Universe (..), Finite (..))
import Data.Universe.Helpers (Tagged (..), Natural)

#if MIN_VERSION_base(4,7,0)
import Data.Type.Equality ((:~:) (..))
#else
import Data.GADT.Compare ((:=) (..))
#endif

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

class UniverseSome f where
  universeSome :: [Some f]

class UniverseSome f => FiniteSome f where
  universeFSome :: [Some f]
  universeFSome = universeSome

  cardinalitySome :: Tagged (Some f) Natural
  cardinalitySome = Tagged (genericLength (universeFSome :: [Some f]))

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

mkSome :: f a -> Some f
mkSome = This

-------------------------------------------------------------------------------
-- Instances for Some
-------------------------------------------------------------------------------

instance UniverseSome f => Universe (Some f) where
  universe = universeSome

instance FiniteSome f => Finite (Some f) where
  universeF   = universeFSome
  cardinality = cardinalitySome

-------------------------------------------------------------------------------
-- Type equality is singleton
-------------------------------------------------------------------------------

#if MIN_VERSION_base(4,7,0)
instance UniverseSome ((:~:) a) where
  universeSome = [mkSome Refl]

instance FiniteSome ((:~:) a) where
  cardinalitySome = 1
#else
instance UniverseSome ((:=) a) where
  universeSome = [mkSome Refl]

instance FiniteSome ((:=) a) where
  cardinalitySome = 1
#endif
