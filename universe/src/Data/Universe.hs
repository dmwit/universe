{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe #-}
-- | A convenience module that imports the submodules @Instances.Base@, @Instances.Containers@,
-- @Instances.Extended@, and @Instances.Trans@ to provide instances of
-- 'Universe' and 'Finite' for a wide variety of types.
module Data.Universe (
  Universe(..), Finite(..),
  universeGeneric,
  module Data.Universe.Instances.Extended,
  ) where

import Data.Universe.Class
import Data.Universe.Instances.Extended
import Data.Universe.Some ()
import Data.Universe.Generic (universeGeneric)
