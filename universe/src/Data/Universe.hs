{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | A convenience module that imports the submodules @Instances.Base@, @Instances.Containers@,
-- @Instances.Extended@, and @Instances.Trans@ to provide instances of
-- 'Universe' and 'Finite' for a wide variety of types.
module Data.Universe (
  Universe(..), Finite(..),
#if __GLASGOW_HASKELL__ >= 702
  universeGeneric,
#endif
  module Data.Universe.Instances.Extended,
  ) where

import Data.Universe.Class
import Data.Universe.Instances.Extended
import Data.Universe.Some ()

#if __GLASGOW_HASKELL__ >= 702
import Data.Universe.Generic (universeGeneric)
#endif
