{-# LANGUAGE NoImplicitPrelude #-}
-- | A convenience module that imports the submodules @Instances.Base@, @Instances.Containers@,
-- @Instances.Extended@, and @Instances.Trans@ to provide instances of
-- 'Universe' and 'Finite' for a wide variety of types.
module Data.Universe (
  Universe(..), Finite(..),
  module Data.Universe.Instances.Extended,
  ) where

import Data.Universe.Class
import Data.Universe.Instances.Extended
