{-# LANGUAGE NoImplicitPrelude #-}
module Data.Universe (
	-- | A convenience module that imports the submodules @Instances.Base@,
	-- @Instances.Extended@, and @Instances.Trans@ to provide instances of
	-- 'Universe' and 'Finite' for a wide variety of types.
	Universe(..), Finite(..)
	) where

import Data.Universe.Instances.Base
import Data.Universe.Instances.Extended
import Data.Universe.Instances.Trans
