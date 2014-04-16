{-# LANGUAGE NoImplicitPrelude #-}
module Data.Universe.Instances.Reverse (
	-- | A convenience module that imports the sibling modules @Eq@, @Ord@,
	-- @Show@, @Read@, and @Traversable@ to provide instances of these classes
	-- for functions over finite inputs.
	Eq(..), Ord(..), Show(..), Read(..), Foldable(..), Traversable(..)
	) where

import Data.Universe.Instances.Eq
import Data.Universe.Instances.Ord
import Data.Universe.Instances.Show
import Data.Universe.Instances.Read
import Data.Universe.Instances.Traversable
