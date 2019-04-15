{-# LANGUAGE NoImplicitPrelude #-}
-- | A convenience module that imports the sibling modules @Eq@, @Ord@,
-- @Show@, @Read@, and @Traversable@ to provide instances of these classes
-- for functions over finite inputs.
module Data.Universe.Instances.Reverse (
  module Data.Universe.Instances.Eq,
  module Data.Universe.Instances.Ord,
  module Data.Universe.Instances.Show,
  module Data.Universe.Instances.Read,
  module Data.Universe.Instances.Traversable,
  ) where

import Data.Universe.Instances.Eq
import Data.Universe.Instances.Ord
import Data.Universe.Instances.Show
import Data.Universe.Instances.Read
import Data.Universe.Instances.Traversable
