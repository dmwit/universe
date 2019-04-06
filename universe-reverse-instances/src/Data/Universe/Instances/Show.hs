-- | A 'Show' instance for functions, given the input is 'Finite' and both
-- the input and output are 'Show'.
module Data.Universe.Instances.Show () where

import Data.Universe.Class (Universe (..), Finite (..))
import qualified Data.Monoid as Mon

instance (Finite a, Show a, Show b) => Show (a -> b) where
  showsPrec n f = showsPrec n [(a, f a) | a <- universeF]

instance (Finite a, Show a) => Show (Mon.Endo a) where
  showsPrec d (Mon.Endo f) = showParen (d > 10)
    $ showString "Endo "
    . showsPrec 11 f
