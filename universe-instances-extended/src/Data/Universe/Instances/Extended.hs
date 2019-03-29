{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Universe.Instances.Extended (
  -- | Instances for 'Universe' and 'Finite' for function-like functors and the empty type.
  Universe(..), Finite(..)
  ) where

import Control.Comonad.Trans.Traced (TracedT (..))
import Data.Functor.Rep (Representable (..), Co(..))
import Data.Universe.Class (Universe (..), Finite (..))
import Data.Universe.Helpers (retag, Tagged, Natural)

-- | We could do this:
--
-- instance Universe (f a) => Universe (Co f a) where universe = map Rep universe
--
-- However, since you probably only apply Rep to functors when you want to
-- think of them as being representable, I think it makes sense to use an
-- instance based on the representable-ness rather than the inherent
-- universe-ness.
--
-- Please complain if you disagree!
--
instance (Representable f, Finite (Rep f), Ord (Rep f), Universe a)
  => Universe (Co f a)
  where universe = map tabulate universe
instance (Representable f, Finite s, Ord s, Finite (Rep f), Ord (Rep f), Universe a)
  => Universe (TracedT s f a)
  where universe = map tabulate universe

instance (Representable f, Finite (Rep f), Ord (Rep f), Finite a)
  => Finite (Co f a)
  where universeF = map tabulate universeF; cardinality = retag (cardinality :: Tagged (Rep (Co        f) -> a) Natural)
instance (Representable f, Finite s, Ord s, Finite (Rep f), Ord (Rep f), Finite a)
  => Finite (TracedT s f a)
  where universeF = map tabulate universeF; cardinality = retag (cardinality :: Tagged (Rep (TracedT s f)) Natural)
