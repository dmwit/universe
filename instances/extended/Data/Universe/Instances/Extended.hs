{-# LANGUAGE FlexibleContexts #-}
module Data.Universe.Instances.Extended where

import Data.Void
import Control.Comonad.Trans.Traced
import Data.Functor.Rep
import Data.Universe.Instances.Base ()
import Data.Universe.Class

instance Universe Void where universe = []

-- We could do this:
--
-- instance Universe (f a) => Universe (Co f a) where universe = map Rep universe
--
-- However, since you probably only apply Rep to functors when you want to
-- think of them as being representable, I think it makes sense to use an
-- instance based on the representable-ness rather than the inherent
-- universe-ness.
--
-- Please complain if you disagree!
instance (Representable f, Finite (Rep f), Ord (Rep f), Universe a)
	=> Universe (Co f a)
	where universe = map tabulate universe
instance (Representable f, Finite s, Ord s, Finite (Rep f), Ord (Rep f), Universe a)
	=> Universe (TracedT s f a)
	where universe = map tabulate universe

instance Finite Void

instance (Representable f, Finite (Rep f), Ord (Rep f), Finite a)
	=> Finite (Co f a)
	where universeF = map tabulate universeF
instance (Representable f, Finite s, Ord s, Finite (Rep f), Ord (Rep f), Finite a)
	=> Finite (TracedT s f a)
	where universeF = map tabulate universeF
