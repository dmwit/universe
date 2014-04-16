{-# LANGUAGE FlexibleContexts #-}
module Data.Universe.Instances.Trans (
	-- | Instances of 'Universe' and 'Finite' for the standard monad and functor transformers.
	Universe(..), Finite(..)
	) where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Trans.Identity
import Data.Functor.Compose
import Data.Functor.Product
import Data.Universe.Helpers
import Data.Universe.Instances.Base ()
import Data.Universe.Class

instance  Universe    a                    => Universe (Identity    a) where universe  = map Identity  universe
instance  Universe (f a)                   => Universe (IdentityT f a) where universe  = map IdentityT universe
instance (Finite e, Ord e, Universe (m a)) => Universe (ReaderT e m a) where universe  = map ReaderT   universe
instance  Universe (f (g a))               => Universe (Compose f g a) where universe  = map Compose   universe
instance (Universe (f a), Universe (g a))  => Universe (Product f g a) where universe  = [Pair f g | (f, g) <- universe +*+ universe]
instance  Finite       a                   => Finite   (Identity    a) where universeF = map Identity  universeF
instance  Finite    (f a)                  => Finite   (IdentityT f a) where universeF = map IdentityT universeF
instance (Finite e, Ord e, Finite   (m a)) => Finite   (ReaderT e m a) where universeF = map ReaderT   universeF
instance  Finite (f (g a))                 => Finite   (Compose f g a) where universeF = map Compose   universeF
instance (Finite (f a), Finite (g a))      => Finite   (Product f g a) where universeF = liftM2 Pair universeF universeF
