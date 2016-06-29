{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import Data.Universe.Instances.Base

instance  Universe    a                    => Universe (Identity    a) where universeUniv  = fmap Identity  universeUniv
instance  Universe (f a)                   => Universe (IdentityT f a) where universeUniv  = fmap IdentityT universeUniv
instance (Finite e, Ord e, Universe (m a)) => Universe (ReaderT e m a) where universeUniv  = fmap ReaderT   universeUniv
instance  Universe (f (g a))               => Universe (Compose f g a) where universeUniv  = fmap Compose   universeUniv
instance (Universe (f a), Universe (g a))  => Universe (Product f g a) where universeUniv  = fmap (uncurry Pair) $ universeUniv +*+ universeUniv
instance  Finite    a                      => Finite (Identity    a)   where universeUnivF = fmap Identity  universeUnivF
instance  Finite (f a)                     => Finite (IdentityT f a)   where universeUnivF = fmap IdentityT universeUnivF
instance (Finite e, Ord e, Finite (m a)  ) => Finite (ReaderT e m a)   where universeUnivF = fmap ReaderT   universeUnivF
instance  Finite (f (g a))                 => Finite (Compose f g a)   where universeUnivF = fmap Compose   universeUnivF
instance (Finite (f a), Finite (g a))      => Finite (Product f g a)   where universeUnivF = fmap (uncurry Pair) $ universeUnivF +*+ universeUnivF
