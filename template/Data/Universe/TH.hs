{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Universe.TH (deriveSomeUniverse) where

import Control.Monad
import Data.Foldable
import Data.Some
import Data.Universe.Class
import Language.Haskell.TH

-- | Given a type name, @N@, create an @instance Universe (Some N)@.  @N@ should represent a type of kind @k -> *@.
deriveSomeUniverse :: Name -> Q [Dec]
deriveSomeUniverse n =
  [d| instance Universe (Some $(conT n)) where
        universe = $(universeForData n)
    |]

universeForData :: Name -> Q Exp
universeForData n = reify n >>= \case
  TyConI (DataD _ _ _ _ cons _) -> (varE 'concat `appE`) $ fmap (ListE . concat) $ mapM universeForCon cons
  a -> fail $ "universeForData: Unmatched 'Info': " <> show a
  where
    -- Enumerate all values for the constructor by recursively invoking 'universe'
    --TODO: Should we use a traversal order other than what we get from the list
    --monad's 'join'?
    conWithArgs name numArgs = [| This <$> $(foldl' (\x y -> [| $x <*> $y |]) [| pure $(conE name) |] $ replicate numArgs [| universe |]) |]
    universeForCon = \case
      GadtC names bts _ -> forM names $ \name -> conWithArgs name $ length bts
      NormalC name bts -> fmap (:[]) $ conWithArgs name $ length bts
      ForallC _ _ con -> universeForCon con
      a -> fail $ "universeForData: Unmatched 'Dec': " <> show a
