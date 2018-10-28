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
import Data.Monoid
import Data.Some
import Data.Universe.Class
import Language.Haskell.TH
import Language.Haskell.TH.Extras

class DeriveSomeUniverse a where
  -- | Given a type name, @N@, create an @instance Universe (Some N)@.  @N@ should represent a type of kind @k -> *@.  Or, instead of a name, an instance declaration with an appropriate head may be given.
  deriveSomeUniverse :: a -> Q [Dec]

instance DeriveSomeUniverse Name where
  deriveSomeUniverse n = deriveSomeUniverse
    [d| instance Universe (Some $(conT n)) |]

instance DeriveSomeUniverse a => DeriveSomeUniverse [a] where
  deriveSomeUniverse a = concat <$> traverse deriveSomeUniverse a

instance DeriveSomeUniverse a => DeriveSomeUniverse (Q a) where
  deriveSomeUniverse a = deriveSomeUniverse =<< a

instance DeriveSomeUniverse Dec where
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
  deriveSomeUniverse (InstanceD overlaps c classHead []) = do
    let instanceFor = InstanceD overlaps c classHead
#else
  deriveSomeUniverse (InstanceD c classHead []) = do
    let instanceFor = InstanceD c classHead
#endif
    case classHead of
      u `AppT` (s `AppT` t)
        | u == ConT ''Universe
        , s == ConT ''Some
          -> do
            universeExp <- universeForData $ headOfType t
            return [instanceFor [ValD (VarP 'universe) (NormalB universeExp) []]]
      _ -> fail $ "deriveSomeUniverse: expected an instance head like `Universe (Some (C a b ...))`, got " ++ show classHead
  deriveSomeUniverse _ = fail "deriveSomeUniverse: expected an empty instance declaration"

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
