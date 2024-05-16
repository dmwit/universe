{-# LANGUAGE Safe #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Data.Universe.Some.TH (
  DeriveUniverseSome (..),
  universeSomeQ,
  ) where

import Control.Monad (forM, mapM, unless)
import Data.Some (Some, mkSome)
import Data.Universe.Class (Universe (..))
import Data.Universe.Some (UniverseSome (..))
import Data.Universe.Helpers (interleave, (<+*+>))
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

-- $setup
-- >>> :m + Data.Some Data.Universe.Class Data.Universe.Some
-- >>> import Language.Haskell.TH (DecsQ)

-- | Derive the @'UniverseSome' n@ instance.
--
-- >>> :set -XGADTs -XTemplateHaskell -XStandaloneDeriving
-- >>> import Data.Universe.Class (universe)
-- >>> import Data.GADT.Show
--
-- >>> data Tag b a where IntTag :: Tag b Int; BoolTag :: b -> Tag b Bool
-- >>> deriving instance Show b => Show (Tag b a)
-- >>> instance Show b => GShow (Tag b) where gshowsPrec = showsPrec
--
-- (@data Unused@ is to workaround bug in older GHCi)
-- >>> data Unused; $(deriveUniverseSome ''Tag)
--
-- >>> universe :: [Some (Tag (Maybe Bool))]
-- [Some IntTag,Some (BoolTag Nothing),Some (BoolTag (Just False)),Some (BoolTag (Just True))]
--
-- 'deriveUniverseSome' variant taking a 'Name' guesses simple class constraints.
-- If you need more specific, you can specify them:
-- (Note: on older GHCs this will warn, as the instance definition doesn't have all methods defined).
--
-- >>> data Tag b a where IntTag :: Tag b Int; BoolTag :: b -> Tag b Bool
-- >>> deriving instance Show b => Show (Tag b a)
-- >>> instance Show b => GShow (Tag b) where gshowsPrec = showsPrec
-- >>> data Unused; $(deriveUniverseSome ([d| instance Universe b => UniverseSome (Tag b) |] :: DecsQ))
-- ...
-- >>> universe :: [Some (Tag (Maybe Bool))]
-- [Some IntTag,Some (BoolTag Nothing),Some (BoolTag (Just False)),Some (BoolTag (Just True))]
--
class DeriveUniverseSome a where
  deriveUniverseSome :: a -> DecsQ

instance DeriveUniverseSome a => DeriveUniverseSome [a] where
  deriveUniverseSome a = fmap concat (mapM deriveUniverseSome a)

instance DeriveUniverseSome a => DeriveUniverseSome (Q a) where
  deriveUniverseSome a = deriveUniverseSome =<< a

instance DeriveUniverseSome Name where
  deriveUniverseSome name = do
    di <- reifyDatatype name
    let DatatypeInfo { datatypeContext = ctxt
                     , datatypeName    = parentName
                     , datatypeInstTypes = vars0
                     , datatypeCons    = cons
                     } = di

    case safeUnsnoc vars0 of
      Nothing -> fail "Datatype should have at least one type variable"
      Just (vars, var) -> do
        varNames <- forM vars $ \v -> case v of
          SigT (VarT n) StarT -> newName "x"
          _                   -> fail "Only arguments of kind Type are supported"

        let constrs :: [TypeQ]
            constrs = map (\n -> conT ''Universe `appT` varT n) varNames
        let typ     = foldl (\c n -> c `appT` varT n) (conT parentName) varNames

        i <- instanceD (cxt constrs) (conT ''UniverseSome `appT` typ)
            [ instanceDecFor di
            ]

        return [i]

instanceDecFor :: DatatypeInfo -> Q Dec
instanceDecFor di = valD (varP 'universeSome) (normalB $ universeSomeQ' di) []

instance DeriveUniverseSome Dec where
  deriveUniverseSome (InstanceD overlaps c classHead []) = do
    let instanceFor = InstanceD overlaps c classHead
    case classHead of
      ConT u `AppT` t | u == ''UniverseSome -> do
        name <- headOfType t
        di <- reifyDatatype name
        i <- fmap instanceFor $ mapM id
            [ instanceDecFor di
            ]
        return [i]
      _ -> fail $ "deriveUniverseSome: expected an instance head like `UniverseSome (C a b ...)`, got " ++ show classHead
  deriveUniverseSome _ = fail "deriveUniverseSome: expected an empty instance declaration"

-- | Derive the method for @:: ['Some' tag]@
--
-- >>> :set -XGADTs -XTemplateHaskell -XStandaloneDeriving
-- >>> import Data.GADT.Show
--
-- >>> data Tag b a where IntTag :: Tag b Int; BoolTag :: b -> Tag b Bool
-- >>> deriving instance Show b => Show (Tag b a)
-- >>> instance Show b => GShow (Tag b) where gshowsPrec = showsPrec
--
-- >>> $(universeSomeQ ''Tag) :: [Some (Tag Bool)]
-- [Some IntTag,Some (BoolTag False),Some (BoolTag True)]
--
universeSomeQ :: Name -> ExpQ
universeSomeQ name = reifyDatatype name >>= universeSomeQ'

universeSomeQ' :: DatatypeInfo -> Q Exp
universeSomeQ' di = do
  let DatatypeInfo { datatypeContext = ctxt
                   , datatypeName    = parentName
                   , datatypeInstTypes = vars0
                   , datatypeCons    = cons
                   } = di

  -- check
  unless (null ctxt) $ fail "Datatype context is not empty"

  case safeUnsnoc vars0 of
    Nothing -> fail "Datatype should have at least one type variable"
    Just (vars, var) -> do
      let universe'   = [| universe |]
      let uap         = [| (<+*+>) |]
      let interleave' = [| interleave |]
      let mapSome'    = [| map mkSome |]

      let sums = map (universeForCon mapSome' universe' uap) cons
      interleave' `appE` listE sums
  where
    universeForCon mapSome' universe' uap ci =
      let con     = listE [ conE (constructorName ci) ]
          nargs   = length (constructorFields ci)
          conArgs = foldl (\f x -> infixE (Just f) uap (Just universe')) con (replicate nargs universe')

      in mapSome' `appE` conArgs

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

headOfType :: Type -> Q Name
headOfType (AppT t _) = headOfType t
headOfType (VarT n)   = return n
headOfType (ConT n)   = return n
headOfType t          = fail $ "headOfType: " ++ show t

safeUnsnoc :: [a] -> Maybe ([a], a)
safeUnsnoc xs = case reverse xs of
  []     -> Nothing
  (y:ys) -> Just (reverse ys, y)
