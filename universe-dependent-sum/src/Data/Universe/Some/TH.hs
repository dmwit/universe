{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes #-}
#else
{-# LANGUAGE TemplateHaskell #-}
#endif
module Data.Universe.Some.TH (
  DeriveUniverseSome (..),
  universeSomeQ,
  ) where

import Control.Monad (forM, mapM, unless)
import Data.Some (Some (..))
import Data.Universe.Class (Universe (..))
import Data.Universe.Some (UniverseSome (..))
import Data.Universe.Helpers (interleave, (<+*+>))
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

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
-- >>> ; deriveUniverseSome ''Tag
-- >>> universe :: [Some (Tag (Maybe Bool))]
-- [Some IntTag,Some (BoolTag Nothing),Some (BoolTag (Just False)),Some (BoolTag (Just True))]
--
-- 'deriveUniverseSome' variant taking a 'Name' guesses simple class constraints.
-- If you need more specific, you can specify them:
--
-- >>> ; deriveUniverseSome [d| instance Universe b => UniverseSome (Tag b) |]
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
#if MIN_VERSION_th_abstraction(0,3,0)
                     , datatypeInstTypes = vars0
#else
                     , datatypeVars    = vars0
#endif
                     , datatypeCons    = cons
                     } = di

    case safeUnsnoc vars0 of
      Nothing -> fail "Datatype should have at least one type variable"
      Just (vars, var) -> do
        varNames <- forM vars $ \v -> case v of
#if MIN_VERSION_template_haskell(2,8,0)
          SigT (VarT n) StarT -> newName "x"
#else
          SigT (VarT n) StarK -> newName "x"
#endif
          _                   -> fail "Only arguments of kind Type are supported"

#if MIN_VERSION_template_haskell(2,10,0)
        let constrs :: [TypeQ]
            constrs = map (\n -> conT ''Universe `appT` varT n) varNames
#else
        let constrs :: [PredQ]
            constrs = map (\n -> classP ''Universe [varT n]) varNames
#endif
        let typ     = foldl (\c n -> c `appT` varT n) (conT parentName) varNames

        i <- instanceD (cxt constrs) (conT ''UniverseSome `appT` typ)
            [ instanceDecFor di
            ]

        return [i]

instanceDecFor :: DatatypeInfo -> Q Dec
instanceDecFor di = valD (varP 'universeSome) (normalB $ universeSomeQ' di) []

instance DeriveUniverseSome Dec where
#if MIN_VERSION_template_haskell(2,11,0)
  deriveUniverseSome (InstanceD overlaps c classHead []) = do
    let instanceFor = InstanceD overlaps c classHead
#else
  deriveUniverseSome (InstanceD c classHead []) = do
    let instanceFor = InstanceD c classHead
#endif
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
#if MIN_VERSION_th_abstraction(0,3,0)
                   , datatypeInstTypes = vars0
#else
                   , datatypeVars    = vars0
#endif
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
#if MIN_VERSION_dependent_sum(0,5,0)
      let mapSome'    = [| map Some |]
#else
      let mapSome'    = [| map This |]
#endif

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
