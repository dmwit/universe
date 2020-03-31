{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes #-}
#else
{-# LANGUAGE TemplateHaskell #-}
#endif
module Data.Universe.Some.TH (
  DeriveUniverseSome (..),
  universeSomeQ,
  universeFSomeQ,
  ) where

import Control.Monad (forM, mapM, unless)
import Data.Some (Some, mkSome, withSomeM)
import Data.Traversable (for)
import Data.Universe.Class (Universe (..), Finite (..))
import Data.Universe.Some (UniverseSome (..), FiniteSome (..))
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

  -- | 'deriveFiniteSome' derives 'FiniteSome' instance with slightly different
  -- code, without interleaving of different branches.  This allows to handle
  -- more cases.
  --
  -- It also defines a trivial 'universeSome = universeFSome' 'UniverseSome' instance.
  --
  deriveFiniteSome   :: a -> DecsQ

instance DeriveUniverseSome a => DeriveUniverseSome [a] where
  deriveUniverseSome a = fmap concat (mapM deriveUniverseSome a)
  deriveFiniteSome   a = fmap concat (mapM deriveFiniteSome a)

instance DeriveUniverseSome a => DeriveUniverseSome (Q a) where
  deriveUniverseSome a = deriveUniverseSome =<< a
  deriveFiniteSome   a = deriveFiniteSome   =<< a

instance DeriveUniverseSome Name where
  deriveUniverseSome = deriveUniverseSomeName ClsUniverse
  deriveFiniteSome   = deriveUniverseSomeName ClsFinite

deriveUniverseSomeName :: Cls -> Name -> Q [Dec]
deriveUniverseSomeName cls name = do
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
            constrs = map (\n -> conT clsName `appT` varT n) varNames
#else
        let constrs :: [PredQ]
            constrs = map (\n -> classP clsName [varT n]) varNames
#endif
        let typ     = foldl (\c n -> c `appT` varT n) (conT parentName) varNames

        case cls of
          ClsUniverse -> do
            i <- instanceD (cxt constrs) (conT clsSomeName `appT` typ)
                [ instanceDecForU di
                ]
            return [i]
          ClsFinite -> do
            i <- instanceD (cxt constrs) (conT clsSomeName `appT` typ)
                [ instanceDecForF di
                ]
            j <- instanceD (cxt constrs) (conT ''UniverseSome `appT` typ)
                [ instanceDecForUviaF
                ]
            return [i, j]

  where
    clsName = case cls of
      ClsUniverse -> ''Universe
      ClsFinite   -> ''Finite

    clsSomeName = case cls of
      ClsUniverse -> ''UniverseSome
      ClsFinite   -> ''FiniteSome

instanceDecForU :: DatatypeInfo -> Q Dec
instanceDecForU di = valD (varP 'universeSome) (normalB $ universeSomeQ' di) []

instanceDecForF :: DatatypeInfo -> Q Dec
instanceDecForF di = valD (varP 'universeFSome) (normalB $ universeFSomeQ' di) []

instanceDecForUviaF :: Q Dec
instanceDecForUviaF = valD (varP 'universeSome) (normalB $ varE 'universeFSome) []

instance DeriveUniverseSome Dec where
  deriveUniverseSome = deriveUniverseSomeDec ClsUniverse
  deriveFiniteSome   = deriveUniverseSomeDec ClsFinite

deriveUniverseSomeDec :: Cls -> Dec -> Q [Dec]
#if MIN_VERSION_template_haskell(2,11,0)
deriveUniverseSomeDec cls (InstanceD overlaps c classHead []) = do
    let instanceFor  = fmap (InstanceD overlaps c classHead) . sequence
    let instanceForU = fmap (InstanceD overlaps c (overHeadOfType (const ''UniverseSome) classHead)) . sequence
#else
deriveUniverseSomeDec cls (InstanceD c classHead []) = do
    let instanceFor  = fmap (InstanceD c classHead) . sequence
    let instanceForU = fmap (InstanceD c (overHeadOfType (const ''UniverseSome) classHead)) . sequence
#endif
    case classHead of
      ConT u `AppT` t
        | cls == ClsUniverse
        , u == ''UniverseSome -> do
          name <- headOfType t
          di <- reifyDatatype name
          i <- instanceFor [ instanceDecForU di ]
          return [i]
        | cls == ClsFinite
        , u == ''FiniteSome -> do
          name <- headOfType t
          di <- reifyDatatype name
          i <- instanceFor [ instanceDecForF di ]
          j <- instanceForU [ instanceDecForUviaF ]
          return [i, j]
      _ -> fail $ "deriveUniverseSome/deriveFiniteSome: expected an instance head like `UniverseSome (C a b ...)`, got " ++ show classHead
deriveUniverseSomeDec _ _ = fail "deriveUniverseSome/deriveFiniteSome: expected an empty instance declaration"

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

-- | Like 'universeSomeQ' but derives expression for 'universeF'.
--
-- Uses the fact that subterms should be finite,
-- thus allowing to derive more instances.
--
-- >>> :set -XGADTs -XTemplateHaskell -XStandaloneDeriving
-- >>> import Data.GADT.Show
--
-- >>> data Tag b a where IntTag :: Tag b Int; BoolTag :: b -> Tag b Bool
-- >>> deriving instance Show b => Show (Tag b a)
-- >>> instance Show b => GShow (Tag b) where gshowsPrec = showsPrec
-- >>> $(deriveFiniteSome ''Tag);
--
-- >>> universeFSome :: [Some (Tag Bool)]
-- [Some IntTag,Some (BoolTag False),Some (BoolTag True)]
--
-- >>> data Wrap a where Once :: Tag Bool a -> Wrap a; Empty :: Wrap ()
-- >>> deriving instance Show (Wrap a)
-- >>> instance GShow Wrap where gshowsPrec = showsPrec
--
-- >>> $(universeFSomeQ ''Wrap) :: [Some Wrap]
-- [Some (Once IntTag),Some (Once (BoolTag False)),Some (Once (BoolTag True)),Some Empty]
--
universeFSomeQ :: Name -> ExpQ
universeFSomeQ name = reifyDatatype name >>= universeFSomeQ'

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
      let sums = map universeForCon cons
      interleave' `appE` listE sums
  where
    universeForCon ci = do
      let con     = listE [ conE (constructorName ci) ]
          nargs   = length (constructorFields ci)
          conArgs = foldl (\f x -> infixE (Just f) uap (Just universe')) con (replicate nargs universe')
      mapSome' `appE` conArgs

    universe'   = [| universe |]
    uap         = [| (<+*+>) |]
    interleave' = [| interleave |]
    mapSome'    = [| map mkSome |]

universeFSomeQ' :: DatatypeInfo -> Q Exp
universeFSomeQ' di = do
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
      let sums = map universeForCon cons
      [| concat |] `appE` listE sums
  where
    universeForCon ci = do
      let con     = conE (constructorName ci)
      args <- forM (zip [1..] (constructorFields ci)) $ \(j, f) -> do
        name <- newName ("x" ++ show (j :: Int))
        let defStep kont = infixE (Just universeF') [| (>>=) |] (Just (lamE [varP name] kont))
        return $ (,) (varE name) $ \kont -> case f of
          AppT c _ -> do
#if MIN_VERSION_template_haskell(2,6,0)
            isInst <- isInstance ''FiniteSome [c]
#else
            isInst <- isClassInstance ''FiniteSome [c]
#endif
            if isInst
            then [| withSomeM |] `appE` [| universeFSome |] `appE` (lamE [varP name] kont)
            else defStep kont
          _ -> defStep kont

      foldl (\acc (_, kont) -> kont acc)
            (listE [([| mkSome |] `appE` (foldl appE con (map fst args)))])
            args

    universeF'     = [| universeF |]
    universeFSome' = [| universeFSome |]
    mapSome'       = [| map mkSome |]

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

headOfType :: Type -> Q Name
headOfType (AppT t _) = headOfType t
headOfType (VarT n)   = return n
headOfType (ConT n)   = return n
headOfType t          = fail $ "headOfType: " ++ show t

overHeadOfType :: (Name -> Name) -> Type -> Type
overHeadOfType f (AppT x y) = AppT (overHeadOfType f x) y
overHeadOfType f (VarT n)   = VarT (f n)
overHeadOfType f (ConT n)   = ConT (f n)
overHeadOfType f t          = t

safeUnsnoc :: [a] -> Maybe ([a], a)
safeUnsnoc xs = case reverse xs of
  []     -> Nothing
  (y:ys) -> Just (reverse ys, y)

data Cls = ClsUniverse | ClsFinite deriving (Eq)
