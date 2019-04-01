{-# LANGUAGE TemplateHaskell #-}
module Data.Universe.Some.TH where

import Control.Monad (unless)
import Data.Universe.Some
import Language.Haskell.TH.Datatype
import Language.Haskell.TH
import Control.Monad.IO.Class (liftIO)
import Data.Some (Some (..))

-- | Derive the method for @:: ['Some' tag]@
--
-- >>> :set -XGADTs -XTemplateHaskell -XStandaloneDeriving
-- >>> import Data.GADT.Show
--
-- >>> data Tag b a where IntTag :: Tag b Int; BoolTag :: Bool -> Tag b Bool
-- >>> deriving instance Show (Tag b a)
-- >>> instance GShow (Tag b) where gshowsPrec = showsPrec
--
-- >>> $(universeSomeQ ''Tag)
-- [This IntTag,This (BoolTag False),This (BoolTag True)]
--
universeSomeQ :: Name -> Q Exp
universeSomeQ name = do
  DatatypeInfo { datatypeContext = ctxt
               , datatypeName    = parentName
               , datatypeVars    = vars
               , datatypeVariant = variant
               , datatypeCons    = cons
               } <- reifyDatatype name

  -- check
  unless (null ctxt) $ fail "Datatype context is not empty"

  case safeUnsnoc vars of
    Nothing -> fail "Datatype should have at least one type variable"
    Just (vars, var) -> do
      Just universe' <- lookupValueName "Data.Universe.Class.universe"
      Just uap <- lookupValueName "Data.Universe.Helpers.<+*+>"
      Just interleave' <- lookupValueName "Data.Universe.Helpers.interleave"

      let sums = map (universeForCon (varE universe') (varE uap)) cons
      varE interleave' `appE` listE sums
  where
    universeForCon universe' uap ci =
      let con     = listE [ conE (constructorName ci) ]
          nargs   = length (constructorFields ci)
          conArgs = foldl (\f x -> infixE (Just f) uap (Just universe')) con (replicate nargs universe')

      in [| map This |] `appE` conArgs

safeUnsnoc :: [a] -> Maybe ([a], a)
safeUnsnoc xs = case reverse xs of
  []     -> Nothing
  (y:ys) -> Just (reverse ys, y)
