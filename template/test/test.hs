{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
import Data.Universe.TH

import Data.Some
import Data.GADT.Show.TH
import Data.Universe.Class
import Data.Universe.Instances.Base ()

data TestGadt a where
  A, B :: TestGadt Int
  C :: Bool -> TestGadt Int
  D :: forall b. TestGadt b
  E :: Bool -> Either Bool () -> TestGadt a
  F :: Some TestGadt2 -> TestGadt a

data TestGadt2 a where
  T2I :: TestGadt2 Int
  T2S :: TestGadt2 String

deriveGShow ''TestGadt
deriveGShow ''TestGadt2

deriveSomeUniverse ''TestGadt
deriveSomeUniverse ''TestGadt2

main :: IO ()
main = print (universe :: [Some TestGadt])
