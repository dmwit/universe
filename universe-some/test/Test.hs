{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Data.Universe.Class (Universe (..), Finite (..))
import Data.Some (Some (..))
import Data.GADT.Show
import Data.Universe.Some (UniverseSome (..))
import Data.Universe.Some.TH

-------------------------------------------------------------------------------
-- Name
-------------------------------------------------------------------------------

data Tag b a where
  IntTag  :: Tag b Int
  BoolTag :: b -> Tag b Bool

deriving instance Show b => Show (Tag b a)
instance Show b => GShow (Tag b) where gshowsPrec = showsPrec

deriveUniverseSome ''Tag


-------------------------------------------------------------------------------
-- Dec
-------------------------------------------------------------------------------

data Tag2 b a where
  IntTag2  :: Tag2 b Int
  BoolTag2 :: b -> Tag2 b Bool

deriving instance Show b => Show (Tag2 b a)
instance Show b => GShow (Tag2 b) where gshowsPrec = showsPrec

deriveUniverseSome [d| instance Universe b => UniverseSome (Tag2 b) |]

-------------------------------------------------------------------------------
-- Manual
-------------------------------------------------------------------------------

data Tag3 b a where
  IntTag3  :: Tag3 b Int
  BoolTag3 :: b -> Tag3 b Bool

deriving instance Show b => Show (Tag3 b a)
instance Show b => GShow (Tag3 b) where gshowsPrec = showsPrec

-- to separate splices
$(return [])

instance Universe b => UniverseSome (Tag3 b) where
    universeSome = $(universeSomeQ ''Tag3)

-------------------------------------------------------------------------------
-- Issue 53
-------------------------------------------------------------------------------

data Foo a where
  MkFoo :: Foo Int
data Bar a where
  MkBar :: Bool -> Foo a -> Bar a

deriving instance Show (Foo a)
deriving instance Show (Bar a)
instance GShow Foo where gshowsPrec = showsPrec
instance GShow Bar where gshowsPrec = showsPrec

deriveFiniteSome ''Foo
deriveFiniteSome ''Bar

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  print (universe :: [Some (Tag (Maybe Bool)) ])
  print (universe :: [Some (Tag2 (Maybe Bool)) ])
  print (universe :: [Some (Tag3 (Maybe Bool)) ])

  print (universeF :: [Some Foo])
  print (universeF :: [Some Bar])
