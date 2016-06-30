{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Universe.Instances.Base (
    -- | Instances of 'Universe' and 'Finite' for built-in types.
    Universe(..), Finite(..)
    ) where

import Control.Monad
import Data.Int
import Data.Map ((!), fromList)
import Data.Monoid
import Data.Ratio
import Data.Universe.Class
import Data.Universe.Helpers
import Data.Word

instance Universe ()       where universeUniv = universeDef
instance Universe Bool     where universeUniv = universeDef
instance Universe Char     where universeUniv = universeDef
instance Universe Ordering where universeUniv = universeDef
instance Universe Integer  where universeUniv = univFromList [0, -1..] +++ univFromList [1..]
instance Universe Int      where universeUniv = universeDef
instance Universe Int8     where universeUniv = universeDef
instance Universe Int16    where universeUniv = universeDef
instance Universe Int32    where universeUniv = universeDef
instance Universe Int64    where universeUniv = universeDef
instance Universe Word     where universeUniv = universeDef
instance Universe Word8    where universeUniv = universeDef
instance Universe Word16   where universeUniv = universeDef
instance Universe Word32   where universeUniv = universeDef
instance Universe Word64   where universeUniv = universeDef

instance (Universe a, Universe b) => Universe (Either a b) where universeUniv = fmap Left universeUniv +++ fmap Right universeUniv
instance  Universe a              => Universe (Maybe  a  ) where universeUniv = univCons Nothing $ fmap Just universeUniv

instance (Universe a, Universe b) => Universe (a, b) where universeUniv = universeUniv +*+ universeUniv
instance (Universe a, Universe b, Universe c) => Universe (a, b, c) where
    universeUniv = fmap mk $ universeUniv +*+ universeUniv +*+ universeUniv where
        mk ((a,b),c) = (a,b,c)
instance (Universe a, Universe b, Universe c, Universe d) => Universe (a, b, c, d) where
    universeUniv = fmap mk $ universeUniv +*+ universeUniv +*+ universeUniv +*+ universeUniv where
        mk (((a,b),c),d) = (a,b,c,d)
instance (Universe a, Universe b, Universe c, Universe d, Universe e) => Universe (a, b, c, d, e) where
    universeUniv = fmap mk $ universeUniv +*+ universeUniv +*+ universeUniv +*+ universeUniv +*+ universeUniv where
        mk ((((a,b),c),d),e) = (a,b,c,d,e)

-- | TODO: `diagonal` to `join`
instance Universe a => Universe [a] where
    universeUniv = univCons [] ((:) <$> universeUniv <*> universeUniv)

instance Universe All where universeUniv = fmap All universeUniv
instance Universe Any where universeUniv = fmap Any universeUniv
instance Universe a => Universe (Sum     a) where universeUniv = fmap Sum     universeUniv
instance Universe a => Universe (Product a) where universeUniv = fmap Product universeUniv
instance Universe a => Universe (Dual    a) where universeUniv = fmap Dual    universeUniv
instance Universe a => Universe (First   a) where universeUniv = fmap First   universeUniv
instance Universe a => Universe (Last    a) where universeUniv = fmap Last    universeUniv

-- see http://mathlesstraveled.com/2008/01/07/recounting-the-rationals-part-ii-fractions-grow-on-trees/
--
-- also, Brent Yorgey writes:
--
-- positiveRationals2 :: [Ratio Integer]
-- positiveRationals2 = iterate' next 1
--   where
--     next x = let (n,y) = properFraction x in recip (fromInteger n + 1 - y)
--     iterate' f x = let x' = f x in x' `seq` (x : iterate' f x')
--
-- Compiling this code with -O2 and doing some informal tests seems to
-- show that positiveRationals and positiveRationals2 have almost exactly
-- the same efficiency for generating the entire list (e.g. the times for
-- finding the sum of the first 100000 rationals are pretty much
-- indistinguishable).  positiveRationals is still the clear winner for
-- generating just the nth rational for some particular n -- some simple
-- experiments seem to indicate that doing this with positiveRationals2
-- scales linearly while with positiveRationals it scales sub-linearly,
-- as expected.
--
-- Surprisingly, replacing % with :% in positiveRationals seems to make
-- no appreciable difference.
positiveRationals :: Univ (Ratio Integer)
positiveRationals = univCons 1 $ fmap lChild positiveRationals +++ fmap rChild positiveRationals where
    lChild frac = numerator frac % (numerator frac + denominator frac)
    rChild frac = (numerator frac + denominator frac) % denominator frac

instance a ~ Integer => Universe (Ratio a) where universeUniv = univCons 0 $ fmap negate positiveRationals +++ positiveRationals

-- could change the Ord constraint to an Eq one, but come on, how many finite
-- types can't be ordered?
instance (Finite a, Ord a, Universe b) => Universe (a -> b) where
    universeUniv = fmap tableToFunction tables where
        tables          = choices [universeUniv | _ <- monoUniverse]
        tableToFunction = (!) . fromList . zip monoUniverse
        monoUniverse    = universeF

instance Finite ()
instance Finite Bool
instance Finite Char
instance Finite Ordering
instance Finite Int
instance Finite Int8
instance Finite Int16
instance Finite Int32
instance Finite Int64
instance Finite Word
instance Finite Word8
instance Finite Word16
instance Finite Word32
instance Finite Word64

instance  Finite a            => Finite (Maybe  a  )
instance (Finite a, Finite b) => Finite (Either a b)

instance (Finite a, Finite b) => Finite (a, b) where universeUnivF = liftM2 (,) universeUnivF universeUnivF
instance (Finite a, Finite b, Finite c) => Finite (a, b, c) where universeUnivF = liftM3 (,,) universeUnivF universeUnivF universeUnivF
instance (Finite a, Finite b, Finite c, Finite d) => Finite (a, b, c, d) where universeUnivF = liftM4 (,,,) universeUnivF universeUnivF universeUnivF universeUnivF
instance (Finite a, Finite b, Finite c, Finite d, Finite e) => Finite (a, b, c, d, e) where universeUnivF = liftM5 (,,,,) universeUnivF universeUnivF universeUnivF universeUnivF universeUnivF

instance Finite All where universeUnivF = fmap All universeUnivF
instance Finite Any where universeUnivF = fmap Any universeUnivF
instance Finite a => Finite (Sum     a) where universeUnivF = fmap Sum     universeUnivF
instance Finite a => Finite (Product a) where universeUnivF = fmap Product universeUnivF
instance Finite a => Finite (Dual    a) where universeUnivF = fmap Dual    universeUnivF
instance Finite a => Finite (First   a) where universeUnivF = fmap First   universeUnivF
instance Finite a => Finite (Last    a) where universeUnivF = fmap Last    universeUnivF

instance (Ord a, Finite a, Finite b) => Finite (a -> b) where
    universeUnivF = fmap tableToFunction tables where
        tables          = choices [universeUniv | _ <- monoUniverse]
        tableToFunction = (!) . fromList . zip monoUniverse
        monoUniverse    = universeF

-- to add when somebody asks for it: instance (Eq a, Finite a) => Finite (Endo a) (+Universe)
