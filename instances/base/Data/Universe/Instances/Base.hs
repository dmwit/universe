{-# LANGUAGE TypeFamilies #-}
module Data.Universe.Instances.Base (Universe(..), Finite(..)) where

import Control.Monad
import Data.Int
import Data.Map ((!), fromList)
import Data.Monoid
import Data.Ratio
import Data.Universe.Class
import Data.Universe.Helpers
import Data.Word

instance Universe ()       where universe = universeDef
instance Universe Bool     where universe = universeDef
instance Universe Char     where universe = universeDef
instance Universe Ordering where universe = universeDef
instance Universe Integer  where universe = [0, -1..] +++ [1..]
instance Universe Int      where universe = universeDef
instance Universe Int8     where universe = universeDef
instance Universe Int16    where universe = universeDef
instance Universe Int32    where universe = universeDef
instance Universe Int64    where universe = universeDef
instance Universe Word     where universe = universeDef
instance Universe Word8    where universe = universeDef
instance Universe Word16   where universe = universeDef
instance Universe Word32   where universe = universeDef
instance Universe Word64   where universe = universeDef

instance (Universe a, Universe b) => Universe (Either a b) where universe = map Left universe +++ map Right universe
instance  Universe a              => Universe (Maybe  a  ) where universe = Nothing : map Just universe

instance (Universe a, Universe b) => Universe (a, b) where universe = universe +*+ universe
instance (Universe a, Universe b, Universe c) => Universe (a, b, c) where universe = [(a,b,c) | ((a,b),c) <- universe +*+ universe +*+ universe]
instance (Universe a, Universe b, Universe c, Universe d) => Universe (a, b, c, d) where universe = [(a,b,c,d) | (((a,b),c),d) <- universe +*+ universe +*+ universe +*+ universe]
instance (Universe a, Universe b, Universe c, Universe d, Universe e) => Universe (a, b, c, d, e) where universe = [(a,b,c,d,e) | ((((a,b),c),d),e) <- universe +*+ universe +*+ universe +*+ universe +*+ universe]

instance Universe a => Universe [a] where
	universe = diagonal $ [[]] : [[h:t | t <- universe] | h <- universe]

instance Universe All where universe = map All universe
instance Universe Any where universe = map Any universe
instance Universe a => Universe (Sum     a) where universe = map Sum     universe
instance Universe a => Universe (Product a) where universe = map Product universe
instance Universe a => Universe (Dual    a) where universe = map Dual    universe
instance Universe a => Universe (First   a) where universe = map First   universe
instance Universe a => Universe (Last    a) where universe = map Last    universe

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
positiveRationals :: [Ratio Integer]
positiveRationals = 1 : map lChild positiveRationals +++ map rChild positiveRationals where
	lChild frac = numerator frac % (numerator frac + denominator frac)
	rChild frac = (numerator frac + denominator frac) % denominator frac

instance a ~ Integer => Universe (Ratio a) where universe = 0 : map negate positiveRationals +++ positiveRationals

-- could change the Ord constraint to an Eq one, but come on, how many finite
-- types can't be ordered?
instance (Finite a, Ord a, Universe b) => Universe (a -> b) where
	universe = map tableToFunction tables where
		tables          = choices [universe | _ <- monoUniverse]
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
instance (Finite a, Finite b) => Finite (Either a b) where universeF = map Left universe ++ map Right universe

instance (Finite a, Finite b) => Finite (a, b) where universeF = liftM2 (,) universeF universeF
instance (Finite a, Finite b, Finite c) => Finite (a, b, c) where universeF = liftM3 (,,) universeF universeF universeF
instance (Finite a, Finite b, Finite c, Finite d) => Finite (a, b, c, d) where universeF = liftM4 (,,,) universeF universeF universeF universeF
instance (Finite a, Finite b, Finite c, Finite d, Finite e) => Finite (a, b, c, d, e) where universeF = liftM5 (,,,,) universeF universeF universeF universeF universeF

instance Finite All where universeF = map All universeF
instance Finite Any where universeF = map Any universeF
instance Finite a => Finite (Sum     a) where universeF = map Sum     universeF
instance Finite a => Finite (Product a) where universeF = map Product universeF
instance Finite a => Finite (Dual    a) where universeF = map Dual    universeF
instance Finite a => Finite (First   a) where universeF = map First   universeF
instance Finite a => Finite (Last    a) where universeF = map Last    universeF

instance (Ord a, Finite a, Finite b) => Finite (a -> b) where
	universeF = map tableToFunction tables where
		tables          = sequence [universeF | _ <- monoUniverse]
		tableToFunction = (!) . fromList . zip monoUniverse
		monoUniverse    = universeF

-- to add when somebody asks for it: instance (Eq a, Finite a) => Finite (Endo a) (+Universe)
