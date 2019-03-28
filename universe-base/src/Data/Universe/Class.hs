{-# LANGUAGE CPP, TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
#ifdef DEFAULT_SIGNATURES
{-# LANGUAGE DefaultSignatures #-}
#endif
-- | Bottoms are ignored for this entire module:
-- only fully-defined inhabitants are considered inhabitants.
module Data.Universe.Class
  ( Universe(..)
  , Finite(..)
  ) where

import Data.Universe.Helpers

import Control.Monad (liftM2, liftM3, liftM4, liftM5)
import Control.Monad.Trans.Identity (IdentityT (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product (Product (..))
import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.List (genericLength)
import Data.Map ((!), fromList)
import Data.Ratio (Ratio, numerator, denominator, (%))
import Data.Void (Void)
import Data.Word  (Word, Word8, Word16, Word32, Word64)

import qualified Data.Monoid as Mon
import qualified Data.Set as Set
-- import qualified Data.Map as Map

-- | Creating an instance of this class is a declaration that your type is
-- recursively enumerable (and that 'universe' is that enumeration). In
-- particular, you promise that any finite inhabitant has a finite index in
-- 'universe', and that no inhabitant appears at two different finite indices.
--
-- Well-behaved instance should produce elements lazily.
--
-- /Laws:/
--
-- @
-- 'elem' x 'universe'                    -- any inhabitant has a finite index
-- let pfx = 'take' n 'universe'          -- any finite prefix of universe has unique elements
-- in 'length' pfx = 'length' (nub pfx)
-- @
class Universe a where
  universe :: [a]
#ifdef DEFAULT_SIGNATURES
  default universe :: (Enum a, Bounded a) => [a]
  universe = universeDef
#endif

-- | Creating an instance of this class is a declaration that your 'universe'
-- eventually ends. Minimal definition: no methods defined. By default,
-- @universeF = universe@, but for some types (like 'Either') the 'universeF'
-- method may have a more intuitive ordering.
--
-- /Laws:/
--
-- @
-- 'elem' x 'universeF'                       -- any inhabitant has a finite index
-- 'length' ('filter' (== x) 'universeF') == 1  -- should terminate
-- (\xs -> 'cardinality' xs == 'genericLength' xs) 'universeF'
-- @
--
-- /Note:/ @'elemIndex' x 'universe' == 'elemIndex' x 'universeF'@
-- may not hold for all types, though the laws imply that `universe`
-- is a permutation of `universeF`.
--
-- @
-- >>> elemIndex (Left True :: Either Bool Bool) universe
-- Just 2
-- >>> elemIndex (Left True :: Either Bool Bool) universeF
-- Just 1
-- @
class Universe a => Finite a where
  universeF :: [a]
  universeF = universe

  cardinality :: proxy a -> Integer
  cardinality = genericLength . ((\_ -> universeF) :: Finite t => proxy t -> [t])

-------------------------------------------------------------------------------
-- Base
-------------------------------------------------------------------------------

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

instance Universe Mon.All where universe = map Mon.All universe
instance Universe Mon.Any where universe = map Mon.Any universe
instance Universe a => Universe (Mon.Sum     a) where universe = map Mon.Sum     universe
instance Universe a => Universe (Mon.Product a) where universe = map Mon.Product universe
instance Universe a => Universe (Mon.Dual    a) where universe = map Mon.Dual    universe
instance Universe a => Universe (Mon.First   a) where universe = map Mon.First   universe
instance Universe a => Universe (Mon.Last    a) where universe = map Mon.Last    universe

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

instance Finite ()       where cardinality _ = 1
instance Finite Bool     where cardinality _ = 2
instance Finite Char     where cardinality _ = 1114112
instance Finite Ordering where cardinality _ = 3
instance Finite Int      where cardinality _ = fromIntegral (maxBound :: Int) - fromIntegral (minBound :: Int) + 1
instance Finite Int8     where cardinality _ = 2^8
instance Finite Int16    where cardinality _ = 2^16
instance Finite Int32    where cardinality _ = 2^32
instance Finite Int64    where cardinality _ = 2^64
instance Finite Word     where cardinality _ = fromIntegral (maxBound :: Word) - fromIntegral (minBound :: Word) + 1
instance Finite Word8    where cardinality _ = 2^8
instance Finite Word16   where cardinality _ = 2^16
instance Finite Word32   where cardinality _ = 2^32
instance Finite Word64   where cardinality _ = 2^64

instance  Finite a            => Finite (Maybe  a  ) where cardinality _ = 1 + cardinality ([] :: [a])
instance (Finite a, Finite b) => Finite (Either a b) where
  universeF = map Left universe ++ map Right universe
  cardinality _ = cardinality ([] :: [a]) + cardinality ([] :: [b])

instance (Finite a, Finite b) => Finite (a, b) where
  universeF = liftM2 (,) universeF universeF
  cardinality _ = product [cardinality ([] :: [a]), cardinality ([] :: [b])]

instance (Finite a, Finite b, Finite c) => Finite (a, b, c) where
  universeF = liftM3 (,,) universeF universeF universeF
  cardinality _ = product [cardinality ([] :: [a]), cardinality ([] :: [b]), cardinality ([] :: [c])]

instance (Finite a, Finite b, Finite c, Finite d) => Finite (a, b, c, d) where
  universeF = liftM4 (,,,) universeF universeF universeF universeF
  cardinality _ = product [cardinality ([] :: [a]), cardinality ([] :: [b]), cardinality ([] :: [c]), cardinality ([] :: [d])]

instance (Finite a, Finite b, Finite c, Finite d, Finite e) => Finite (a, b, c, d, e) where
  universeF = liftM5 (,,,,) universeF universeF universeF universeF universeF
  cardinality _ = product [cardinality ([] :: [a]), cardinality ([] :: [b]), cardinality ([] :: [c]), cardinality ([] :: [d]), cardinality ([] :: [e])]

instance Finite Mon.All where universeF = map Mon.All universeF; cardinality _ = 2
instance Finite Mon.Any where universeF = map Mon.Any universeF; cardinality _ = 2
instance Finite a => Finite (Mon.Sum     a) where universeF = map Mon.Sum     universeF; cardinality = cardinality . unwrapProxy Mon.Sum
instance Finite a => Finite (Mon.Product a) where universeF = map Mon.Product universeF; cardinality = cardinality . unwrapProxy Mon.Product
instance Finite a => Finite (Mon.Dual    a) where universeF = map Mon.Dual    universeF; cardinality = cardinality . unwrapProxy Mon.Dual
instance Finite a => Finite (Mon.First   a) where universeF = map Mon.First   universeF; cardinality = cardinality . unwrapProxy Mon.First
instance Finite a => Finite (Mon.Last    a) where universeF = map Mon.Last    universeF; cardinality = cardinality . unwrapProxy Mon.Last

instance (Ord a, Finite a, Finite b) => Finite (a -> b) where
  universeF = map tableToFunction tables where
    tables          = sequence [universeF | _ <- monoUniverse]
    tableToFunction = (!) . fromList . zip monoUniverse
    monoUniverse    = universeF
  cardinality _ = cardinality ([] :: [b]) ^ cardinality ([] :: [a])

-- to add when somebody asks for it: instance (Eq a, Finite a) => Finite (Endo a) (+Universe)

-------------------------------------------------------------------------------
-- void
-------------------------------------------------------------------------------

instance Universe Void where universe = []
instance Finite Void where cardinality _ = 0

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------


instance (Ord a, Universe a) => Universe (Set.Set a) where
    universe = Set.empty : go universe
      where
        go []     = []
        go (x:xs) = Set.singleton x : inter (go xs)
          where
            -- Probably more efficient than using (+++)
            inter []     = []
            inter (y:ys) = y : Set.insert x y : inter ys


instance (Ord a, Finite a) => Finite (Set.Set a) where
    cardinality p = 2 ^ cardinality (unwrapProxy Set.singleton p)

-- This is tricky
-- instance (Ord k, Universe k, Universe v) => Universe (Map.Map k v)
--     universe = ...

-------------------------------------------------------------------------------
-- transformers
-------------------------------------------------------------------------------



instance  Universe    a                    => Universe (Identity    a) where universe  = map Identity  universe
instance  Universe (f a)                   => Universe (IdentityT f a) where universe  = map IdentityT universe
instance (Finite e, Ord e, Universe (m a)) => Universe (ReaderT e m a) where universe  = map ReaderT   universe
instance  Universe (f (g a))               => Universe (Compose f g a) where universe  = map Compose   universe
instance (Universe (f a), Universe (g a))  => Universe (Product f g a) where universe  = [Pair f g | (f, g) <- universe +*+ universe]
instance  Finite       a                   => Finite   (Identity    a) where universeF = map Identity  universeF; cardinality = cardinality . unwrapProxy Identity
instance  Finite    (f a)                  => Finite   (IdentityT f a) where universeF = map IdentityT universeF; cardinality = cardinality . unwrapProxy IdentityT
instance (Finite e, Ord e, Finite   (m a)) => Finite   (ReaderT e m a) where universeF = map ReaderT   universeF; cardinality = cardinality . unwrapProxy ReaderT
instance  Finite (f (g a))                 => Finite   (Compose f g a) where universeF = map Compose   universeF; cardinality = cardinality . unwrapProxy Compose
instance (Finite (f a), Finite (g a))      => Finite   (Product f g a) where
  universeF = liftM2 Pair universeF universeF
  cardinality proxy = cardinality (unwrapProxy1of2 Pair proxy) * cardinality (unwrapProxy2of2 Pair proxy)
