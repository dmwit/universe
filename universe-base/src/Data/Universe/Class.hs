{-# LANGUAGE CPP, BangPatterns, TypeFamilies, ScopedTypeVariables #-}
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

import Control.Applicative (Const (..))
import Control.Monad (liftM2, liftM3, liftM4, liftM5)
import Control.Monad.Trans.Identity (IdentityT (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product (Product (..))
import Data.Functor.Sum (Sum (..))
import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.List (genericLength)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map ((!), fromList)
import Data.Proxy (Proxy (..))
import Data.Ratio (Ratio, numerator, denominator, (%))
import Data.Tagged (Tagged (..), retag)
import Data.Void (Void)
import Data.Word  (Word, Word8, Word16, Word32, Word64)
import GHC.Real (Ratio (..))
import Numeric.Natural (Natural)

import qualified Data.Monoid as Mon
import qualified Data.Semigroup as Semi
import qualified Data.Set as Set
import qualified Data.Map as Map

-- $setup
-- >>> import Data.List

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
-- >>> elemIndex (Left True :: Either Bool Bool) universe
-- Just 2
--
-- >>> elemIndex (Left True :: Either Bool Bool) universeF
-- Just 1
--
class Universe a => Finite a where
  universeF :: [a]
  universeF = universe

  cardinality :: Tagged a Natural
  cardinality = Tagged (genericLength (universeF :: [a]))

-------------------------------------------------------------------------------
-- Base
-------------------------------------------------------------------------------

instance Universe ()       where universe = universeDef
instance Universe Bool     where universe = universeDef
instance Universe Char     where universe = universeDef
instance Universe Ordering where universe = universeDef
instance Universe Integer  where universe = [0, -1..] +++ [1..]
instance Universe Natural  where universe = [0..]
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

instance Universe a => Universe (NonEmpty a) where
  universe = diagonal [[h :| t | t <- universe] | h <- universe]

instance Universe Mon.All where universe = map Mon.All universe
instance Universe Mon.Any where universe = map Mon.Any universe
instance Universe a => Universe (Mon.Sum     a) where universe = map Mon.Sum     universe
instance Universe a => Universe (Mon.Product a) where universe = map Mon.Product universe
instance Universe a => Universe (Mon.Dual    a) where universe = map Mon.Dual    universe
instance Universe a => Universe (Mon.First   a) where universe = map Mon.First   universe
instance Universe a => Universe (Mon.Last    a) where universe = map Mon.Last    universe

-------------------------------------------------------------------------------
-- Semi
-------------------------------------------------------------------------------

instance Universe a => Universe (Semi.Max   a) where universe = map Semi.Max   universe
instance Universe a => Universe (Semi.Min   a) where universe = map Semi.Min   universe
instance Universe a => Universe (Semi.First a) where universe = map Semi.First universe
instance Universe a => Universe (Semi.Last  a) where universe = map Semi.Last  universe

-------------------------------------------------------------------------------
-- Rational
-------------------------------------------------------------------------------

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
-- But this turns out to be substantially slower.
--
-- We used to use
--
--    positiveRationals =
--      1 : map lChild positiveRationals +++ map rChild positiveRationals
--
-- where lChild and rChild produced the left and right child of each fraction,
-- respectively. Aside from building unnecessary thunks (thanks to the lazy
-- map), this had the problem of calculating each sum at least four times:
-- once for a denominator, a second time for the following numerator, and then two
-- more times on the other side of the Calkin-Wilf tree. That doesn't
-- sound too bad, since in practice the integers will be small. But taking each
-- sum allocates a constructor to wrap the result, and that's not
-- free. We can avoid the problem with very little additional effort by
-- interleaving manually. Negative rationals, unfortunately, don't get the
-- full benefit of sharing here, but we can still share their denominators.

infixr 5 :<
data Stream a = !a :< Stream a

-- All the rational numbers on the left side of the Calkin-Wilf tree,
-- in breadth-first order.
leftSideStream :: Integral a => Stream (Ratio a)
leftSideStream = 1 :% 2 :< go leftSideStream
  where
      go (x :< xs) = lChild :< rChild :< go xs
        where
          nd = numerator x + denominator x
          !lChild = numerator x :% nd
          !rChild = nd :% denominator x

instance RationalUniverse a => Universe (Ratio a) where
  universe = rationalUniverse

class RationalUniverse a where
  rationalUniverse :: [Ratio a]

instance RationalUniverse Integer where
    -- Why force the negations and reciprocals? This is more expensive if we
    -- ignore most of the result: it allocates four words (generally) for a
    -- negative element rather than two words for a thunk that will evaluate to
    -- one. But it's presumably more common to use the elements in a universe
    -- than to leap over them, so we optimize for the former case. We
    -- interleave manually to avoid allocating four intermediate lists.
    rationalUniverse = 0 : 1 : (-1) : go leftSideStream
      where
        go (x@(xn :% xd) :< xs) =
          let !nx = -x
              !rx = xd :% xn
              !nrx = -rx
          in x : rx : nx : nrx : go xs

instance RationalUniverse Natural where
    rationalUniverse = 0 : 1 : go leftSideStream
      where
        go (x@(xn :% xd) :< xs) =
          let !rx = xd :% xn
          in x : rx : go xs

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- could change the Ord constraint to an Eq one, but come on, how many finite
-- types can't be ordered?
instance (Finite a, Ord a, Universe b) => Universe (a -> b) where
  universe = map tableToFunction tables where
    tables          = choices [universe | _ <- monoUniverse]
    tableToFunction = (!) . fromList . zip monoUniverse
    monoUniverse    = universeF

instance Finite ()       where cardinality = 1
instance Finite Bool     where cardinality = 2
instance Finite Char     where cardinality = 1114112
instance Finite Ordering where cardinality = 3
instance Finite Int      where cardinality = fromIntegral (maxBound :: Int) - fromIntegral (minBound :: Int) + 1
instance Finite Int8     where cardinality = 2^8
instance Finite Int16    where cardinality = 2^16
instance Finite Int32    where cardinality = 2^32
instance Finite Int64    where cardinality = 2^64
instance Finite Word     where cardinality = fromIntegral (maxBound :: Word) - fromIntegral (minBound :: Word) + 1
instance Finite Word8    where cardinality = Tagged $ 2^8
instance Finite Word16   where cardinality = Tagged $ 2^16
instance Finite Word32   where cardinality = Tagged $ 2^32
instance Finite Word64   where cardinality = Tagged $ 2^64

instance  Finite a            => Finite (Maybe  a  ) where
    cardinality = fmap succ (retag (cardinality :: Tagged a Natural))
instance (Finite a, Finite b) => Finite (Either a b) where
  universeF = map Left universe ++ map Right universe
  cardinality = liftM2 (\a b -> a + b)
    (retag (cardinality :: Tagged a Natural))
    (retag (cardinality :: Tagged b Natural))

instance (Finite a, Finite b) => Finite (a, b) where
  universeF = liftM2 (,) universeF universeF
  cardinality = liftM2 (\a b -> a * b)
    (retag (cardinality :: Tagged a Natural))
    (retag (cardinality :: Tagged b Natural))

instance (Finite a, Finite b, Finite c) => Finite (a, b, c) where
  universeF = liftM3 (,,) universeF universeF universeF
  cardinality = liftM3 (\a b c -> a * b * c)
    (retag (cardinality :: Tagged a Natural))
    (retag (cardinality :: Tagged b Natural))
    (retag (cardinality :: Tagged c Natural))

instance (Finite a, Finite b, Finite c, Finite d) => Finite (a, b, c, d) where
  universeF = liftM4 (,,,) universeF universeF universeF universeF
  cardinality = liftM4 (\a b c d -> a * b * c * d)
    (retag (cardinality :: Tagged a Natural))
    (retag (cardinality :: Tagged b Natural))
    (retag (cardinality :: Tagged c Natural))
    (retag (cardinality :: Tagged d Natural))

instance (Finite a, Finite b, Finite c, Finite d, Finite e) => Finite (a, b, c, d, e) where
  universeF = liftM5 (,,,,) universeF universeF universeF universeF universeF
  cardinality = liftM5 (\a b c d e -> a * b * c * d * e)
    (retag (cardinality :: Tagged a Natural))
    (retag (cardinality :: Tagged b Natural))
    (retag (cardinality :: Tagged c Natural))
    (retag (cardinality :: Tagged d Natural))
    (retag (cardinality :: Tagged e Natural))

instance Finite Mon.All where universeF = map Mon.All universeF; cardinality = 2
instance Finite Mon.Any where universeF = map Mon.Any universeF; cardinality = 2
instance Finite a => Finite (Mon.Sum     a) where universeF = map Mon.Sum     universeF; cardinality = retagWith Mon.Sum     cardinality
instance Finite a => Finite (Mon.Product a) where universeF = map Mon.Product universeF; cardinality = retagWith Mon.Product cardinality
instance Finite a => Finite (Mon.Dual    a) where universeF = map Mon.Dual    universeF; cardinality = retagWith Mon.Dual    cardinality
instance Finite a => Finite (Mon.First   a) where universeF = map Mon.First   universeF; cardinality = retagWith Mon.First   cardinality
instance Finite a => Finite (Mon.Last    a) where universeF = map Mon.Last    universeF; cardinality = retagWith Mon.Last    cardinality

instance Finite a => Finite (Semi.Max   a) where universeF = map Semi.Max   universeF; cardinality = retagWith Semi.Max   cardinality
instance Finite a => Finite (Semi.Min   a) where universeF = map Semi.Min   universeF; cardinality = retagWith Semi.Min   cardinality
instance Finite a => Finite (Semi.First a) where universeF = map Semi.First universeF; cardinality = retagWith Semi.First cardinality
instance Finite a => Finite (Semi.Last  a) where universeF = map Semi.Last  universeF; cardinality = retagWith Semi.Last  cardinality

instance (Ord a, Finite a, Finite b) => Finite (a -> b) where
  universeF = map tableToFunction tables where
    tables          = sequence [universeF | _ <- monoUniverse]
    tableToFunction = (!) . fromList . zip monoUniverse
    monoUniverse    = universeF
  cardinality = liftM2 (^)
    (retag (cardinality :: Tagged b Natural))
    (retag (cardinality :: Tagged a Natural))

-- to add when somebody asks for it: instance (Eq a, Finite a) => Finite (Endo a) (+Universe)

-------------------------------------------------------------------------------
-- void
-------------------------------------------------------------------------------

instance Universe Void where universe = []
instance Finite Void where cardinality = 0

-------------------------------------------------------------------------------
-- tagged
-------------------------------------------------------------------------------

instance Universe (Proxy a) where universe = [Proxy]
instance Finite (Proxy a) where cardinality = 1

instance Universe a => Universe (Tagged b a) where universe = map Tagged universe
instance Finite a => Finite (Tagged b a) where cardinality = retagWith Tagged cardinality

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
    cardinality = retag (fmap (2 ^) (cardinality :: Tagged a Natural))

instance (Ord k, Finite k, Universe v) => Universe (Map.Map k v) where
  universe = map tableToFunction tables where
    tables          = choices [universe | _ <- monoUniverse]
    tableToFunction = fromList' . zip monoUniverse
    monoUniverse    = universeF
    fromList' xs = fromList [ (k,v) | (k, Just v) <- xs ]

instance (Ord k, Finite k, Finite v) => Finite (Map.Map k v) where
  universeF = map tableToFunction tables where
    tables          = sequence [universeF | _ <- monoUniverse]
    tableToFunction = fromList' . zip monoUniverse
    monoUniverse    = universeF
    fromList' xs = fromList [ (k,v) | (k, Just v) <- xs ]

  cardinality = liftM2 (\b a -> (1 + b) ^ a)
    (retag (cardinality :: Tagged v Natural))
    (retag (cardinality :: Tagged k Natural))

-------------------------------------------------------------------------------
-- transformers
-------------------------------------------------------------------------------

instance  Universe  a => Universe (Const a b) where universe  = map Const universe
instance  Finite    a => Finite   (Const a b) where universeF = map Const universeF; cardinality = retagWith Const cardinality

instance  Universe    a                    => Universe (Identity    a) where universe  = map Identity  universe
instance  Universe (f a)                   => Universe (IdentityT f a) where universe  = map IdentityT universe
instance (Finite e, Ord e, Universe (m a)) => Universe (ReaderT e m a) where universe  = map ReaderT   universe
instance  Universe (f (g a))               => Universe (Compose f g a) where universe  = map Compose   universe
instance (Universe (f a), Universe (g a))  => Universe (Product f g a) where universe  = [Pair f g | (f, g) <- universe +*+ universe]
instance (Universe (f a), Universe (g a))  => Universe (Sum     f g a) where universe  = map InL universe +++ map InR universe

instance  Finite       a                   => Finite   (Identity    a) where universeF = map Identity  universeF; cardinality = retagWith Identity  cardinality
instance  Finite    (f a)                  => Finite   (IdentityT f a) where universeF = map IdentityT universeF; cardinality = retagWith IdentityT cardinality
instance (Finite e, Ord e, Finite   (m a)) => Finite   (ReaderT e m a) where universeF = map ReaderT   universeF; cardinality = retagWith ReaderT   cardinality
instance  Finite (f (g a))                 => Finite   (Compose f g a) where universeF = map Compose   universeF; cardinality = retagWith Compose   cardinality
instance (Finite (f a), Finite (g a))      => Finite   (Product f g a) where
  universeF = liftM2 Pair universeF universeF
  cardinality = liftM2 (*)
    (retag (cardinality :: Tagged (f a) Natural))
    (retag (cardinality :: Tagged (g a) Natural))
instance (Finite (f a), Finite (g a))      => Finite   (Sum f g a) where
  universeF =  map InL universe ++ map InR universe
  cardinality = liftM2 (+)
    (retag (cardinality :: Tagged (f a) Natural))
    (retag (cardinality :: Tagged (g a) Natural))
