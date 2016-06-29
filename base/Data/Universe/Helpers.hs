{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
-- | This module is for functions that are useful for writing instances,
-- but not necessarily for using them (and hence are not exported by the
 -- main module to avoid cluttering up the namespace).
module Data.Universe.Helpers (
   -- * 'Univ' type
   Univ(..),
   emptyUniv,
   univCons,
   diagonal,
   (+++),
   (+*+),
   choices,
   -- * Default definitions
   universeDef,
   ) where

import Prelude.Compat

import Control.Applicative (Alternative (..))
import Data.Typeable (Typeable)
import Data.Semigroup (Semigroup (..))
import Data.List.Compat

-- | Type synonym representing container of elements.
--
-- 'Univ' has one invariant: all elements in @'Univ' a@ are distinct.
--
-- See <https://hackage.haskell.org/package/control-monad-omega-0.3.1/docs/Control-Monad-Omega.html>
newtype Univ a = Univ { getUniv :: [a] }
   deriving (Functor, Foldable, Traversable, Typeable)

instance Applicative Univ where
    pure = Univ . return
    f <*> x = uncurry ($) <$> f +*+ x

instance Monad Univ where
    return = Univ . return
    m >>= f = Univ $ getUniv m >>= getUniv . f

instance Alternative Univ where
    empty = mempty
    (<|>) = (<>)

emptyUniv :: Univ a
emptyUniv = Univ []

univCons :: a -> Univ a -> Univ a
univCons x xs = pure x <> xs

-- | Appending is fair interleaving, associativity rule holds if one consider equality on `Univ` as sets.
instance Semigroup (Univ a) where
   (<>) = (+++)

instance Monoid (Univ a) where
    mempty = emptyUniv
    mappend = (+++)

-- | For many types, the 'universe' should be @[minBound .. maxBound]@;
-- 'universeDef' makes it easy to make such types an instance of 'Universe' via
-- the snippet
--
-- > instance Universe Foo where universe = universeDef
universeDef :: (Bounded a, Enum a) => Univ a
universeDef = Univ [minBound .. maxBound]

-- | Fair n-way interleaving: given a finite number of (possibly infinite)
-- lists, produce a single list such that whenever @v@ has finite index in one
-- of the input lists, @v@ also has finite index in the output list. No list's
-- elements occur more frequently (on average) than another's.
interleave :: [Univ a] -> Univ a
interleave = Univ . concat . transpose . fmap getUniv

-- | Unfair n-way interleaving: given a possibly infinite number of (possibly
-- infinite) lists, produce a single list such that whenever @v@ has finite
-- index in an input list at finite index, @v@ also has finite index in the
-- output list. Elements from lists at lower index occur more frequently, but
-- not exponentially so.
--
-- TODO: `join`, check use-cases
diagonal :: [Univ a] -> Univ a
diagonal = Univ . concat . diagonals . fmap getUniv

-- | Like 'diagonal', but expose a tiny bit more (non-semantic) information:
-- if you lay out the input list in two dimensions, each list in the result
-- will be one of the diagonals of the input. In particular, each element of
-- the output will be a list whose elements are each from a distinct input
-- list.
diagonals :: [[a]] -> [[a]]
diagonals = tail . go [] where
    -- it is critical for some applications that we start producing answers
    -- before inspecting es_
    go b es_ = [h | h:_ <- b] : case es_ of
        []   -> transpose ts
        e:es -> go (e:ts) es
        where ts = [t | _:t <- b]

-- | Fair 2-way interleaving.
(+++) :: Univ a -> Univ a -> Univ a
xs +++ ys = interleave [xs,ys]

-- | Slightly unfair 2-way Cartesian product: given two (possibly infinite)
-- lists, produce a single list such that whenever @v@ and @w@ have finite
-- indices in the input lists, @(v,w)@ has finite index in the output list.
-- Lower indices occur as the @fst@ part of the tuple more frequently, but not
-- exponentially so.
(+*+) :: Univ a -> Univ b -> Univ (a,b)
Univ xs +*+ Univ ys = Univ $ unfairProduct xs ys

unfairProduct :: [a] -> [b] -> [(a,b)]
unfairProduct []  _ = [] -- special case: don't want to construct an infinite list of empty lists to pass to diagonal
unfairProduct xs ys = getUniv $ diagonal [Univ [(x, y) | x <- xs] | y <- ys]

-- | Slightly unfair n-way Cartesian product: given a finite number of
-- (possibly infinite) lists, produce a single list such that whenever @vi@ has
-- finite index in list i for each i, @[v1, ..., vn]@ has finite index in the
-- output list.
--
-- TODO: this is probably the same as 'unfairCartesianProduct' atm.
choices :: [Univ a] -> Univ [a]
choices = sequenceA

{-
-- | Very unfair 2-way Cartesian product: same guarantee as the slightly unfair
-- one, except that lower indices may occur as the @fst@ part of the tuple
-- exponentially more frequently. This mainly exists as a specification to test
-- against.
unfairCartesianProduct :: Univ a -> Univ b -> Univ (a,b)
unfairCartesianProduct _         (Univ []) = emptyUniv -- special case: don't want to walk down xs forever hoping one of them will produce a nonempty thing
unfairCartesianProduct (Univ xs') ys        = go xs' where
    go (x:xs)  = fmap ((,) x) ys +++ go xs
    go []      = emptyUniv

-- | Very unfair n-way Cartesian product: same guarantee as the slightly unfair
-- one, but not as good in the same sense that the very unfair 2-way product is
-- worse than the slightly unfair 2-way product. Mainly for testing purposes.
unfairChoices :: [[a]] -> [[a]]
unfairChoices = foldr ((map (uncurry (:)) .) . unfairCartesianProduct) [[]]
-}
