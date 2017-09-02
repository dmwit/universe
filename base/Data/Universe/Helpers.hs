{-# LANGUAGE RankNTypes, DeriveFunctor, DeriveFoldable, DeriveTraversable, GADTs, TupleSections, BangPatterns #-}
module Data.Universe.Helpers (
    -- | This module is for functions that are useful for writing instances,
    -- but not necessarily for using them (and hence are not exported by the
    -- main module to avoid cluttering up the namespace).

    -- * Tree
    Stream (..),
    unstream,
    streamLength,
    consStream,
    unfoldrInfiniteStream,
    streamIterate,

    -- * Building lists
    universeDef,
    interleave,
    diagonal,
    (+++),
    (+*+),
    -- choices,

    -- * Building cardinalities
    -- | These functions are handy for inheriting the definition of
    -- 'Data.Universe.Class.cardinality' in a newtype instance. For example,
    -- one might write
    --
    -- > newtype Foo = Foo Bar
    -- > instance Finite Foo where cardinality = cardinality . unwrapProxy Foo
    unwrapProxy,
    unwrapProxy1of2,
    unwrapProxy2of2,

    -- * Debugging
    -- | These functions exist primarily as a specification to test against.
    unfairCartesianProduct,
    -- unfairChoices
    ) where

import Data.List
import Data.Foldable
import Data.Traversable

import Debug.Trace

data Stream a where
    Stream :: !(s -> Step a s) -> !s -> Stream a

instance Functor Stream where
    fmap f (Stream next s0) = Stream next' s0
      where
        next' s = case next s of
            Done -> Done
            Skip s' -> Skip s'
            Yield x s' -> Yield (f x) s'

data Step a s
    = Yield a !s
    | Skip !s
    | Done

instance Functor (Step a) where
    fmap f (Yield x s) = Yield x (f s)
    fmap f (Skip s)    = Skip (f s)
    fmap _ Done        = Done

consStream :: a -> Stream a -> Stream a
consStream x (Stream next' initS) = Stream next Nothing
  where
    next Nothing  = Yield x (Just initS)
    next (Just s) = fmap Just (next' s)

unfoldrStream :: s -> (s -> Maybe (a, s)) -> Stream a
unfoldrStream initS next' = Stream next initS
  where
    next s = case next' s of
        Nothing     -> Done
        Just (x, s) -> Yield x s

unfoldrInfiniteStream :: s -> (s -> (a, s)) -> Stream a
unfoldrInfiniteStream initS next = Stream (uncurry Yield . next) initS

streamIterate :: a -> (a -> a) -> Stream a
streamIterate x f = Stream next x where
    next x = Yield x (f x)

streamLength :: Stream a -> Integer
streamLength (Stream next s) = go 0 s where
    go !acc !s = case next s of
        Yield _ s -> go (1 + acc) s
        Skip s    -> go acc s
        Done      -> acc

emptyStream :: Stream a
emptyStream = Stream (const Done) ()

foldlStream :: (b -> a -> b) -> b -> Stream a -> b
foldlStream f z (Stream next s) = go z s
  where
    go acc s = case next s of
        Done       -> acc
        Skip s'    -> go acc s'
        Yield x s' -> go (f acc x) s'

-- | Flatten a stream back into a list.
unstream :: Stream a -> [a]
unstream (Stream next s0) = go s0
  where
    go !s = case next s of
        Done       -> []
        Skip    s' -> go s'
        Yield x s' -> x : go s'

-- | For many types, the 'universe' should be @[minBound .. maxBound]@;
-- 'universeDef' makes it easy to make such types an instance of 'Universe' via
-- the snippet
--
-- > instance Universe Foo where universe = universeDef
universeDef :: (Bounded a, Enum a) => Stream a
universeDef = Stream next [minBound .. maxBound] where
    next []       = Done
    next (x : xs) = Yield x xs

-- | Fair 2-way interleaving.
(+++) :: Stream a -> Stream a -> Stream a
Stream nextA initA +++ Stream nextB initB = Stream next (NextA initA initB)
  where
    next (NextA sa sb) = case nextA sa of
        Done        -> Skip (DrainB sb)
        Skip sa'    -> Skip (NextB sa' sb)
        Yield x sa' -> Yield x (NextB sa' sb)
    next (NextB sa sb) = case nextB sb of
        Done        -> Skip (DrainA sa)
        Skip sb'    -> Skip (NextA sa sb')
        Yield x sb' -> Yield x (NextA sa sb')
    next (DrainA sa) = case nextA sa of
        Done        -> Done
        Skip sa'    -> Skip (DrainA sa')
        Yield x sa' -> Yield x (DrainA sa')
    next (DrainB sb) = case nextB sb of
        Done        -> Done
        Skip sb'    -> Skip (DrainB sb')
        Yield x sb' -> Yield x (DrainB sb')

-- State for (+++)
data I2 sa sb
    = NextA !sa !sb
    | NextB !sa !sb
    | DrainA !sa
    | DrainB !sb

-- TODO: make "square producter"
--
-- - first go from top-to-corner
-- - then left-to-corner
-- - then corner
-- - ... continue with next level
-- 
-- @
-- 1234
-- 2234
-- 3334
-- 4444
-- @
--
unfairProduct :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
unfairProduct f as bs = foldlStream (\cs a -> fmap (f a) bs +++ cs) emptyStream as

-- | Fair n-way interleaving: given a finite number of (possibly infinite)
-- lists, produce a single list such that whenever @v@ has finite index in one
-- of the input lists, @v@ also has finite index in the output list. No list's
-- elements occur more frequently (on average) than another's.
interleave :: [Stream a] -> Stream a 
interleave = foldl' (+++) emptyStream -- TODO

-- | Unfair n-way interleaving: given a possibly infinite number of (possibly
-- infinite) lists, produce a single list such that whenever @v@ has finite
-- index in an input list at finite index, @v@ also has finite index in the
-- output list. Elements from lists at lower index occur more frequently, but
-- not exponentially so.
diagonal :: [Stream a] -> Stream a
diagonal = interleave -- todo

{-
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
-}

-- | Slightly unfair 2-way Cartesian product: given two (possibly infinite)
-- lists, produce a single list such that whenever @v@ and @w@ have finite
-- indices in the input lists, @(v,w)@ has finite index in the output list.
-- Lower indices occur as the @fst@ part of the tuple more frequently, but not
-- exponentially so.
(+*+) :: Stream a -> Stream b -> Stream (a, b)
(+*+) = unfairProduct (,)

{-
-- | Slightly unfair n-way Cartesian product: given a finite number of
-- (possibly infinite) lists, produce a single list such that whenever @vi@ has
-- finite index in list i for each i, @[v1, ..., vn]@ has finite index in the
-- output list.
choices :: [Stream a] -> [[a]]
choices = foldr ((map (uncurry (:)) .) . (+*+)) [[]]
-}

-- | Convert a proxy for a newtype to a proxy for the contained type, given the
-- newtype's constructor.
unwrapProxy     :: (a -> b)      -> proxy b -> [a]

-- | Convert a proxy for a pair-like type to a proxy for the first part of the
-- pair, given a pairing-like constructor.
unwrapProxy1of2 :: (a -> b -> c) -> proxy c -> [a]
--
-- | Convert a proxy for a pair-like type to a proxy for the second part of the
-- pair, given a pairing-like constructor.
unwrapProxy2of2 :: (a -> b -> c) -> proxy c -> [b]

unwrapProxy     _ _ = []
unwrapProxy1of2 _ _ = []
unwrapProxy2of2 _ _ = []

-- | Very unfair 2-way Cartesian product: same guarantee as the slightly unfair
-- one, except that lower indices may occur as the @fst@ part of the tuple
-- exponentially more frequently.
unfairCartesianProduct :: Stream a -> Stream b -> Stream (a, b)
unfairCartesianProduct = unfairProduct (,)

{-
-- | Very unfair n-way Cartesian product: same guarantee as the slightly unfair
-- one, but not as good in the same sense that the very unfair 2-way product is
-- worse than the slightly unfair 2-way product.
unfairChoices :: [[a]] -> [[a]]
unfairChoices = foldr ((map (uncurry (:)) .) . unfairCartesianProduct) [[]]
-}
