module Data.Universe.Helpers (
	-- | This module is for functions that are useful for writing instances,
	-- but not necessarily for using them (and hence are not exported by the
	-- main module to avoid cluttering up the namespace).
	module Data.Universe.Helpers
	) where

import Data.List

-- | For many types, the 'universe' should be @[minBound .. maxBound]@;
-- 'universeDef' makes it easy to make such types an instance of 'Universe' via
-- the snippet
--
-- > instance Universe Foo where universe = universeDef
universeDef :: (Bounded a, Enum a) => [a]
universeDef = [minBound .. maxBound]

-- | Fair n-way interleaving: given a finite number of (possibly infinite)
-- lists, produce a single list such that whenever @v@ has finite index in one
-- of the input lists, @v@ also has finite index in the output list. No list's
-- elements occur more frequently (on average) than another's.
interleave :: [[a]] -> [a]
interleave = concat . transpose

-- | Unfair n-way interleaving: given a possibly infinite number of (possibly
-- infinite) lists, produce a single list such that whenever @v@ has finite
-- index in an input list at finite index, @v@ also has finite index in the
-- output list. Elements from lists at lower index occur more frequently, but
-- not exponentially so.
diagonal :: [[a]] -> [a]
diagonal = concat . diagonals

-- | Like 'diagonal', but expose a tiny bit more (non-semantic) information:
-- if you lay out the input list in two dimensions, each list in the result
-- will be one of the diagonals of the input. In particular, each element of
-- the output will be a list whose elements are each from a distinct input
-- list.
diagonals :: [[a]] -> [[a]]
diagonals = go [] where
	-- it is critical for some applications that we start producing answers
	-- before inspecting es_
	go b es_ = [h | h:_ <- b] : case es_ of
		[]   -> transpose ts
		e:es -> go (e:ts) es
		where ts = [t | _:t <- b]

-- | Fair 2-way interleaving.
(+++) :: [a] -> [a] -> [a]
xs +++ ys = interleave [xs,ys]

-- | Slightly unfair 2-way Cartesian product: given two (possibly infinite)
-- lists, produce a single list such that whenever @v@ and @w@ have finite
-- indices in the input lists, @(v,w)@ has finite index in the output list.
-- Lower indices occur as the @fst@ part of the tuple more frequently, but not
-- exponentially so.
(+*+) :: [a] -> [b] -> [(a,b)]
[] +*+ _  = [] -- special case: don't want to construct an infinite list of empty lists to pass to diagonal
xs +*+ ys = diagonal [[(x, y) | x <- xs] | y <- ys]

-- | Slightly unfair n-way Cartesian product: given a finite number of
-- (possibly infinite) lists, produce a single list such that whenever @vi@ has
-- finite index in list i for each i, @[v1, ..., vn]@ has finite index in the
-- output list.
choices :: [[a]] -> [[a]]
choices = foldr ((map (uncurry (:)) .) . (+*+)) [[]]

-- | Very unfair 2-way Cartesian product: same guarantee as the slightly unfair
-- one, except that lower indices may occur as the @fst@ part of the tuple
-- exponentially more frequently. This mainly exists as a specification to test
-- against.
unfairCartesianProduct :: [a] -> [b] -> [(a,b)]
unfairCartesianProduct _  [] = [] -- special case: don't want to walk down xs forever hoping one of them will produce a nonempty thing
unfairCartesianProduct xs ys = go xs ys where
	go (x:xs) ys = map ((,) x) ys +++ go xs ys
	go []     ys = []

-- | Very unfair n-way Cartesian product: same guarantee as the slightly unfair
-- one, but not as good in the same sense that the very unfair 2-way product is
-- worse than the slightly unfair 2-way product. Mainly for testing purposes.
unfairChoices :: [[a]] -> [[a]]
unfairChoices = foldr ((map (uncurry (:)) .) . unfairCartesianProduct) [[]]
