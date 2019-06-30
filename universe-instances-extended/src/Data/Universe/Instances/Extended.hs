{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Universe.Instances.Extended (
  -- | Instances for 'Universe' and 'Finite' for function-like functors and the empty type.
  Universe(..), Finite(..)
  ) where

import Control.Comonad.Trans.Traced (TracedT (..))
#if MIN_VERSION_base(4,7,0)
import Data.Coerce
#endif
import Data.Function (on)
import Data.Functor.Contravariant (Equivalence (..), Op (..), Predicate (..))
import Data.Functor.Rep (Representable (..), Co (..))
import Data.List (genericIndex, genericLength, genericReplicate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Set (Set)
import Data.Universe.Class (Universe (..), Finite (..))
import Data.Universe.Helpers (retag, Tagged, Natural)

import qualified Data.List.NonEmpty as NE (head, last, scanl1, toList)
import qualified Data.Map as M
import qualified Data.Set as S

-- | We could do this:
--
-- instance Universe (f a) => Universe (Co f a) where universe = map Rep universe
--
-- However, since you probably only apply Rep to functors when you want to
-- think of them as being representable, I think it makes sense to use an
-- instance based on the representable-ness rather than the inherent
-- universe-ness.
--
-- Please complain if you disagree!
--
instance (Representable f, Finite (Rep f), Ord (Rep f), Universe a)
  => Universe (Co f a)
  where universe = map tabulate universe
instance (Representable f, Finite s, Ord s, Finite (Rep f), Ord (Rep f), Universe a)
  => Universe (TracedT s f a)
  where universe = map tabulate universe
instance (Finite a, Ord a)
  => Universe (Equivalence a)
  where universe = [Equivalence ((==) `on` (`M.lookup` m)) | m <- partitions universeF]
          where partitionA :: Applicative f => (a -> f Bool) -> [a] -> f ([a], [a])
                partitionA f xs = mconcat <$> traverse inject xs
                            where inject x = (\b -> ([x | b], [x | not b])) <$> f x
                partitions :: Ord a => [a] -> [Map a Natural]
                partitions = go 0
                       where go _ []       = [M.empty]
                             go n (x : xs) = do
                               (eq, notEq) <- partitionA (const [False, True]) xs
                               rest        <- go (n + 1) notEq
                               pure $ M.fromList [(x', n) | x' <- x : eq] `M.union` rest
instance (Universe a, Finite b, Ord b)
  => Universe (Op a b) where
#if MIN_VERSION_base(4,7,0)
        universe = coerce (universe :: [b -> a])
#else
        universe = map Op universe
#endif
instance (Finite a, Ord a)
  => Universe (Predicate a)
  where universe = map (Predicate . flip S.member) universe

instance (Representable f, Finite (Rep f), Ord (Rep f), Finite a)
  => Finite (Co f a)
  where universeF = map tabulate universeF; cardinality = retag (cardinality :: Tagged (Rep (Co        f) -> a) Natural)
instance (Representable f, Finite s, Ord s, Finite (Rep f), Ord (Rep f), Finite a)
  => Finite (TracedT s f a)
  where universeF = map tabulate universeF; cardinality = retag (cardinality :: Tagged (Rep (TracedT s f)) Natural)
instance (Finite a, Ord a)
  => Finite (Equivalence a)
  where cardinality = retag (bell <$> (cardinality :: Tagged a Natural))
              where -- Compute the Bell number for the given cardinality.
                    bell :: Natural -> Natural
                    bell n = NE.head (nth (\ns -> NE.scanl1 (+) (NE.last ns :| NE.toList ns)) (1 :| []))
                       where nth = foldr (.) id . genericReplicate n
instance (Finite a, Finite b, Ord b)
  => Finite (Op a b)
  where cardinality = retag (cardinality :: Tagged (b -> a) Natural)
instance (Finite a, Ord a)
  => Finite (Predicate a)
  where cardinality = retag (cardinality :: Tagged (Set a)  Natural)
