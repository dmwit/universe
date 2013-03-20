{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Data.Universe
	( -- | Bottoms are ignored for this entire module: only fully-defined inhabitants are considered inhabitants.
	  Universe(..)
	, Finite(..)
	) where

import Control.Monad
import Data.Int
import Data.Map ((!), fromList)
import Data.Monoid
import Data.Ratio
import Data.Universe.Helpers
import Data.Void
import Data.Word

-- for representable stuff!
import Control.Comonad.Trans.Traced
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Trans.Identity
import Data.Functor.Compose
import Data.Functor.Representable
import Data.Key (Key)
import qualified Data.Functor.Product as Functor

-- | Creating an instance of this class is a declaration that your type is
-- recursively enumerable (and that 'universe' is that enumeration). In
-- particular, you promise that any finite inhabitant has a finite index in
-- 'universe', and that no inhabitant appears at two different finite indices.
class Universe a where universe :: [a]

-- | Creating an instance of this class is a declaration that your 'universe'
-- eventually ends. Minimal definition: no methods defined. By default,
-- @universeF = universe@, but for some types (like 'Either') the 'universeF'
-- method may have a more intuitive ordering.
class Universe a => Finite a where
	universeF :: [a]
	universeF = universe

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
instance Universe Void     where universe = []

instance (Universe a, Universe b) => Universe (Either a b) where universe = map Left universe +++ map Right universe
instance  Universe a              => Universe (Maybe  a  ) where universe = Nothing : map Just universe

instance (Universe a, Universe b) => Universe (a, b) where universe = universe +*+ universe
instance (Universe a, Universe b, Universe c) => Universe (a, b, c) where universe = [(a,b,c) | ((a,b),c) <- universe +*+ universe +*+ universe]
instance (Universe a, Universe b, Universe c, Universe d) => Universe (a, b, c, d) where universe = [(a,b,c,d) | (((a,b),c),d) <- universe +*+ universe +*+ universe +*+ universe]
instance (Universe a, Universe b, Universe c, Universe d, Universe e) => Universe (a, b, c, d, e) where universe = [(a,b,c,d,e) | ((((a,b),c),d),e) <- universe +*+ universe +*+ universe +*+ universe +*+ universe]

instance Universe All where universe = map All universe
instance Universe Any where universe = map Any universe
instance Universe a => Universe (Sum     a) where universe = map Sum     universe
instance Universe a => Universe (Product a) where universe = map Product universe
instance Universe a => Universe (Dual    a) where universe = map Dual    universe
instance Universe a => Universe (First   a) where universe = map First   universe
instance Universe a => Universe (Last    a) where universe = map Last    universe

-- see http://mathlesstraveled.com/2008/01/07/recounting-the-rationals-part-ii-fractions-grow-on-trees/
-- TODO: since we know these numerators and denominators are always going to be
-- in reduced terms, we could use (:%) when we know we're compiling with GHC to
-- get a small speed boost
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

-- instances for Representable functors; in general we want
--   instance (Finite (Key f), Ord (Key f), Universe a, Representable f)
--   	=> Universe (f a)
--   	where universe = map tabulate universe
-- but this has ridiculous overlap, so we expand this for each of the
-- instantiations of f that are Representable instead

instance Universe a => Universe (Identity a) where universe = map Identity universe
instance (Representable f, Finite (Key f), Ord (Key f), Universe a)
	=> Universe (IdentityT f a)
	where universe = map tabulate universe
instance (Representable f, Finite (Key f), Ord (Key f), Universe a)
	=> Universe (Rep f a)
	where universe = map tabulate universe
instance (Representable f, Finite s, Ord s, Finite (Key f), Ord (Key f), Universe a)
	=> Universe (TracedT s f a)
	where universe = map tabulate universe
instance (Representable f, Finite e, Ord e, Finite (Key f), Ord (Key f), Universe a)
	=> Universe (ReaderT e f a)
	where universe = map tabulate universe
instance (Representable f, Representable g, Finite (Key f), Ord (Key f), Finite (Key g), Ord (Key g), Universe a)
	=> Universe (Compose f g a)
	where universe = map tabulate universe
instance (Representable f, Representable g, Finite (Key f), Ord (Key f), Finite (Key g), Ord (Key g), Universe a)
	=> Universe (Functor.Product f g a)
	where universe = map tabulate universe

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
instance Finite Void
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

instance (Ord a, Finite a, Finite b) => Finite (a -> b)

-- to add as people ask for them:
-- instance (Eq a, Finite a) => Finite (Endo a) (+Universe)
-- instance (Ord a, Universe a) => Universe (Set a) (+Finite)
-- instance (Ord k, Universe k, Universe v) => Universe (Map k v) (+Finite)
