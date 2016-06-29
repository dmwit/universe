{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.Set (Set)
import Test.QuickCheck
import Data.Universe.Class
import Data.Universe.Instances.Base ()
import Data.Universe.Instances.Containers ()

import qualified Data.Set as Set

-- | Proxy type
data P a = P

-------------------------------------------------------------------------------
-- Universe laws
-------------------------------------------------------------------------------

universeLaw :: (Eq a, Show a, Arbitrary a, Universe a) => P a -> a -> Property
universeLaw _ x = counterexample (show x) (elem x universe)

universeProdLaw
    :: forall a. (Ord a, Show a, Arbitrary a, Universe a)
    => P a -> NonNegative (Small Int) -> Property
universeProdLaw _ (NonNegative (Small n)) = label (show $ div n 10) $
    let n' = min 10 n
        pfx = take n' universe :: [a]
    in length pfx === nubLength pfx

universeLaws :: (Ord a, Show a, Arbitrary a, Universe a) => P a -> Property
universeLaws p = universeLaw p .&&. universeProdLaw p

nubLength :: Ord a => [a] -> Int
nubLength = Set.size . Set.fromList

-------------------------------------------------------------------------------
-- Finite laws
-------------------------------------------------------------------------------

finiteLaw1 :: (Eq a, Show a, Arbitrary a, Finite a) => P a -> a -> Property
finiteLaw1 _ x = counterexample (show x) (elem x universeF)

finiteLaw2 :: (Eq a, Show a, Arbitrary a, Finite a) => P a -> a -> Property
finiteLaw2 _ x = length (filter (== x) universeF) === 1

finiteLaws :: (Ord a, Show a, Arbitrary a, Finite a) => P a -> Property
finiteLaws p = universeLaws p .&&. finiteLaw1 p .&&. finiteLaw2 p

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    quickCheck $ universeProdLaw (P :: P (Set Integer))
    quickCheck $ finiteLaws (P :: P (Set ()))
    quickCheck $ finiteLaws (P :: P (Set Bool))
    quickCheck $ finiteLaws (P :: P (Set (Maybe Bool)))
    quickCheck $ finiteLaws (P :: P (Set (Set (Maybe Bool))))
