{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.List (elemIndex, nub)
import Data.Int (Int8)
import Test.QuickCheck
import Data.Universe.Instances.Base (Universe(..), Finite(..))

-- | Proxy type
data P a = P

-------------------------------------------------------------------------------
-- Universe laws
-------------------------------------------------------------------------------

universeLaw :: (Eq a, Show a, Arbitrary a, Universe a) => P a -> a -> Property
universeLaw _ x = counterexample (show x) (elem x universe)

universeProdLaw
    :: forall a. (Eq a, Show a, Arbitrary a, Universe a)
    => P a -> NonNegative Int -> Property
universeProdLaw _ (NonNegative n) = label (show n) $
    let pfx = take n universe :: [a]
    in length pfx === length (nub pfx)

universeLaws :: (Eq a, Show a, Arbitrary a, Universe a) => P a -> Property
universeLaws p = universeLaw p .&&. universeProdLaw p

-------------------------------------------------------------------------------
-- Finite laws
-------------------------------------------------------------------------------

finiteLaw1 :: (Eq a, Show a, Arbitrary a, Finite a) => P a -> a -> Property
finiteLaw1 _ x = counterexample (show x) (elem x universeF)

finiteLaw2 :: (Eq a, Show a, Arbitrary a, Finite a) => P a -> a -> Property
finiteLaw2 _ x = length (filter (== x) universeF) === 1

finiteLaws :: (Eq a, Show a, Arbitrary a, Finite a) => P a -> Property
finiteLaws p = universeLaws p .&&. finiteLaw1 p .&&. finiteLaw2 p

-------------------------------------------------------------------------------
-- Special examples
-------------------------------------------------------------------------------

eitherExample :: Property
eitherExample = once $ u /= f
  where
    u = elemIndex (Left True :: Either Bool Bool) universe
    f = elemIndex (Left True :: Either Bool Bool) universeF

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    -- Note: checking on 'Int' is bad idea as it's definition is 'universeDef',
    -- i.e. it takes lots of time to get to small numbers!
    quickCheck eitherExample
    quickCheck $ universeLaws (P :: P Integer)
    quickCheck $ finiteLaws (P :: P Char)
    quickCheck $ finiteLaws (P :: P (Maybe Int8))
    quickCheck $ finiteLaws (P :: P (Either Int8 Int8))
    -- Even this is a bad idea:
    -- quickCheck $ universeLaw (P :: P [Bool])
