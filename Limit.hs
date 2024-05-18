{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Limit where
import Base
import Data.List
import Data.Maybe

-- TODO instance Comonad

-- inf list
inf = 1000 :: Nat
d inf = 1/fromIntegral inf -- :: RealFrac a => a
data RealFrac a => Limit a = UnsafeMkLimit {history::[a], precision::Nat, lim::a}
  deriving (Show)


instance RealFrac a => Extract (Limit a) a where
  run = lim  --history?

data Precision = Exact Nat | Min Nat
  -- with Max actual precision can be lower
-- type SeqLimit = Maybe RealFrac

-- precLimitTo :: forall a. RealFrac a => Nat -> a -> [a] -> Maybe (Limit a)
--inf list
limit :: forall a. RealFrac a => Precision -> Maybe a -> [a] -> Maybe (Limit a)
limit n lim xs = let
  subsequences :: Precision -> [[a]]
  subsequences (Exact n) = [take n . drop (n-2)]
  subsequences (Min n) = [take i . drop (i-2) $ xs | i <- takeWhile (<=n) (2^) <$> [1..]]

  allCloseTo :: RealFrac a => Maybe a -> [a] -> Maybe a
  allCloseTo mLim xs = filterMaybe (all $ approxEq n) lim
    where lim = fromMaybe (last xs) mLim

  in (\xs -> UnsafeMkLimit xs (length xs) (fromJust $ allCloseTo lim)) find (isJust . allCloseTo lim) (subsequences n) -- <> lastSubsequnce

  -- precLimitTo n lim xs | length xs < n = UnsafeMkLimit xs n <$> last xs `eqMaybe` lim --need lazy nats
precLimitTo n lim xs | isNothing $ xs !? n = UnsafeMkLimit xs n <$> last xs `eqMaybe` lim --infinite precision?
-- data InfNat = NatInf | N Nat
precLimitTo n lim xs = let

  in toMaybe (all (approxEq n lim) (subsequence xs)) (UnsafeMkLimit xs n lim)

minPrecLimit :: forall a. RealFrac a => Nat -> [a] -> Maybe (Limit a)
minPrecLimit n xs = let
-- minPrecLimitTo n lim xs | isNothing $ xs !? n = last xs == lim

    firstConverging = find (\subsequence -> all (approxEq n $ last subsequence) subsequence ) subsequences
  in do
    firstConverging <- firstConverging
    return $ UnsafeMkLimit xs (length firstConverging) (last firstConverging)

-- converge :: [a] -> Maybe a
-- converge (x : xs@(y : _))
--   | x ~=~ y = Just x -- check after 
--   | n == 0 = Nothing
--   | otherwise = limitN (n - 1) (MkLimit xs) 

-- limitN _ lim [x]) = lim `eqMaybe` x



-- Nat[] -> a <=> [a]
infLimN :: RealFrac a => Nat -> (Nat -> a) -> Maybe a
infLimN n f = limitN n $ MkLimit $ map f [0..inf]
infLim :: RealFrac a => (Nat -> a) -> Maybe a
infLim = infLimN inf


approxEq :: RealFrac a => Nat -> a -> a -> Bool
approxEq precision x y = abs (x - y) <= realToFrac (d precision)
-- interval
(~=~) :: RealFrac a => a -> a -> Nat -> Bool
x ~=~ y = \precision -> approxEq precision x y
--use application functor?

--limitToN
limitTo :: (RealFrac a) => a -> Limit a -> Bool
limitTo x xs = x ~=~ fromJust (limit xs)

-- instance Num (Limit a)
instance RealFrac a => Eq (Limit a) where
  x == y = fromJust (limit x) ~=~ fromJust (limit y)

-- limList :: Monoid a => [a] -> Limit a

limSum :: (RealFrac a) => [a] -> Limit a
limSum xs = MkLimit [sum $ take n xs | n <- [1..]] -- couldn't use length