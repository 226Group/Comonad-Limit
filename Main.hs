{-# LANGUAGE ConstraintKinds #-}
-- import Data.Fix
import qualified Data.Set as S
-- import Data.MultiSet (MultiSet)
import qualified Data.Map as M
import Comonad
import Base
-- module Main where

main = do
  -- print $ iterateMaybe (\ x -> Just x) 0
  --putStrLn "Hello, World!"
   listSumReverse' <$> readLn

type MultiSet a = M.Map a Nat

occur :: Hash a => a -> MultiSet a -> Nat
occur x bag = bag M.! x

insert :: Hash a => a -> MultiSet a -> MultiSet a
insert x = M.insertWith (+) x 1

singleton :: Hash a => a -> MultiSet a
singleton x = x `insert` M.empty

newtype Solution {-(x:Nat)-} = Sum {runSum :: [Nat]}
  deriving (Eq)
instance Show Solution where
  show (Sum xs) = show xs ++ " {" ++ show (sum xs) ++ "}"

sumS (Sum xs) = sum xs
-- : x:Nat -> Solution n -> Solution (x+n)
add x (Sum xs) = Sum (x:xs)

listSumReverse :: Nat -> [Solution]
listSumReverse 0 = [Sum []]
-- listSumReverse 1 = [Sum [1]]
listSumReverse n =
  do
    i <- [0..n-1]
    solution <- listSumReverse i :: [Solution]
    return $ add (n-i) solution


foldNat :: Nat -> (a -> a) -> a -> a
foldNat n f x = iterate f x !! n
-- cata :: (f a -> a) -> Fix f -> a

someAlgMorf :: Nat -> ([a] -> a) -> a
someAlgMorf n mu = last $ foldNat n (\xs -> mu xs : xs) []
--maybe its paramorfism
-- Struct = Fix . f : * -> *
-- generalAlgebraicMorf :: (Struct a -> a) -> Struct a -> a


listSumReverse' :: Nat -> [Solution]
listSumReverse' n = fst $ someAlgMorf n fromPreviousSolutions
  where
    fromPreviousSolutions :: [([Solution], n)] -> ([Solution], n)
    -- : [([Solution n], n:Nat)] -> [Solution n+1]
    fromPreviousSolutions [] {-zeroth-} = ([Sum []], 0)
    fromPreviousSolutions xss = do
      let n = length xss + 1
      (m, solutions) <- xss
      add 1 <$> solutions

indexed :: [a] -> [(Nat, a)]
indexed = zip [0..]
imap :: (Int -> a -> b) -> [a] -> [b]
imap f = map (uncurry f) . indexed

myListSumReverse :: Nat -> [Solution]
myListSumReverse n = foldNat n fromPreviousSums [Sum []]
  where 
  fromPreviousSums :: [Solution] -> [Solution]
  -- [Solution n] -> [Solution n+1]
  fromPreviousSums solution = do
    (Sum solution) <- solution
    --add 0
    un




-- bagSumReverse :: Nat -> S.Set (MultiSet Nat)
-- bagSumReverse n = foldNat n 