-- import Debug.Trace
-- import Debug
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Base where
import Data.Maybe
import Data.List

(!?) :: [a] -> Nat -> Maybe a
[] !? _ = Nothing
(x:xs) !? 0 = Just x
(x:xs) !? i = xs !? (i - 1)
-- fromJust :: Maybe a -> a
-- fromJust (Just x) = x
toMaybe :: Bool -> a -> Maybe a
toMaybe True x = Just x
toMaybe False _ = Nothing

filterMaybe :: (a -> Bool) -> a -> Maybe a
filterMaybe f x = toMaybe (f x) x

eqMaybe :: Eq a => a -> a -> Maybe a
eqMaybe x = filterMaybe (== x)


type Nat = Int
un = undefined
factorial :: Nat -> Nat
factorial n = product [1..n]

mapfst :: (a -> b) -> (a, c) -> (b, c)
mapfst f (x, y) = (f x, y)

mapsnd :: (b -> c) -> (a, b) -> (a, c)
mapsnd f (x, y) = (x, f y)

check :: (a -> Bool) -> String -> a -> a
check f str x = if f x then x else error str

-- data family + fundep
class Extract a b | a -> b where
  run :: a -> b
run' :: forall b a. Extract a b => a -> b
run' = run
-- class Extract a b | a -> b where

isTrue (Just True) = True
isTrue _ = False

lift2A :: Applicative m => (a -> b -> c) -> m a -> m b -> m c
lift2A f x y = f <$> x <*> y

--TODO lazy Nats
from_N_to_List_len :: Nat -> [a] -> [Nat]
from_N_to_List_len n xs = fst <$> zip [n..] xs

enumerate :: [a] -> [(Nat, a)]
enumerate = zip [0..]

(??) :: Functor f => f (a -> b) -> a -> f b 
infixl 1 ??
fab ?? a = fmap ($ a) fab