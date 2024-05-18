{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Base where
import Data.List
import Data.Maybe
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad

un = undefined
type Nat = Int
type Ind = Nat
type Cont = Functor

type Hash = Ord

isSameAs :: a -> a -> a
isSameAs x y = x
_ = isSameAs `isSameAs` const
(=~=) = isSameAs
infix 0 =~=

class Iso a b where
  to :: b -> a
  from :: a -> b
instance Iso a b => Iso b a where
  to = from
  from = to

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 f = fmap (fmap f)

swap (x, y) = (y, x)
mapfst f (x, y) = (f x, y)
mapsnd = (\f (x, y) -> (x, f y)) =~= fmap -- @(a, )

thanks = putStrLn "Glad to serve you!"


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


factorial :: Nat -> Nat
factorial n = product [1..n]

check :: (a -> Bool) -> String -> a -> a
check f str x = if f x then x else error str

-- data family + fundep
class Extract a b | a -> b where
  run :: a -> b
run' :: forall b a. Extract a b => a -> b
run' = run

-- isTrue (Just True) = True
-- isTrue _ = False

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

farthest :: (a -> Maybe a) -> a -> a
farthest f = last . iterateMaybe f

indexes :: [a] -> [(Ind, a)]
indexes = zip [0..]

-- Set a = a -> Bool
-- Struct f a = Fix $ f a
-- Skeleton f = Struct f ()
-- indexesOfEnds :: Struct f -> Set Ind
-- takeG :: Skeleton f -> Struct f a -> Struct f a
-- findIndexG = depthSearch . not :: (Set a) -> (Struct f) -> Skeleton f
toLength :: Ind -> Nat
toLength = succ

iterateMaybe, iterateMaybe' :: (a -> Maybe a) -> a -> [a]    --TODO isSameAs  --unfoldMaybe takeWhileJust
iterateMaybe f x = -- strict, couse take is strict
  let tailOfNothings = iterate (>>= f) $ Just x
  in fromJust <$> take (pred $ toLength $ fromJust $ findIndex isNothing tailOfNothings) tailOfNothings

iterateMaybe' f = replaceUndef [] . catMaybes . iterate (>>= f) . Just

-- | Replaces undefined tail of snd list with the first list. Doesn't work.
replaceUndef :: [a] -> [a] -> [a]  -- : Fix f -> <Fix f> -> Fix f
replaceUndef = un -- but if we can proof tail is undefined...