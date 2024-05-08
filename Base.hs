{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Base where
import SMO.Base
import Data.List
import Data.Maybe
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Trans.Maybe

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
-- Set a = a -> Bool
-- Struct f a = Fix $ f a
-- Skeleton f = Struct f ()
-- indexesOfEnds :: Struct f -> Set Ind
-- takeG :: Skeleton f -> Struct f a -> Struct f a
-- findIndexG = depthSearch . not :: (Set a) -> (Struct f) -> Skeleton f
toLength :: Ind -> Nat
toLength = succ

-- conveyorList (a: ts@(b:_)) = Conveyor (a -> b) conveyorList ts
type TickerT m a = StateT Nat m a
type Ticker a = TickerT Identity a

tick :: Monad m => TickerT m Nat
tick = do
  i <- get
  modify succ
  return i

tickedM :: forall m a b. Monad m => (a-> m b) -> (a -> TickerT m b)
tickedM = fmap @((->) a) ((tick >>) . lift)  --black magic
  `isSameAs` (((tick >>) . lift) . )
  `isSameAs` \f x -> do
      tick
      lift $ f x

ticked :: (a -> b) -> (a -> State Nat b)
ticked f = tickedM (fmap Identity f)

runTick :: TickerT m a -> m (a, Nat)
runTick ticker = runStateT ticker 0

brokenFarthestM :: forall a. 
  (a -> StateT Nat Maybe a) -> a -> StateT Nat Identity a
brokenFarthestM stateF x = let
  f = fmap runStateT stateF
  f :: a -> Nat -> Maybe (a, Nat)
  goalF :: a -> Maybe (Nat -> (a, Nat))
  goalF = un
  -- goalF x = f x
  -- imposible
  in un

swapMonads :: (Traversable m, Monad n) => m (n a) -> n (m a)
swapMonads = sequence

-- farthestM :: forall m a. (MonadTrans m, Traversable (m Maybe) ) => (a -> m Maybe a) -> a -> m Identity a
farthestM :: forall m a. (Monad m, Traversable m) => (a -> Maybe (m a)) -> a -> m a
farthestM f = farthest @(m a) ( fmap swapMonads(>>=(fmap swapMonads f :: a -> m (Maybe a)))) . return  --i wrote this, but i dont understand this
  where 
  -- f1 :: a -> m (n a)
  heavyMonadery :: (Monad n, Traversable n, Monad m) => m (n (m a)) -> n (m a)
  heavyMonadery = fmap join . swapMonads

tickedFarthest :: (a -> Maybe a) -> a -> (a, Ind)
tickedFarthest = flip fmap2 iterateMaybe $ swap . last . indexes

farthest :: (a -> Maybe a) -> a -> a
farthest f = last . iterateMaybe f

indexes :: [a] -> [(Ind, a)]
indexes = zip [0..]

iterateMaybe, iterateMaybe' :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x = -- strict, couse take is strict
  let tailOfNothings = iterate (>>= f) $ Just x
  in fromJust <$> take (pred $ toLength $ fromJust $ findIndex isNothing tailOfNothings) tailOfNothings

iterateMaybe' f = replaceUndef [] . catMaybes . iterate (>>= f) . Just

-- | Replaces undefined tail of snd list with the first list. Doesn't work.
replaceUndef :: [a] -> [a] -> [a]  -- : Fix f -> <Fix f> -> Fix f
replaceUndef = un -- but if we can proof tail is undefined...