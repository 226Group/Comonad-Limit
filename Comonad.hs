{-#LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications#-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}

module Comonad where

import Base
import Control.Monad.Trans.State.Lazy
import Control.Monad
import Data.Maybe

class Functor f => Comonad f where
  extract :: f a -> a
  duplicate :: f a -> f (f a)
extend :: Comonad f => f a -> (f a -> b) -> f b
extend mx f = f <$> duplicate mx
-- (=>>) = extend -- :: Comonad f => f a -> (f a -> b) -> f b
-- WTF



instance Show a => Show (LZipper a) where
  show (LZipper xs x ys) = "LZipper: " ++ unwords (show <$> [reverse xs, [x], ys]) --impredactive types...

type IndexedL a = (Nat, [a])
data LZipper a = LZipper [a] a [a]
  deriving (Eq, Functor)
instance Iso (IndexedL a) (LZipper a) where
  to = toIndexedL
  from = fromIndexedL


fromIndexedL :: IndexedL a -> LZipper a
fromIndexedL (0, xs) = toZipper xs
fromIndexedL (i, x:xs) = addToTop x $ fromIndexedL (i-1, xs)
fromIndexedL (i, []) = error "Indexation error"
--bug: fromIndexedL = toZipper

toZipper :: [a] -> LZipper a
toZipper (x:xs) = LZipper [] x xs
toZipper [] = error "empty list"

addToTop :: a -> LZipper a -> LZipper a
-- addToTop x = fromIndexedL . fmap (x:) . toIndexedL
addToTop x (LZipper xs y ys) = LZipper (xs++[x]) y ys

down :: () -> LZipper a -> Maybe (LZipper a)
down () (LZipper xs x (y:ys)) = Just $ LZipper (x:xs) y ys
down () (LZipper xs x []) = Nothing

up :: LZipper a -> Maybe (LZipper a)
up (LZipper (x:xs) y ys) = Just $ LZipper xs x (y:ys)
up (LZipper [] y ys) = Nothing

toIndexedL :: LZipper a -> IndexedL a
toIndexedL = mapsnd fromTop . swap . tickedFarthest up
-- toIndexedL zipper = fmap fromTop $ runTick $ farthestM (fmap swapMonads {- Identity @(Ticker)-} $ ticked up) zipper --(a -> Maybe (m a))
--runTick . farthest (>>= ticked up) . return
  where
  fromTop :: LZipper a -> [a]
  fromTop (LZipper [] x ys) = x:ys
  fromTop _ = error "toIndexedL is broken"

-- Nat = [()]
type GPath i = [Step i]
--type Int = GPath Nat
data Step dir = Up | Down dir

step :: Step () -> LZipper i -> Maybe (LZipper i)
step Up = up
step (Down unitT) = down unitT

intToPath :: Int -> GPath ()
intToPath = un
-- intToPath i | i>=0 = UpDown 0 i
-- intToPath i | i <0 = UpDown (abs i) 0

move :: Int -> LZipper a -> Maybe (LZipper a)
move i | i <0  = move (i+1) <=< up
       | i==0 = Just
       | i >0  = move (i-1) <=< down ()

instance Comonad LZipper where
  extract (LZipper _ x _) = x
  duplicate zipp = let
    -- go :: (LZipper a -> Maybe (LZipper a)) -> LZipper a -> [LZipper a]
    -- go movement = maybe [] duplicate . movement
      walk :: Step () -> LZipper a -> [LZipper a]
      walk direction = iterateMaybe (step direction)
    in LZipper (walk Up zipp) zipp (walk (Down ()) zipp)

example = fromIndexedL (2, [0..5])