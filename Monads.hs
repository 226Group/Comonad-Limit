

-- conveyorList (a: ts@(b:_)) = Conveyor (a -> b) conveyorList ts
type TickerT m a = StateT Nat m a
type Ticker a = TickerT Identity a

tick :: Monad m => TickerT m Nat
tick = do
  i <- get
  modify succ
  return i

iterateM :: forall m a b. Monad m => (a -> m a) -> a -> m [a]
iterateM f x = sequence $ iterate @(m a) (>>= f) (return @m x)

iterateMaybeM :: Monad m => (a -> m (Maybe a)) -> a -> m [a]
iterateMaybeM f x = sequence $ iterate (>>= f) (return x)

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

-- m MonadTransf
farthestM :: forall m a. (Monad m, Traversable m) => (a -> Maybe (m a)) -> a -> m a
farthestM f = farthest (>>= f) . return
--farthest @(m a) ( fmap swapMonads (>>=(fmap swapMonads f :: a -> m (Maybe a)))) . return  --i wrote this, but i dont understand this
  where
  -- f1 :: a -> m (n a)
  heavyMonadery :: (Monad n, Traversable n, Monad m) => m (n (m a)) -> n (m a)
  heavyMonadery = fmap join . swapMonads

tickedFarthest :: (a -> Maybe a) -> a -> (a, Ind)
tickedFarthest = flip fmap2 iterateMaybe $ swap . last . indexes