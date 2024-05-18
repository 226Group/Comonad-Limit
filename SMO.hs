{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module SMO where
import Base
import Limit
import Data.Maybe

instance Extract (ProbDistrib a) [(P, a)] where
  run (ProbDistribConstr xs) = xs
instance Extract RejectSMO SMOdata where
  run (RejectSMO d) = d
instance Extract WaitSMO SMOdata where
  run (WaitSMO d) = d

  -- let a = 0.5; am = 4
  -- -- print $ 7^2
  -- print $ a^am
  -- print [(a^n) / fromIntegral (factorial n) | n<-[0..am]]
  -- -- putStrLn $ trace "hi" "hello"
  -- -- print $ factorial <$> [0..5]


-- data UniformList
type P = Double --a = Fractional a => a
inv :: P -> P
inv = (1 - )
--Random a
newtype ProbDistrib a = ProbDistribConstr [(P, a)]
  deriving (Foldable, Traversable, Eq)
instance Functor ProbDistrib where
  fmap f (ProbDistribConstr xs) = ProbDistribConstr $ mapsnd f <$> xs

instance Show a => Show (ProbDistrib a) where
  show (ProbDistribConstr xs) = "ProbDistrib " ++ show xs

probDistrib :: [(P, a)] -> ProbDistrib a
probDistrib = let
  check_prob :: Double -> P
  check_prob = check (\x -> x >= 0 && x <= 1) "not probability"
  in ProbDistribConstr . check (convergeTo 1 . limSum . map fst) "doesn't add up to 1" .
    map (mapfst check_prob)

exp_value :: forall a. Real a => ProbDistrib a -> Double
exp_value = fromJust . converge . limSum . map (uncurry (*)) . run @(ProbDistrib Double) . fmap realToFrac

-- SMOtype : (a -> SMO) -> a -> 
-- Система Массового Обслуживания : a -> RandomT State [Bool] a
data SMOdata = SMO {services::Nat, avr_service_time::Double, avr_arriving_time :: Double}
  deriving (Show, Eq)
  --S0 -> S1 -> S2 ...
freq = (1/)
freq_service :: SMOdata -> Double
freq_service = freq . avr_service_time
freq_receipts :: SMOdata -> Double
freq_receipts = freq . avr_arriving_time
-- newtype WaitSMO = InfSMO {runInfSMO :: SMOdata }
newtype RejectSMO = RejectSMO SMOdata
  deriving (Show, Eq)
newtype WaitSMO = WaitSMO SMOdata
  deriving (Show, Eq)

class Stochastic system where
  type St system :: *
  --RandomT State s a -> ProbDistrib s
  state_prob :: system -> ProbDistrib (St system)

instance Stochastic RejectSMO where
  type St RejectSMO = Nat
  state_prob (RejectSMO syst) =
    let p0 = 1/sum [(a^n) / fromIntegral (factorial n) | n<-[0..services syst]]
    in ProbDistribConstr [(p0 * a^n/ fromIntegral (factorial n), n) | n <- [0..services syst]]
    where a = freq_receipts syst / freq_service syst

rejP :: RejectSMO -> P
rejP = fst . last . run  @(ProbDistrib Nat) . state_prob

servP :: RejectSMO -> P
servP = inv . rejP

throughput :: RejectSMO -> Double
throughput = lift2A (*) servP (freq_receipts . run)

avr_state_rejSMO :: RejectSMO -> Double
avr_state_rejSMO syst = throughput syst / (freq_service $ run $ syst)
--throughput `lift2A (/)` (freq_service . run)

avr_state :: (Stochastic system, Real a, St system ~ a) => system -> Double
avr_state = exp_value . state_prob

--wrong
instance Stochastic WaitSMO where
  type St WaitSMO = Nat
  state_prob (WaitSMO syst) =
    let p0 = 1/sum [(a^n) / fromIntegral (factorial n) | n<-[0..services syst]]
    in ProbDistribConstr [(p0 * a^n/ fromIntegral (factorial n), n) | n <- [0..services syst]]
    where a = freq_receipts syst / freq_service syst
