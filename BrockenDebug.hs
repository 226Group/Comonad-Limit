module Debug where
import System.IO.Unsafe

{-# NOINLINE trace #-}
trace :: String -> a -> a
trace s = unsafePerformIO (putStrLn s) `seq` id

traceShow :: Show a => a -> b -> b
traceShow = trace . show