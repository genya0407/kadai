module Main where

import           Control.Monad
import qualified Seq as S
import           Types

main = do
  putStrLn $ "name, f1, f2, f3, f4"
  forM_ [("trapezoidal", S.compositTrapezoidalRule), ("simpsons", S.compositSimpsonsRule)] $ \(name, integrator) -> do
    putStr name
    forM_ [f1, f2, f3, f4] $ \f -> do
      putStr ","
      putStr $ let ans = f integrator (2 ^ 25) in show (logBase 2 (abs $ ans - pi))
    putStrLn ""

f1 :: Integrator -> Integer -> Double
f1 integrator n = 4.0 * int
  where
    int = integrator (\x -> 1/(1 + x ^ 2)) n (0, 1)

f2 :: Integrator -> Integer -> Double
f2 integrator n = 4.0 * int
  where
    int = integrator (\x -> sqrt (1.0 - x ^ 2)) n (0, 1)

f3 :: Integrator -> Integer -> Double
f3 integrator n = 12.0 * ( - (sqrt 3.0)/8.0 + int)
  where
    int = integrator (\x -> sqrt (1.0 - x ^ 2)) n (0, 0.5)

f4 :: Integrator -> Integer -> Double
f4 integrator n = 6.0 * ((sqrt 3.0)/8.0 + int)
  where
    int = integrator (\x -> sqrt (1.0 - x ^ 2)) n (0.5, 1)
