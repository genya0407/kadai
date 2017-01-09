module Main where

import           Control.Monad
import           Data.List (intersperse)
import qualified Integrator.Default as D
import qualified Integrator.Recursive as R
import           Types

main :: IO ()
main = do
  putStrLn "k,trapezoidal_default,trapezoidal_recursive,simpsons_default,simpsons_recursive"
  forM_ [0..25] $ \k -> do
    let
      n = 2 ^ k
      defaultTrapezoidal = D.compositTrapezoidalRule f19 n (0, 1)
      recursiveTrapezoidal = R.compositTrapezoidalRule f19 n (0, 1)
      defaultSimpsons = D.compositSimpsonsRule f19 n (0, 1)
      recursiveSimpsons = R.compositSimpsonsRule f19 n (0, 1)
    putStrLn $ concat $ intersperse "," $ (show k):(map (show . logE) [defaultTrapezoidal, recursiveTrapezoidal, defaultSimpsons, recursiveSimpsons])

logE ans = logBase 2 $ abs $ (ans - 0.05) / 0.05

f19 :: Double -> Double
f19 x = x ^ 19
