module Main where

import           Control.Monad
import           Data.List (intersperse)
import qualified Seq as S
import qualified Func as F
import           Types

main :: IO ()
main = do
  putStrLn "k,trapezoidal_default,trapezoidal_recursive,simpsons_default,simpsons_recursive"
  forM_ [0..25] $ \k -> do
    let
      n = 2 ^ k
      defaultTrapezoidal = S.compositTrapezoidalRule f19 n (0, 1)
      recursiveTrapezoidal = F.compositTrapezoidalRule f19 n (0, 1)
      defaultSimpsons = S.compositSimpsonsRule f19 n (0, 1)
      recursiveSimpsons = F.compositSimpsonsRule f19 n (0, 1)
    putStrLn $ concat $ intersperse "," $ (show k):(map show $ map f19errorLog [defaultTrapezoidal, recursiveTrapezoidal, defaultSimpsons, recursiveSimpsons])

f19errorLog ans = logBase 2 $ abs $ (ans - 0.05) / 0.05

f19 :: Double -> Double
f19 x = x ^ 19
