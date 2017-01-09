module Main where

import           Control.Monad
import           Data.List (intersperse)
import qualified Seq as S
import qualified Func as F
import           Types

main :: IO ()
main = do
  putStrLn "k,trapezoidal_seq,trapezoidal_func,simpsons_seq,simpsons_func"
  forM_ [0..20] $ \k -> do
    let
      n = 2 ^ k
      seqTrapezoidal = S.compositTrapezoidalRule f19 n (0, 1)
      funcTrapezoidal = F.compositTrapezoidalRule f19 n (0, 1)
      seqSimpsons = S.compositSimpsonsRule f19 n (0, 1)
      funcSimpsons = F.compositSimpsonsRule f19 n (0, 1)
    putStrLn $ concat $ intersperse "," $ (show k):(map show $ map f19errorLog [seqTrapezoidal, funcTrapezoidal, seqSimpsons, funcSimpsons])

f19errorLog ans = logBase 2 $ abs $ (ans - 0.05) / 0.05

f19 :: Double -> Double
f19 x = x ^ 19