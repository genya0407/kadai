module Main where

import           Control.Monad (forM_)
import           Data.List (intercalate)
import qualified Seq as S

main :: IO ()
main = do
  putStrLn "k,trapezoidal,simpsons" -- CSVのヘッダ
  forM_ [0..20] $ \k -> do -- k = 0,1,2 .., 20について、積分を計算する
    let
      n = 2 ^ k
      trapezoidal = S.compositTrapezoidalRule f19 n (0, 1) -- 複合台形公式で積分を計算
      simpsons = S.compositSimpsonsRule f19 n (0, 1) -- 複合シンプソン公式で積分を計算
    -- CSVの行を合成して出力
    putStrLn $ intercalate "," $ (show k):(map (show . logE) [trapezoidal, simpsons])

-- 積分の結果から、その相対誤差の絶対値の対数を計算する
logE :: Double -> Double
logE ans = logBase 2 $ abs $ (ans - 0.05) / 0.05

-- 積分対象の関数
f19 :: Double -> Double
f19 x = x ^ 19
