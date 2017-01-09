module Main where

import Numeric.LinearAlgebra
import Control.Monad

-- 問の行列A
a :: Matrix Double
a = matrix 6 [
    2,  3,  4,  5,  6,  7,
    3,  8,  9, 10, 11, 12,
    4,  9, 13, 14, 15, 16,
    5, 10, 14, 17, 18, 19,
    6, 11, 15, 18, 20, 21,
    7, 12, 16, 19, 21, 22
  ]

-- 初期値u_0
u0 :: Vector Double
u0 = scalar (sqrt $ 1.0/6.0) * vector (take 6 $ repeat 1.0 )

-- 収束条件に用いるεの値
epsilon :: Double
epsilon = 10 ^^ (-15)

-- 収束判定に用いるスカラ地を返す関数
-- ２つのVectorについて、γ_minとγ_maxを求め、その差の絶対値を返す
diff :: (Vector Double, Vector Double) -> Double
diff (v1, v2) = abs $ gMax - gMin
  where
    elems = zip (toList v1) (toList v2)
    gMin = minimum $ map (\(e1, e2) -> e1/e2 ) elems
    gMax = maximum $ map (\(e1, e2) -> e1/e2 ) elems

-- 条件を満たさなかった要素も含めるtakeWhile
-- 引数のリストの頭から条件関数に当てはめてみて、条件関数がTrueを返すうちは返り値に含める
-- 条件関数がFalseを返したとき、その要素を含めたリストを返り値として返す
takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive cond (x:xs) = x : if cond x then takeWhileInclusive cond xs
                                               else []

-- 収束していればTrueを、収束していなければFalseを返す
convergent :: (Vector Double, Vector Double) -> Bool
convergent (v1, v2) = diff (v1, v2) <= epsilon

main = do
  let
    -- _vs = [u0, a #> u0, a #> (a #> u0), ...]
    _vs = iterate (\v -> a #> v ) u0

    -- untilConvergent = [(v1, v2), (v2, v3), .. , (vl, vm), (vm, vn)]
    -- また、diff (vl, vm) > εであるが、diff (vm, vn) <= εである
    untilConvergent = takeWhileInclusive (not . convergent) $ zip _vs (tail _vs)

    -- vs1 = [v1, v2, v3, ..., vm]
    -- vs2 = [v2, v3, v4, ..., vn]
    (vs1, vs2) = unzip untilConvergent
    -- vs = [v1, v2, v3, ..., vm, vn]
    vs = (head vs1):vs2

    -- diffs = [diff (v1, v2), diff (v2, v3), ..., diff (vm, vn)]
    diffs = map diff untilConvergent

  putStrLn "u1:"
  print (last vs) -- u1を出力
  putStrLn "収束判定に用いたスカラ値："
  print diffs -- 収束判定に用いたスカラ値のリストを出力
