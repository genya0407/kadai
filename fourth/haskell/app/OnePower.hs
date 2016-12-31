module Main where

import Numeric.LinearAlgebra
import Control.Monad

a :: Matrix Double
a = matrix 6 [
    2,  3,  4,  5,  6,  7,
    3,  8,  9, 10, 11, 12,
    4,  9, 13, 14, 15, 16,
    5, 10, 14, 17, 18, 19,
    6, 11, 15, 18, 20, 21,
    7, 12, 16, 19, 21, 22
  ]

u0 :: Vector Double
u0 = scalar (sqrt $ 1.0/6.0) * vector (take 6 $ repeat 1.0 )

epsilon :: Double
epsilon = 10 ^^ (-15)

err :: (Vector Double, Vector Double) -> Double
err (v1, v2) = abs $ gMax - gMin
  where
    elems = zip (toList v1) (toList v2)
    gMin = minimum $ map (\(e1, e2) -> e1/e2 ) elems
    gMax = maximum $ map (\(e1, e2) -> e1/e2 ) elems

-- 条件を満たさなかった要素も含めるtakeWhile
takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive cond (x:xs) = x : if cond x then takeWhileInclusive cond xs
                                               else []

main = do
  let
    _vs = iterate (\v -> a #> v ) u0
    errs = takeWhileInclusive (\e -> e > epsilon) $ map err $ zip _vs (tail _vs)
    (vs1, vs2) = unzip $ takeWhileInclusive (\e -> (err e) > epsilon) $ zip _vs (tail _vs)
    vs = vs1 ++ [last vs2]
  print errs
  forM_ vs $ \v -> do
    print $ norm_2 v
