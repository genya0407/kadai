module Main where

import Numeric.LinearAlgebra
import Control.Monad

main = do
  let 
    -- untilConvergent = [(A_0, P_0), (A_1, P_1), .., (A_n, P_n), (A_m, P_m)]
    -- ただし、A_nは収束条件を満たさないが、A_mは収束条件を満たす
    untilConvergent = takeWhileInclusive (not . convergent) $ iterate yield (a, p a)

    -- as = [A_0, A_1, .., A_m]
    -- ps = [P_0, P_1, .., P_m]
    (as, ps) = unzip untilConvergent 

    lambda = last as -- 対角要素がAの固有値である行列
    u = foldr (<>) (ident 6) ps -- 固有ベクトルをまとめた行列
  putStrLn "UU^(T):"
  print $ u <> (tr' u) -- UU^(T)を出力する
  putStrLn "UΛU^(T):"
  print $ u <> lambda <> (tr' u) -- UΛU^(T)を出力する

-- わかりやすくするため、型に別名をつける
type A = Matrix Double
type P = Matrix Double

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

-- (A_n, P_n)を受け取り、(A_n+1, P_n+1)を計算する関数
yield :: (A, P) -> (A, P)
yield (a0, p0) = (a1, p1)
  where
    a1 = tr' p0 <> a0 <> p0
    p1 = p a1

-- AとPのタプルを受け取り、Aが収束条件をみたすときTrueを、満たさないときFalseを返す関数
-- Aの非対角成分の絶対値の内、最大のものが10^(-8)以下であれば、収束したとみなす
convergent :: (A, P) -> Bool
convergent (m, _) = 10 ^^ (-8) >= (maxElement . (cmap abs) $ notdiag m)

-- 条件を満たさなかった要素も含めるtakeWhile
takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive cond (x:xs) = x : if cond x then takeWhileInclusive cond xs
                                               else []

-- _upper, _lower, _notdiagは、upper関数とnotdiag関数の内部で使う行列
_upper = matrix 6 [
    0, 1, 1, 1, 1, 1,
    0, 0, 1, 1, 1, 1,
    0, 0, 0, 1, 1, 1,
    0, 0, 0, 0, 1, 1,
    0, 0, 0, 0, 0, 1,
    0, 0, 0, 0, 0, 0
  ]
_lower = tr' _upper
_notdiag = _upper + _lower

-- 行列の"上三角部分"を取り出す関数
upper m = m * _upper
-- 行列の非対角成分を取り出す関数
notdiag m = m * _notdiag

-- 引数の行列の"上三角部分"の要素のうち、絶対値が最大のものをゼロにする直交変換行列を求める関数
-- accumに関しては、次を参照のこと:
--   https://hackage.haskell.org/package/hmatrix-0.18.0.0/docs/Numeric-LinearAlgebra-Data.html#v:accum
p :: A -> P
p m = accum (ident 6) (\value _ -> value) modifiers 
  where
    uptr = upper m -- "上三角部分"を取り出す
    (_p, _q) = maxIndex $ (cmap abs) uptr -- uptrの各要素の絶対値を取り、その中で最大の要素の添字を取得する
    apq = atIndex m (_p, _q) -- (_p, _q)要素を取り出す
    app = atIndex m (_p, _p) -- (_p, _p)要素を取り出す
    aqq = atIndex m (_q, _q) -- (_q, _q)要素を取り出す
    theta = 0.5 * atan (- 2.0 * apq / (app - aqq)) -- θを計算する
    -- modifierは、「単位行列のうち、この添字の要素をこの値に入れ替えれば、それがPになる」というような、
    -- (添字, 値)のタプルのリストである
    modifiers = [((_p,_p), cos theta), ((_p,_q), sin theta), ((_q, _p), - (sin theta)), ((_q, _q), cos theta)]
