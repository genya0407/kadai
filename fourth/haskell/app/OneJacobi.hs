module Main where

import Numeric.LinearAlgebra

main = do
  print $ p a

a :: Matrix Double
a = matrix 6 [
    2,  3,  4,  5,  6,  7,
    3,  8,  9, 10, 11, 12,
    4,  9, 13, 14, 15, 16,
    5, 10, 14, 17, 18, 19,
    6, 11, 15, 18, 20, 21,
    7, 12, 16, 19, 20, 22
  ]

_upper = matrix 6 [
    0, 1, 1, 1, 1, 1,
    0, 0, 1, 1, 1, 1,
    0, 0, 0, 1, 1, 1,
    0, 0, 0, 0, 1, 1,
    0, 0, 0, 0, 0, 1,
    0, 0, 0, 0, 0, 0
  ]
_lower = matrix 6 $ (reverse . concat . toLists) _upper

upper m = m * _upper
lower m = m * _lower

p :: Matrix Double -> Matrix Double
p m = accum (ident 6) (\value _ -> value) modifiers
  where
    uptr = upper m
    (_p, _q) = maxIndex uptr
    apq = atIndex m (_p, _q)
    app = atIndex m (_p, _p)
    aqq = atIndex m (_q, _q)
    theta = 0.5 * atan (- 2.0 * apq / (app - aqq))
    modifiers = [((_p,_p), cos theta), ((_p,_q), sin theta), ((_q, _p), - (sin theta)), ((_q, _q), cos theta)]
