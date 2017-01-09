module Main where

import           Three (calc)
import qualified Seq as S

-- 複合台形公式によってπを求める
main = calc S.compositTrapezoidalRule
