module Main where

import           Three (calc)
import qualified Seq as S

-- 複合シンプソン公式によってπを求める
main = calc S.compositSimpsonsRule
