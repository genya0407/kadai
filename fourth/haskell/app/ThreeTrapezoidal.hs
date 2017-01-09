module Main where

import           Three (calc)
import           Integrator.Default (compositTrapezoidalRule)

-- 複合台形公式によってπを求める
main = calc compositTrapezoidalRule
