module Main where

import           Three (calc)
import           Integrator.Default (compositSimpsonsRule)

-- 複合シンプソン公式によってπを求める
main = calc compositSimpsonsRule
