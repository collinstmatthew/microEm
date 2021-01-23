module Circuit (runCircuit) where

import qualified Control.Category as Cat
import Control.Arrow
import Types

runCircuit :: Circuit a b -> [a] -> [b]
runCircuit _ [] = []
runCircuit cir (x:xs) =
    let (cir',x')= unCircuit cir x
    in x' : runCircuit cir' xs

