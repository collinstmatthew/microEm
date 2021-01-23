{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}          -- for forall
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude hiding (Bool(..),not,(&&),(||))

import qualified Control.Category as Cat

import Control.Arrow
import Control.Monad

import Data.List
import Data.Maybe

import Control.Lens.Tuple
import Control.Lens.Setter
import Control.Lens.Operators

import Types
import Circuit(runCircuit)
import Components.Memory(buildRam)

--main :: IO Bit
main = do

    let testData' = [([High,High,Low,Low],High,[High]),
                     ([Low,Low,Low,Low],Low,[High]),
                     ([High,Low,Low,Low],Low,[High]),
                     ([Low,High,Low,Low],Low,[High]),
                     ([High,High,Low,Low],Low,[Low])
                    ]

    let z = buildRam (6,6)
    print $ size z

