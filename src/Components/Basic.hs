{-# LANGUAGE Arrows #-}
module Components.Basic ((&&),not,(!||),splitHalf) where

import qualified Control.Category as Cat

import Control.Arrow
import Prelude hiding (Bool(..),not,(&&),(||))
import Types

splitHalf :: [a] -> ([a],[a])
splitHalf l = splitAt ((length l + 1) `div` 2) l

blend' :: [a] -> [a] -> [a]
blend'    = (foldr($)[].) . (.map(:)) . zipWith(.) . map(:)

altList :: Int -> [Bit]
altList n = take n $ blend'  (repeat Low) (repeat High)

not :: Bit -> Bit
not High = Low
not Low = High

(&&) :: Bit -> Bit -> Bit
(&&) High High = High
(&&) _ _ = Low

(||) :: Bit -> Bit -> Bit
(||) High _    = High
(||) _    High = High
(||) _    _    = Low

-- nand gate
(~&&) :: Bit -> Bit -> Bit
(~&&) a b = not $ a && b

-- exclusive or
(!||) :: Bit -> Bit -> Bit
(!||) a b = (a || b) && (a ~&& b)

norA = Circuit $ \(a,b) -> (norA, (a || b) && (not a && not b))

wire :: Circuit a a
wire = Cat.id

splittedWire :: Circuit Bit (Bit,Bit)
splittedWire = (wire &&& wire)

nandGate :: Circuit (Bit,Bit) Bit
nandGate = Circuit $ \(a,b) -> (nandGate, not (a && b))

inverter :: Circuit Bit Bit
inverter = splittedWire >>> nandGate

parallelInverters :: Circuit (Bit,Bit) (Bit,Bit)
parallelInverters = inverter *** inverter

orGate :: Circuit (Bit,Bit) Bit
orGate = parallelInverters >>> nandGate

orGate' :: Circuit (Bit,Bit) Bit
orGate' = proc (a,b) -> do

    m1 <- nandGate -< (a,a)
    m2 <- nandGate -< (b,b)

    nandGate -< (m1,m2)

