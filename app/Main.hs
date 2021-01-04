{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (Bool(..),not,(&&),(||))

import Control.Arrow
import Control.Monad

import qualified Control.Category as Cat
import Data.List

import Data.Maybe


data Bit = High | Low deriving (Eq)

instance Show Bit where
    show High = "1"
    show Low  = "0"


type Byte = (Bit,Bit,Bit,Bit,Bit,Bit,Bit,Bit)


type CarryIn = Bit
type CarryOut = Bit

newtype Circuit a b = Circuit { unCircuit :: a -> (Circuit a b, b) }

instance Cat.Category Circuit where
    id = Circuit $ \a -> (Cat.id, a)
    (.) = dot
      where
        (Circuit cir2) `dot` (Circuit cir1) = Circuit $ \a ->
            let (cir1', b) = cir1 a
                (cir2', c) = cir2 b
            in  (cir2' `dot` cir1',c)

instance Arrow Circuit where
    arr f = Circuit $ \a -> (arr f, f a)
    first (Circuit cir) = Circuit $ \(b,d) ->
        let (cir', c) = cir b
        in (first cir', (c, d))

instance ArrowLoop Circuit where
    loop (Circuit cir) = Circuit $ \b ->
        let (cir', (c,d)) = cir (b,d)
        in  (loop cir', c)

runCircuit :: Circuit a b -> [a] -> [b]
runCircuit _ [] = []
runCircuit cir (x:xs) =
    let (cir',x')= unCircuit cir x
    in x' : runCircuit cir' xs

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

halfAdder :: Bit -> Bit -> (Bit, Bit)
halfAdder a b = (a !|| b, a && b)

halfAdderA :: Circuit (Bit,Bit) (Bit, Bit)
halfAdderA = Circuit $ \(a,b) -> (halfAdderA, (a !|| b, a && b))

fullAdderA :: Circuit (Bit,Bit,Bit) (Bit,Bit)
fullAdderA = proc (carryIn,a,b) -> do
    (s1,co1) <- halfAdderA -< (a,b)
    (s2,co2) <- halfAdderA -< (carryIn,s1)
    carryOut <- arr (uncurry (||)) -< (co1,co2)
    wire -< (s2,carryOut)

-- maybe can make this better by mapping over the tuple somehow
adder8BitA :: Circuit (CarryIn, Byte, Byte) (CarryOut, Byte)
adder8BitA = proc (carryIn,(a0,a1,a2,a3,a4,a5,a6,a7),(b0,b1,b2,b3,b4,b5,b6,b7)) -> do
    (s0,co0) <- fullAdderA -< (carryIn, a0, b0)
    (s1,co1) <- fullAdderA -< (co0, a1, b1)
    (s2,co2) <- fullAdderA -< (co1, a2, b2)
    (s3,co3) <- fullAdderA -< (co2, a2, b2)
    (s4,co4) <- fullAdderA -< (co3, a2, b2)
    (s5,co5) <- fullAdderA -< (co4, a2, b2)
    (s6,co6) <- fullAdderA -< (co5, a2, b2)
    (s7,co7) <- fullAdderA -< (co6, a2, b2)
    wire   -< (co7,(s0,s1,s2,s3,s4,s5,s6,s7))

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

delay :: a -> Circuit a a
delay last = Circuit $ \this -> (delay this, last)

rsFlipFlop :: (Bit,Bit) -> Circuit (Bit,Bit) (Bit,Bit)
rsFlipFlop last = Circuit $ f where
    f (Low,High)  = (rsFlipFlop (Low,High), (Low,High))
    f (High,Low)  = (rsFlipFlop (High,Low), (High,Low))
    f (Low,Low)   = (rsFlipFlop last, last)
    f (High,High) = error "RS Flip Flop cannot have (High,High) as input"

-- level triggered d type latch
ltDLatch :: Bit -> Circuit (Bit,Bit) Bit
ltDLatch last = Circuit $ f where
    f (Low ,_)    = (ltDLatch last, last)
    f (High,Low)  = (ltDLatch Low, Low)
    f (High,High) = (ltDLatch High, High)

latch8Bit :: Circuit (Bit, Byte) Byte
latch8Bit = proc (latch,(d7,d6,d5,d4,d3,d2,d1,d0)) -> do
    q0 <- ltDLatch Low -< (latch,d0)
    q1 <- ltDLatch Low -< (latch,d1)
    q2 <- ltDLatch Low -< (latch,d2)
    q3 <- ltDLatch Low -< (latch,d3)
    q4 <- ltDLatch Low -< (latch,d4)
    q5 <- ltDLatch Low -< (latch,d5)
    q6 <- ltDLatch Low -< (latch,d6)
    q7 <- ltDLatch Low -< (latch,d7)
    returnA -< (q7,q6,q5,q4,q3,q2,q1,q0)

edgeDFlipFlop :: (Bit,Bit) -> Circuit Bit Bit
edgeDFlipFlop (lastClk,state) = Circuit $ f where
    f clk  = (edgeDFlipFlop (clk,newState clk),newState clk)
    --newState clk = if lastClk && not clk then not state else state
    newState clk = (lastClk && not clk) !|| state

rippleCounter8Bit :: Circuit Bit Byte
rippleCounter8Bit = proc inp -> do
    q1 <- edgeDFlipFlop (Low,Low) -< inp
    q2 <- edgeDFlipFlop (Low,Low) -< q1
    q3 <- edgeDFlipFlop (Low,Low) -< q2
    q4 <- edgeDFlipFlop (Low,Low) -< q3
    q5 <- edgeDFlipFlop (Low,Low) -< q4
    q6 <- edgeDFlipFlop (Low,Low) -< q5
    q7 <- edgeDFlipFlop (Low,Low) -< q6
    returnA -< (q7,q6,q5,q4,q3,q2,q1,inp)

blend' = (foldr($)[].) . (.map(:)) . zipWith(.) . map(:)

altList n = take n $ blend'  (repeat Low) (repeat High)

--main :: IO Bit
main = do
    print $ runCircuit (delay 0) [5,6,7]
    print $ "finished"
    let x = runCircuit rippleCounter8Bit  $ altList 256
    mapM_ (putStrLn . show) x

