{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import Control.Arrow
import Control.Monad

import qualified Control.Category as Cat
import Data.List

import Data.Maybe

type Bus8Bit = (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool)
type CarryIn = Bool
type CarryOut = Bool

newtype Circuit a b = Circuit { unCircuit :: a -> (Circuit a b, b) }

instance Cat.Category Circuit where
    id = Circuit $ \a -> (Cat.id, a)
    (.) = kdot
      where
        (Circuit cir2) `kdot` (Circuit cir1) = Circuit $ \a ->
            let (cir1', b) = cir1 a
                (cir2', c) = cir2 b
            in  (cir2' `kdot` cir1',c)

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

-- | Accumulator that outputs a value determined by the supplied function.
accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum acc f = Circuit $ \input ->
    let (output, acc') = input `f` acc
    in  (accum acc' f, output)

-- | Accumulator that outputs the accumulator value.
accum' :: b -> (a -> b -> b) -> Circuit a b
accum' acc f = accum acc (\a b ->  (a `f` b , a `f` b ))

total :: Num a => Circuit a a
total = accum' 0 (+)

-- Returns true the first time and then false afterwards
oneShot :: Circuit () Bool
oneShot = accum True $ \_ acc -> (acc, False)

-- nand gate
(~&&) :: Bool -> Bool -> Bool
(~&&) a b = not $ a && b

-- exclusive or
(!||) :: Bool -> Bool -> Bool
(!||) a b = (a || b) && (a ~&& b)

halfAdder :: Bool -> Bool -> (Bool, Bool)
halfAdder a b = (a !|| b, a && b)



notA :: Circuit Bool Bool
notA = Circuit $ \a -> (notA, not a)

andA :: Circuit (Bool,Bool) Bool
andA = Circuit $ \(a,b) -> (andA, a && b)

orA :: Circuit (Bool,Bool) Bool
orA = Circuit $ \(a,b) -> (orA, a || b)

norA = Circuit $ \(a,b) -> (norA, (a || b) && (not a && not b))

halfAdderA :: Circuit (Bool,Bool) (Bool, Bool)
halfAdderA = Circuit $ \(a,b) -> (halfAdderA, (a !|| b, a && b))

fullAdderA :: Circuit (Bool,Bool,Bool) (Bool,Bool)
fullAdderA = proc (carryIn,a,b) -> do
    (s1,co1) <- halfAdderA -< (a,b)
    (s2,co2) <- halfAdderA -< (carryIn,s1)
    carryOut <- orA -< (co1,co2)
    wire -< (s2,carryOut)

-- maybe can make this better by mapping over the tuple somehow
adder8BitA :: Circuit (CarryIn, Bus8Bit, Bus8Bit) (CarryOut, Bus8Bit)
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

splittedWire :: Circuit Bool (Bool,Bool)
splittedWire = (wire &&& wire)

nandGate :: Circuit (Bool,Bool) Bool
nandGate = Circuit $ \(a,b) -> (nandGate, not (a && b))

inverter :: Circuit Bool Bool
inverter = splittedWire >>> nandGate

parallelInverters :: Circuit (Bool,Bool) (Bool,Bool)
parallelInverters = inverter *** inverter

orGate :: Circuit (Bool,Bool) Bool
orGate = parallelInverters >>> nandGate

orGate' :: Circuit (Bool,Bool) Bool
orGate' = proc (a,b) -> do
    m1 <- nandGate -< (a,a)
    m2 <- nandGate -< (b,b)
    nandGate -< (m1,m2)

delay :: a -> Circuit a a
delay last = Circuit $ \this -> (delay this, last)



rsFlipFlop :: (Bool,Bool) -> Circuit (Bool,Bool) (Bool,Bool)
rsFlipFlop last = Circuit $ f where
    f (False,True)  = (rsFlipFlop (False,True), (False,True))
    f (True,False)  = (rsFlipFlop (True,False), (True,False))
    f (False,False) = (rsFlipFlop last, last)
    f (True,True)   = error "RS Flip Flop cannot have (True,True) as input"

--edge-triggered D type flip flop
-- can implement this using the latch by remembering the last input and output
--dFlipFlop

-- level triggered d type latch
ltDLatch :: Bool -> Circuit (Bool,Bool) Bool
ltDLatch last = Circuit $ f where
    f (False,_) = (ltDLatch last, last)
    f (True ,False)  = (ltDLatch False, False)
    f (True ,True)  = (ltDLatch True, True)

latch8Bit :: Circuit (Bool, Bus8Bit) Bus8Bit
latch8Bit = proc (latch,(d0,d1,d2,d3,d4,d5,d6,d7)) -> do
    q0 <- ltDLatch False -< (latch,d0)
    q1 <- ltDLatch False -< (latch,d1)
    q2 <- ltDLatch False -< (latch,d2)
    q3 <- ltDLatch False -< (latch,d3)
    q4 <- ltDLatch False -< (latch,d4)
    q5 <- ltDLatch False -< (latch,d5)
    q6 <- ltDLatch False -< (latch,d6)
    q7 <- ltDLatch False -< (latch,d7)
    returnA -< (q0,q1,q2,q3,q4,q5,q6,q7)

ffR :: Circuit (Bool,Bool) Bool
ffR = proc (s1,s2) -> do
    rec
        o1 <- norA -< (s1,common)
        o2 <- norA -< (s2,o1)
        (common,o3) <- splittedWire -< o2
    returnA -< o3

edgeDFlipFlop :: (Bool,Bool) -> Circuit Bool Bool
edgeDFlipFlop (lastClk,state) = Circuit $ f where
    f clk  = (edgeDFlipFlop (clk,newState clk),newState clk)
    newState clk = if lastClk && not clk then not state else state

halfFreq :: Circuit Bool Bool
halfFreq = proc inp -> do
    o1 <- edgeDFlipFlop (False,False) -< inp
    o2 <- edgeDFlipFlop (False,False) -< o1
    returnA -< o2

blend' = (foldr($)[].) . (.map(:)) . zipWith(.) . map(:)

altList n = take n $ blend' (repeat True) (repeat False)

--main :: IO Bool
main = do
    print $ runCircuit (delay 0) [5,6,7]
--    print $ runCircuit (rsFlipFlop (False,False)) $ [(True,False),(True,True),(False,False)]
--    print $ runCircuit ffR        $ [(True,False),(True,True),(False,False)]
    print $ runCircuit (edgeDFlipFlop (False,False)) $ [True,False,True,False,True,False,True,False]
    print $ runCircuit halfFreq $ altList 50
--    print $ runCircuit (edgeDFlipFlop False) $ [False,False,True,True,False,False,True,True]
--    print $ runCircuit notA [True,True,False]
--    print $ runCircuit andA [(True,False),(True,True),(False,False)]
    --let x = total Cat.. total
    --print $ runCircuit total [1,0,1,0,0,2]

