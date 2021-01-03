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

exF :: (a -> (b,c)) -> (a -> c)
exF f = (\x -> snd (f x)) where


rsFlipFlop :: Bool -> Circuit (Bool,Bool) Bool
rsFlipFlop last = Circuit $ f where
    f (False,True)  = (rsFlipFlop False, False)
    f (True,False)  = (rsFlipFlop True, True)
    f (False,False) = (rsFlipFlop last, last)
    f (True,True)   = error "RS Flip Flop cannot have (True,True) as input"

--main :: IO Bool
main = do
    print $ runCircuit (delay 0) [5,6,7]
    --print $ runCircuit (rsFlipFlop (False,True)) [(True,False),(True,False)]
    print $ runCircuit (rsFlipFlop False) [(True,False),(False,False),(False,True),(False,False),(True,False)]
--    print $ runCircuit notA [True,True,False]
--    print $ runCircuit andA [(True,False),(True,True),(False,False)]
    --let x = total Cat.. total
    --print $ runCircuit total [1,0,1,0,0,2]

