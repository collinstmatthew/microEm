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

data Bit = High | Low deriving Eq

instance Show Bit where
    show High = "1"
    show Low  = "0"

type Byte = (Bit,Bit,Bit,Bit,Bit,Bit,Bit,Bit)

-- Types for constructing components

type CarryIn  = Bit
type CarryOut = Bit

-- # TODO change this to a list
type Address = [Bit]
-- # TODO make the data out into a list as well
type Data    = [Bit]
type Write   = Bit

newtype Circuit a b = Circuit { unCircuit :: a -> (Circuit a b, b) }

-- also keep track of the size of the ram a.k.a the legnth of the adresses
data Ram = Ram { unRam :: (Circuit (Address,Write,Data) Data),
                    -- (log_2 number of addresses,log_2 size of databus)
                 size :: (Int,Int)}

splitHalf :: [a] -> ([a],[a])
splitHalf l = splitAt ((length l + 1) `div` 2) l

blend' :: [a] -> [a] -> [a]
blend'    = (foldr($)[].) . (.map(:)) . zipWith(.) . map(:)

altList :: Int -> [Bit]
altList n = take n $ blend'  (repeat Low) (repeat High)

-- build specified size of the ram
buildRam :: (Int,Int) -> Ram
buildRam (addressN,busN) | addressN < 3 = error "number of ram addresses can't < 8"
                         | otherwise    = expandData where
                            expandAddress = (iterate doubleRamAddress ram8t1)!!(addressN-3)
                            expandData    = (iterate doubleRamBus expandAddress)!!(busN-1)


-- This doubles the number of addresses that are held in the ram
doubleRamAddress :: Ram -> Ram
doubleRamAddress ram = Ram circuit $ size ram & _1 +~ 1 where
    circuit = proc ((aNew:address),write,dat) -> do
        -- a new is the new adress input
        -- check the size of the list is correct by throwing an error if not
        (do1,do0) <- decoder1to2  -< (aNew,dat)
        d1        <- unRam ram    -< (address,write,do1)
        d0        <- unRam ram    -< (address,write,do0)
        dataOut   <- selector2to1 -< (aNew,(d0,d1))

        returnA -< dataOut

-- could always build up these ram recusively by calling smaller ram I guess?
doubleRamBus :: Ram -> Ram
doubleRamBus ram = Ram circuit $ size ram & _2 +~ 1 where
    circuit = proc (address,write,dat) -> do
        let (dat1,dat2) = splitHalf dat

        out1 <- unRam ram -< (address,write,dat1)
        out2 <- unRam ram -< (address,write,dat2)

        returnA -< (out1 ++ out2)

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

    (s1,co1) <- halfAdderA         -< (a,b)
    (s2,co2) <- halfAdderA         -< (carryIn,s1)
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

    wire -< (co7,(s0,s1,s2,s3,s4,s5,s6,s7))

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
latch :: Bit -> Circuit (Bit,Bit) Bit
latch last = Circuit $ f where
    -- (write, data)
    f (Low ,_)    = (latch last, last)
    f (High,Low)  = (latch Low, Low)
    f (High,High) = (latch High, High)

latch8Bit :: Circuit (Bit, Byte) Byte
latch8Bit = proc (write,(d7,d6,d5,d4,d3,d2,d1,d0)) -> do

    q0 <- latch Low -< (write,d0)
    q1 <- latch Low -< (write,d1)
    q2 <- latch Low -< (write,d2)
    q3 <- latch Low -< (write,d3)
    q4 <- latch Low -< (write,d4)
    q5 <- latch Low -< (write,d5)
    q6 <- latch Low -< (write,d6)
    q7 <- latch Low -< (write,d7)

    returnA -< (q7,q6,q5,q4,q3,q2,q1,q0)

edgeDFlipFlop :: (Bit,Bit) -> Circuit Bit Bit
edgeDFlipFlop (lastClk,state) = Circuit $ f where
    f        = \clk -> (edgeDFlipFlop (clk,newState clk),newState clk)
    newState = \clk -> (lastClk && not clk) !|| state

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

-- Is it possible to just define Selector 2 1
-- and then automatically generate the component
selector2to1 :: Circuit (Bit,(Data,Data)) Data
selector2to1 = arr $ f where
    f (Low ,(d0,_)) = d0
    f (High,(_,d1)) = d1

selector8to1 :: Circuit ((Bit,Bit,Bit),Byte) Bit
selector8to1 = arr $ f where
--    ( s2, s1,   s0)  ,(d7,d6,d5,d4,d3,d2,d1,d0)
    f ((Low ,Low ,Low) ,(_,_,_,_,_,_,_,d0)) = d0
    f ((Low ,Low ,High),(_,_,_,_,_,_,d1,_)) = d1
    f ((Low ,High,Low) ,(_,_,_,_,_,d2,_,_)) = d2
    f ((Low ,High,High),(_,_,_,_,d3,_,_,_)) = d3
    f ((High,Low ,Low) ,(_,_,_,d4,_,_,_,_)) = d4
    f ((High,Low ,High),(_,_,d5,_,_,_,_,_)) = d5
    f ((High,High,Low) ,(_,d6,_,_,_,_,_,_)) = d6
    f ((High,High,High),(d7,_,_,_,_,_,_,_)) = d7

--(select, data)-> (d1,d0)
decoder1to2 :: Circuit (Bit,Data) (Data,Data)
decoder1to2 = arr $ f where
    f (Low,dat)  = (replicate (length dat) Low,dat)
    f (High,dat) = (dat,replicate (length dat) Low)

decoder3to8 :: Circuit ((Bit,Bit,Bit),Bit) Byte
decoder3to8 = arr $ f where
--  f ((s2  ,s1  ,s0)  ,data)= (o7 ,o6 ,o5 ,o4 ,o3 ,o2 ,o1 ,o0)
    f ((Low ,Low ,Low) ,dat) = (Low,Low,Low,Low,Low,Low,Low,dat)
    f ((Low ,Low ,High),dat) = (Low,Low,Low,Low,Low,Low,dat,Low)
    f ((Low ,High,Low) ,dat) = (Low,Low,Low,Low,Low,dat,Low,Low)
    f ((Low ,High,High),dat) = (Low,Low,Low,Low,dat,Low,Low,Low)
    f ((High,Low ,Low) ,dat) = (Low,Low,Low,dat,Low,Low,Low,Low)
    f ((High,Low ,High),dat) = (Low,Low,dat,Low,Low,Low,Low,Low)
    f ((High,High,Low) ,dat) = (Low,dat,Low,Low,Low,Low,Low,Low)
    f ((High,High,High),dat) = (dat,Low,Low,Low,Low,Low,Low,Low)

ram8t1 :: Ram
ram8t1 = Ram circuit (3,1) where
            circuit = proc ([a2,a1,a0],write,[dat]) -> do
                (o7,o6,o5,o4,o3,o2,o1,o0) <- decoder3to8  -< ((a2,a1,a0),write)
                d0 <- latch Low -< (o0,dat)
                d1 <- latch Low -< (o1,dat)
                d2 <- latch Low -< (o2,dat)
                d3 <- latch Low -< (o3,dat)
                d4 <- latch Low -< (o4,dat)
                d5 <- latch Low -< (o5,dat)
                d6 <- latch Low -< (o6,dat)
                d7 <- latch Low -< (o7,dat)
                dataOut <-  selector8to1 -< ((a2,a1,a0),(d7,d6,d5,d4,d3,d2,d1,d0))
                returnA -< [dataOut]


--main :: IO Bit
main = do

    let testData' = [([High,High,Low,Low],High,[High]),
                     ([Low,Low,Low,Low],Low,[High]),
                     ([High,Low,Low,Low],Low,[High]),
                     ([Low,High,Low,Low],Low,[High]),
                     ([High,High,Low,Low],Low,[Low])
                    ]

    let y = runCircuit (unRam (doubleRamAddress ram8t1)) testData'
    let expectedResult = [[High],[Low],[Low],[Low],[High]]
    print $ y == expectedResult
    let z = buildRam (6,6)
    print $ size z

