module Types (Bit(..),Data,Byte,Ram(..),Circuit(..)) where

import qualified Control.Category as Cat

import Control.Arrow

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


data Bit = High | Low deriving Eq

instance Show Bit where
    show High = "1"
    show Low  = "0"

type Byte = (Bit,Bit,Bit,Bit,Bit,Bit,Bit,Bit)

-- Types for constructing components

type CarryIn  = Bit
type CarryOut = Bit

type Address = [Bit]
type Data    = [Bit]
type Write   = Bit

-- also keep track of the size of the ram a.k.a the legnth of the adresses
data Ram = Ram { unRam :: (Circuit (Address,Write,Data) Data),
                    -- (log_2 number of addresses,log_2 size of databus)
                 size :: (Int,Int)}


