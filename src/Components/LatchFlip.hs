{-# LANGUAGE Arrows #-}

module Components.LatchFlip (latch) where

import Prelude hiding (Bool(..),not,(&&),(||))
import Control.Arrow
import Types
import Components.Basic


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

