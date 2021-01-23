module Components.SelDec (decoder3to8,selector8to1,decoder1to2,selector2to1) where

import Circuit
import Types
import Control.Arrow


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


