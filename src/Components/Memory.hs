{-# LANGUAGE Arrows #-}

module Components.Memory (buildRam) where

import Control.Arrow
import Control.Lens.Tuple
import Control.Lens.Setter
import Control.Lens.Operators


import Components.SelDec (decoder3to8,selector8to1,decoder1to2,selector2to1)
import Components.LatchFlip (latch)
import Components.Basic(splitHalf)
import Types


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
