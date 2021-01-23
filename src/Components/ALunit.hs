module ALunit () where

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

