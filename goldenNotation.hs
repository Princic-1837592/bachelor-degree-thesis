-- https://users.dimi.uniud.it/~pietro.digianantonio/papers/copy_pdf/golden.pdf



-- simplified notation stream
type SNStream = [Int]

{-
full notation stream

(z, a) = (-1 + Σ ai * phi^-i) * phi^2z

ho preferito usare una coppia invece che un unico stream
perché fa capire meglio la differenza tra le due parti
-}
type FNStream = (Int, SNStream)

approx :: FNStream -> Int -> FNStream
approx (z, as) n = (z, take n as)

printApprox :: FNStream -> Int -> String
printApprox x n = "(" ++ show z ++ ", " ++ show as ++ ")" where
    (z, as) = approx x n

-- simplified addition
-- Definition 6: A
sAddition :: SNStream -> SNStream -> Int -> Int -> SNStream
sAddition (  0:as)  (  0:bs)  0 b =   0:sAddition    as     bs        b 0
sAddition (0:0:as)  (  0:bs)  1 b =   0:sAddition (b:as)    bs        1 1
sAddition (0:1:as)  (0:1:bs)  1 1 = 1:0:sAddition    as     bs        0 1
sAddition (0:0:as)  (1:0:bs)  1 0 = 0:1:sAddition    as     bs        1 0
sAddition (  0:as)  (  1:bs)  1 1 =   1:sAddition    as     bs        0 0
sAddition (  1:as)  (  1:bs)  1 b =   1:sAddition    as     bs        b 1

sAddition (   1:as) (   0:bs) a b =     sAddition (   0:as) (   1:bs) a b
sAddition       as  (   1:bs) 0 b =     sAddition       as  (   0:bs) 1 b
sAddition (a':1:as) (b':0:bs) a b =     sAddition (a':0:as) (b':1:bs) a b
sAddition       as  (b':1:bs) a 0 =     sAddition       as  (b':0:bs) a 1

-- full addition
-- Definition 7: A'
addition :: FNStream -> FNStream -> FNStream
addition (z, as) (t, bs) = if z==t then (z+1, sAddition as bs 1 0) else (if z<t then addition (z+1, 1:0:as) (t, bs) else addition (z, as) (t+1, 1:0:bs))

-- simplified complement
-- Definition 8: C
sComplement :: SNStream -> SNStream
sComplement (x:as) = (1-x):sComplement as

-- full complement
-- Definition 9: C'
complement :: FNStream -> FNStream
complement (z, as) = (z+1, complement' as) where
    complement' (  0:as) =   1:1:0:sComplement as
    complement' (1:0:as) =     1:0:complement' as
    complement' (1:1:as) = 1:0:0:1:sComplement as

-- full subtraction
-- Definition 10: S'
subtraction :: FNStream -> FNStream -> FNStream
subtraction (z, as) (t, bs) = if z==t then (z+1, sAddition as (sComplement bs) 1 1) else (if z<t then subtraction (z+1, 1:0:as) (t, bs) else subtraction (z, as) (t+1, 1:0:bs))

-- simplified multiplication
-- Definition 11: P
sMultiplication :: SNStream -> SNStream -> SNStream
sMultiplication (  0:as) (    bs) = 0:sMultiplication as bs
sMultiplication (    as) (  0:bs) = 0:sMultiplication as bs
sMultiplication (1:0:as) (1:0:bs) = 0:sAddition (sAddition    as     bs  0 0) (  0:sMultiplication as bs) 1 0
sMultiplication (1:1:as) (1:0:bs) =   sAddition (sAddition (0:as)    bs  0 0) (0:0:sMultiplication as bs) 1 0
sMultiplication (1:0:as) (1:1:bs) =   sAddition (sAddition    as  (0:bs) 0 0) (0:0:sMultiplication as bs) 1 0
sMultiplication (1:1:as) (1:1:bs) =   sAddition (sAddition    as     bs  0 0) (0:0:sMultiplication as bs) 1 1

-- full multiplication
-- Definition 12: P'
multiplication :: FNStream -> FNStream -> FNStream
multiplication (z, as) (t, bs) = (z+t+2, sAddition (sMultiplication as bs) (sComplement (sAddition as bs 0 0)) 1 0)


main = putStrLn "golden notation"
