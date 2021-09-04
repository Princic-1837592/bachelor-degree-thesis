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

-- simplified addition
-- Definition 6: A
sAddition :: SNStream -> SNStream -> Int -> Int -> SNStream
sAddition (   0:as) (   0:bs) 0 b =   0:sAddition       as        bs  b 0
sAddition ( 0:0:as) (   0:bs) 1 b =   0:sAddition (   b:as)       bs  1 1
sAddition ( 0:1:as) ( 0:1:bs) 1 1 = 1:0:sAddition       as        bs  0 1
sAddition ( 0:0:as) ( 1:0:bs) 1 0 = 0:1:sAddition       as        bs  1 0
sAddition (   0:as) (   1:bs) 1 1 =   1:sAddition       as        bs  0 0
sAddition (   1:as) (   1:bs) 1 b =   1:sAddition       as        bs  b 1

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
sComplement (1:as) = 0:sComplement as
sComplement (0:as) = 1:sComplement as

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
-- controllare algoritmo di Karatsuba ([8])
multiplication :: FNStream -> FNStream -> FNStream
multiplication (z, as) (t, bs) = (z+t+2, sAddition (sMultiplication as bs) (sComplement (sAddition as bs 0 0)) 1 0)

-- simplified division
-- Definition 13: D
sDivision :: SNStream -> SNStream -> SNStream
sDivision as (1:bs) = sDivision' (0:0:as) (sComplement bs) where
    sDivision' (0:0:as) bs = sDivision'' (sAddition as (0:bs) 0 0) (0:as) bs
    sDivision' (0:1:as) bs = sDivision'' (sAddition as (0:bs) 1 1) (1:as) bs
    sDivision' (1:0:as) bs = sDivision'' (sAddition as (1:bs) 1 1)    as  bs
    sDivision'' (  0:0:cs) as bs = 0:sDivision'      as  bs
    sDivision'' (0:1:0:cs) as bs = 0:sDivision'      as  bs
    sDivision'' (0:1:1:cs) as bs = 1:sDivision' (0:0:cs) bs
    sDivision'' (    1:cs) as bs = 1:sDivision'      cs  bs

-- full division
-- Definition 14: D'
division :: FNStream -> FNStream -> FNStream
division (z, as) (t,   0:bs) = division' (complement (z-t, as)) (0:sComplement bs)
division (z, as) (t, 1:0:bs) = division (z, as) (t-1, bs)
division (z, as) (t, 1:1:bs) = division' (z-t+1, as) bs

division' (z, as) (0:0:bs) = division' (z+1, as) bs
division' (z, as) (0:1:bs) = (z+1, sDivision (sAddition    as  bs 0 0) (sComplement bs))
division' (z, as) (  1:bs) = (z+1, sDivision (sAddition (0:as) bs 0 1) (sComplement bs))



zeros :: SNStream
zeros = 0:zeros

ones :: SNStream
ones = 1:ones

zero :: FNStream
zero = (0, 1:1:zeros)

one :: FNStream
one = (1, 1:1:1:1:zeros)

minusOne :: FNStream
minusOne = (0, zeros)





---------------------------------------------




approx :: FNStream -> Int -> FNStream
approx (z, as) n = (z, take n as)

printApprox :: FNStream -> Int -> String
printApprox x n = "(" ++ show z ++ ", " ++ show as ++ ")" where
    (z, as) = approx x n

-- https://www.wolframalpha.com
toWolfram :: FNStream -> Int -> String
toWolfram x n = "(-1" ++ (if length sums > 0 then "+" else "") ++ sums ++ ")*" ++ sphi ++ "^(2*" ++ show z ++ ")" where
    (z, as) = approx x n
    f' 1 i = sphi ++ "^(" ++ show i ++ ")"
    f' 0 _ = ""
    f x [] = x
    f x xs = x++('+':xs)
    sphi = "((sqrt(5)+1)/2)"
    sums = foldr f "" (filter (\s -> length s > 0) (map (uncurry f') (zip as [-1,-2..])))

-- phi = (sqrt(5)+1)/2
toString :: FNStream -> Int -> String
toString x n = "(-1" ++ (if length sums > 0 then "+" else "") ++ sums ++ ")*phi^(2*" ++ show z ++ ")" where
    (z, as) = approx x n
    f x [] = x
    f x xs = x ++ ('+':xs)
    f' 1 i = "phi^(" ++ show i ++ ")"
    f' 0 _ = ""
    sums = foldr f "" (filter (\s -> length s > 0) (map (uncurry f') (zip as [-1,-2..])))

-- https://keisan.casio.com/calculator
toKeisanCasio :: FNStream -> Int -> String -> String
toKeisanCasio x n s = "\n" ++ s ++ " = " ++ toString x n ++ ";\n" ++ s ++ ";\n"



two ::FNStream
two = addition one one

a = ( 3,                           1:1:1:zeros)
b = ( 5,                             0:1:zeros)
c = (11,         1:1:0:1:0:0:1:0:1:0:0:1:zeros)
d = ( 6, 1:0:0:0:0:1:1:0:0:0:1:1:0:0:1:1:zeros)
f = ( 9, 1:0:1:0:0:0:1:0:1:0:1:0:1:0:1:0:zeros)
g = (13, 1:1:0:0:1:1:0:0:0:0:0:0:0:0:0:1:zeros)
allSum = foldl addition a [b,c,d,f,g]
allSub = foldl subtraction zero [a,b,c,d,f,g]
allSumPlusAllSub = addition allSum allSub

-- main = do {
--     putStrLn $ toKeisanCasio zero     10 "zero";
--     putStrLn $ toKeisanCasio one      10 "one";
--     putStrLn $ toKeisanCasio minusOne 10 "minusOne";
--     putStrLn $ toKeisanCasio two 10 "two";
--     -- putStrLn "\n\n\n\n";
--     -- putStrLn $ toKeisanCasio (addition one (subtraction two (subtraction minusOne two))) 5000 "maybeSix"; -- corretto
--     putStrLn "\n\n\n\n";
--     -- putStrLn $ toKeisanCasio (multiplication two minusOne) 6000 "maybeMinusTwo";
--     putStrLn $ toWolfram (multiplication two minusOne) 20;
--     print True;
-- }
main = do {
    putStrLn "phi = (sqrt(5)+1)/2;";
    -- putStrLn $ toKeisanCasio a 200 "a";
    -- putStrLn $ toKeisanCasio b 200 "b";
    -- putStrLn $ toKeisanCasio c 200 "c";
    -- putStrLn $ toKeisanCasio d 200 "d";
    -- putStrLn $ toKeisanCasio f 200 "f";
    -- putStrLn $ toKeisanCasio g 200 "g";
    -- putStrLn "\n\n\n\n";
    -- putStrLn $ toKeisanCasio (addition a f) 200 "aPlusF";
    -- putStrLn $ toKeisanCasio (addition d g) 200 "dPlusG";
    -- putStrLn $ toKeisanCasio (addition f (addition d g)) 200 "fPlusDPlusG";
    -- putStrLn $ toKeisanCasio (allSum) 200 "sum";
    -- putStrLn $ toKeisanCasio (allSub) 2000 "sub";
    putStrLn $ toKeisanCasio (allSumPlusAllSub) 5000 "maybeZero";
}