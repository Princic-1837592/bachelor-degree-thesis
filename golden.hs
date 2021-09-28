-- https://users.dimi.uniud.it/~pietro.digianantonio/papers/copy_pdf/golden.pdf

import System.Random

-- -- tipo che rappresenta un bit
-- data Bit = B0 | B1 deriving Eq

-- -- istanza di Num necessaria a rendere compatibile il tipo Bit con i literals 0 e 1
-- instance Num Bit where
--     fromInteger 0 = B0
--     fromInteger 1 = B1

-- instance Show Bit where
--     show B0 = "0"
--     show B1 = "1"
type Bit = Int

-- simplified notation stream
type SNStream = [Bit]

{-
full notation stream

(z, a) = (-1 + Σ ai * phi^-i) * phi^2z

ho preferito usare una coppia invece che un unico stream
perché fa capire meglio la differenza tra le due parti
-}
type FNStream = (Integer, SNStream)



------------------------------



-- simplified addition
-- Definition 6: A
sAddition :: SNStream -> SNStream -> Bit -> Bit -> SNStream
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
addition (z, as) (t, bs) = if z == t then (z+1, sAddition as bs 1 0) else (if z < t then addition (z + 1, 1:0:as) (t, bs) else addition (z, as) (t + 1, 1:0:bs))

-- simplified complement
-- Definition 8: C
sComplement :: SNStream -> SNStream
sComplement (1:as) = 0:sComplement as
sComplement (0:as) = 1:sComplement as

-- full complement
-- Definition 9: C'
complement :: FNStream -> FNStream
complement (z, as) = (z + 1, complement' as) where
    complement' (  0:as) =   1:1:0:sComplement as
    complement' (1:0:as) =     1:0:complement' as
    complement' (1:1:as) = 1:0:0:1:sComplement as

-- full subtraction
-- Definition 10: S'
subtraction :: FNStream -> FNStream -> FNStream
subtraction (z, as) (t, bs) = if z == t then (z + 1, sAddition as (sComplement bs) 1 1) else (if z < t then subtraction (z + 1, 1:0:as) (t, bs) else subtraction (z, as) (t + 1, 1:0:bs))

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
multiplication (z, as) (t, bs) = (z + t + 2, sAddition (sMultiplication as bs) (sComplement (sAddition as bs 0 0)) 1 0)

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
division (z, as) (t,   0:bs) = division' (complement (z - t, as)) (0:sComplement bs)
division (z, as) (t, 1:0:bs) = division (z, as) (t - 1, bs)
division (z, as) (t, 1:1:bs) = division' (z - t + 1, as) bs

division' (z, as) (0:0:bs) = division' (z + 1, as) bs
division' (z, as) (0:1:bs) = (z + 1, sDivision (sAddition    as  bs 0 0) (sComplement bs))
division' (z, as) (  1:bs) = (z + 1, sDivision (sAddition (0:as) bs 0 1) (sComplement bs))



------------------------------



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



------------------------------



takeI :: Integer -> [a] -> [a]
takeI n [] = []
takeI n (x:xs) = if n <= 0 then [] else (x:takeI (n - 1) xs)

-- il secondo elemento non è infinito
approx :: FNStream -> Integer -> (Integer, [Bit])
approx (z, as) n = (z, takeI n as)

printApprox :: FNStream -> Integer -> String
printApprox x n = "(" ++ show z ++ ", " ++ show as ++ ")" where
    (z, as) = approx x n

sToString :: SNStream -> Integer -> String
sToString xs n = foldr f "" (map (uncurry f') (filter (\x -> fst x == 1) (zip as [-1,-2..]))) where
    as = takeI n xs
    f x [] = x
    f x xs = x ++ ('+':xs)
    f' 1 i = "phi^(" ++ show i ++ ")"
    -- f' 0 _ = ""

-- phi = (sqrt(5)+1)/2
toString :: FNStream -> Integer -> String
toString (z, as) n = "(-1" ++ (if length sums > 0 then "+" else "") ++ sums ++ ")*phi^(2*" ++ show z ++ ")" where
    sums = sToString as n

-- https://keisan.casio.com/calculator
toKeisanCasio :: FNStream -> Integer -> String -> String
toKeisanCasio x n name = "\n" ++ name ++ " = " ++ toString x n ++ ";\n" ++ name ++ ";\n"

randomSNStream :: RandomGen g => g -> SNStream
randomSNStream gen = (fromIntegral (mod bit 2)):randomSNStream gen' where (bit, gen') = genWord8 gen

randomFNStream :: RandomGen g => g -> FNStream
randomFNStream gen = (fromIntegral x, randomSNStream gen') where (x, gen') = genWord8 gen

randomFNStreamBound :: RandomGen g => g -> Integer -> Integer -> Integer -> SNStream -> FNStream
randomFNStreamBound gen a b n s = ((mod x (b + 1 - a)) + a, if n >= 0 then (takeI n xs ++ s) else xs) where (x, xs) = randomFNStream gen



------------------------------



-- main = do {
    -- gen <- newStdGen;
    -- a <- pure $ randomFNStreamBound gen 0 10 200 zeros;
    -- gen <- newStdGen;
    -- b <- pure $ randomFNStreamBound gen 0 10 200 zeros;
    -- gen <- newStdGen;
    -- c <- pure $ randomFNStreamBound gen 0 10 200 zeros;
    -- gen <- newStdGen;
    -- d <- pure $ randomFNStreamBound gen 0 10 200 zeros;
    -- gen <- newStdGen;
    -- f <- pure $ randomFNStreamBound gen 0 10 200 zeros;
    -- gen <- newStdGen;
    -- g <- pure $ randomFNStreamBound gen 0 10 200 zeros;
    -- putStrLn "\n\n\n\nphi = (sqrt(5)+1)/2;";
    -- putStrLn $ toKeisanCasio a 1000 "a";
    -- putStrLn $ toKeisanCasio b 1000 "b";
    -- putStrLn $ toKeisanCasio c 1000 "c";
    -- putStrLn $ toKeisanCasio d 1000 "d";
    -- putStrLn $ toKeisanCasio f 1000 "f";
    -- putStrLn $ toKeisanCasio g 1000 "g";
    -- -- putStrLn $ "a-b;"++toKeisanCasio (subtraction a b) 1000 "aMinusB";
    -- -- putStrLn $ "c-d;"++toKeisanCasio (subtraction c d) 1000 "cMinusD";
    -- -- putStrLn $ "f-g;"++toKeisanCasio (subtraction f g) 1000 "fMinusG";
    -- -- putStrLn $ "b-a;"++toKeisanCasio (subtraction b a) 1000 "bMinusA";
    -- -- putStrLn $ "d-c;"++toKeisanCasio (subtraction d c) 1000 "dMinusC";
    -- -- putStrLn $ "g-f;"++toKeisanCasio (subtraction g f) 1000 "gMinusF";
    -- putStrLn $ "a*b;"++toKeisanCasio (multiplication a b) 1000 "aTimesB";
    -- putStrLn $ "c*d;"++toKeisanCasio (multiplication c d) 1000 "cTimesD";
    -- putStrLn $ "f*g;"++toKeisanCasio (multiplication f g) 1000 "fTimesG";
-- }


-- -- prove sulla moltiplicazione
-- main = do {
--     -- gen <- newStdGen;
--     -- a <- pure $ snd $ randomFNStreamBound gen 0 10 200 zeros;
--     -- gen <- newStdGen;
--     -- b <- pure $ snd $ randomFNStreamBound gen 0 10 200 zeros;
--     -- gen <- newStdGen;
--     -- c <- pure $ snd $ randomFNStreamBound gen 0 10 200 zeros;
--     -- gen <- newStdGen;
--     -- d <- pure $ snd $ randomFNStreamBound gen 0 10 200 zeros;
--     -- gen <- newStdGen;
--     -- f <- pure $ snd $ randomFNStreamBound gen 0 10 200 zeros;
--     -- gen <- newStdGen;
--     -- g <- pure $ snd $ randomFNStreamBound gen 0 10 200 zeros;
--     a <- pure $ snd (1, [1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0]++zeros);
--     b <- pure $ snd (3, [1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1]++zeros);
--     c <- pure $ snd (5, [1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0]++zeros);
--     d <- pure $ snd (9, [0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1]++zeros);
--     f <- pure $ snd (1, [0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0]++zeros);
--     g <- pure $ snd (3, [0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1]++zeros);
--     putStrLn "\n\n\n\nphi = (sqrt(5)+1)/2;";
--     putStrLn $ "\na = " ++ sToString a 220 ++ ";\na;";
--     putStrLn $ "\nb = " ++ sToString b 220 ++ ";\nb;";
--     putStrLn $ "\nc = " ++ sToString c 220 ++ ";\nc;";
--     putStrLn $ "\nd = " ++ sToString d 220 ++ ";\nd;";
--     putStrLn $ "\nf = " ++ sToString f 220 ++ ";\nf;";
--     putStrLn $ "\ng = " ++ sToString g 220 ++ ";\ng;";
--     putStrLn $ "\n(a*b)/(phi^2);\naTimesB = " ++ sToString (sMultiplication a b) 1000 ++ ";\naTimesB;";
--     putStrLn $ "\n(c*d)/(phi^2);\ncTimesD = " ++ sToString (sMultiplication c d) 1000 ++ ";\ncTimesD;";
--     putStrLn $ "\n(f*g)/(phi^2);\nfTimesG = " ++ sToString (sMultiplication f g) 1000 ++ ";\nfTimesG;";
-- }


-- prove sulla divisione
main = do {
    gen <- newStdGen;
    a <- pure $ 1:(snd $ randomFNStreamBound gen 0 10 200 zeros);
    gen <- newStdGen;
    b <- pure $ 1:(snd $ randomFNStreamBound gen 0 10 200 zeros);
    gen <- newStdGen;
    c <- pure $ 1:(snd $ randomFNStreamBound gen 0 10 200 zeros);
    gen <- newStdGen;
    d <- pure $ 1:(snd $ randomFNStreamBound gen 0 10 200 zeros);
    gen <- newStdGen;
    f <- pure $ 1:(snd $ randomFNStreamBound gen 0 10 200 zeros);
    gen <- newStdGen;
    g <- pure $ 1:(snd $ randomFNStreamBound gen 0 10 200 zeros);
    -- a <- pure $ snd (1, [1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0]++zeros);
    -- b <- pure $ snd (3, [1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1]++zeros);
    -- c <- pure $ snd (5, [1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0]++zeros);
    -- d <- pure $ snd (9, [1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1]++zeros);
    -- f <- pure $ snd (1, [0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0]++zeros);
    -- g <- pure $ snd (3, [1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1]++zeros);
    putStrLn "\n\n\n\nphi = (sqrt(5)+1)/2;";
    putStrLn $ "\na = " ++ sToString a 220 ++ ";\na;";
    putStrLn $ "\nb = " ++ sToString b 220 ++ ";\nb;";
    putStrLn $ "\nc = " ++ sToString c 220 ++ ";\nc;";
    putStrLn $ "\nd = " ++ sToString d 220 ++ ";\nd;";
    putStrLn $ "\nf = " ++ sToString f 220 ++ ";\nf;";
    putStrLn $ "\ng = " ++ sToString g 220 ++ ";\ng;";
    putStrLn $ "\naDb = " ++ sToString (sDivision a b) 1000 ++ ";\nabs(a/(b*phi) - aDb);";
    putStrLn $ "\naTb = " ++ sToString (sMultiplication a b) 1000 ++ ";\nabs((a*b)/(phi^2) - aTb);";
    putStrLn $ "\ncDd = " ++ sToString (sDivision c d) 1000 ++ ";\nabs(c/(d*phi) - cDd);";
    putStrLn $ "\ncTd = " ++ sToString (sMultiplication c d) 1000 ++ ";\nabs((c*d)/(phi^2) - cTd);";
    putStrLn $ "\nfDg = " ++ sToString (sDivision f g) 1000 ++ ";\nabs(f/(g*phi) - fDg);";
    putStrLn $ "\nfTg = " ++ sToString (sMultiplication f g) 1000 ++ ";\nabs((f*g)/(phi^2) - fTg);";
}
