import System.Random

-- -- tipo che rappresenta un bit
-- data SignedBit = B1 | B0 | BM1 deriving Eq

-- -- istanza di Num necessaria a rendere compatibile il tipo Bit con i literals 0 e 1
-- instance Num SignedBit where
--     fromInteger   1  = B1
--     fromInteger   0  = B0
--     fromInteger (-1) = BM1

--     negate  B1 = BM1
--     negate  B0 =  B0
--     negate BM1 =  B1

--     signum  B1 =  1
--     signum  B0 =  0
--     signum BM1 = -1



-- instance Show SignedBit where
--     show  B1 = " 1"
--     show  B0 = " 0"
--     show BM1 = "-1"

-- signed binary digit
type SignedBit = Int

-- signed binary digits stream
type SBDStream = [SignedBit]

-- signed binary digits mantissa exponent
type SBDMantissaExponent = (SBDStream, Int)


-- dyadic digit
type DyadicDigit = (Int, Int)

-- dyadic digits stream
type DDStream = [DyadicDigit]

-- dyadic digits mantissa exponent
type DDMantissaExponent = (DDStream, Int)



------------------------------

-- signed binary stream operations

sNegation :: SBDStream -> SBDStream
sNegation (x:xs) = (-x):sNegation xs

sMultiplicationByDigit :: SignedBit -> SBDStream -> SBDStream
sMultiplicationByDigit   1  xs = xs
sMultiplicationByDigit   0  xs = zeros
sMultiplicationByDigit (-1) xs = sNegation xs

sPlusOne :: SBDStream -> SBDStream
sPlusOne ( 1:xs) = ones
sPlusOne ( 0:xs) = 1:sPlusOne xs
sPlusOne (-1:xs) = 1:xs

sMinusOne :: SBDStream -> SBDStream
sMinusOne ( 1:xs) = minusOnes
sMinusOne ( 0:xs) = -1:sMinusOne xs
sMinusOne (-1:xs) = -1:xs

p :: SignedBit -> SBDStream -> SBDStream
p ( 1) ( 1:xs) = xs
p ( 1) ( 0:xs) = sMinusOne xs
p ( 1) (-1:xs) = minusOnes
p ( 0) ( 1:xs) = sPlusOne (sNegation xs)
p ( 0) ( 0:xs) = xs
p ( 0) (-1:xs) = sMinusOne xs
p (-1) ( 1:xs) = ones
p (-1) ( 0:xs) = sPlusOne (sNegation xs)
p (-1) (-1:xs) = xs

sAverage :: SBDStream -> SBDStream -> SBDStream
sAverage x y = sAverage' x y 0

-- sAverage' : Auxiliary function for sAverage
sAverage' :: SBDStream -> SBDStream -> Int -> SBDStream
sAverage' (x:xs) (y:ys) c = if mod d' 2 == 0 then (signum d':sAverage' xs ys 0) else sAverage'' xs ys d' where
    d' = x + y + 2 * c

-- sAverage'' : Auxiliary function for sAverage
sAverage'' :: SBDStream -> SBDStream -> Int -> SBDStream
sAverage'' (x:xs) (y:ys) d' = e:sAverage' (x:xs) (y:ys) c' where {
    d = 2 * d' + x + y;
    e = if d > 2 then 1 else (if d < -2 then -1 else 0);
    c' = d' - 2 * e
}

sMultiplication :: SBDStream -> SBDStream -> SBDStream
sMultiplication (x0:x1:xs) (y0:y1:ys) = sAverage p q where
    p = sAverage
        ((x0 * y1):sAverage (sMultiplicationByDigit y1 xs) (sMultiplicationByDigit x1 ys))
        (sAverage (sMultiplicationByDigit y0 xs) (sMultiplicationByDigit x0 ys))
    q = (x0 * y0):(x1 * y0):(x1 * y1):sMultiplication xs ys

sDivisionByInteger :: SBDStream -> Int -> SBDStream
sDivisionByInteger xs n = sDivisionByInteger' xs n 0

sDivisionByInteger' :: SBDStream -> Int -> Int -> SBDStream
sDivisionByInteger' (x:xs) n s =
    if s' >= n then
        (1:sDivisionByInteger' xs n (s' - n))
    else
        (if s' <= -n then
            (-1:sDivisionByInteger' xs n (s' + n))
        else
            (0:sDivisionByInteger' xs n s'))
        where
    s' = 2 * s + x

rShift :: SBDStream -> Int -> SBDStream
rShift xs 0 = xs
rShift xs n = 0:rShift xs (n - 1)

normalize :: SBDMantissaExponent -> SBDMantissaExponent
normalize x@(ms, e) = if e <= 0 then x else normalize' x where
normalize' (     0:ms, e) = normalize (   ms, (e - 1))
normalize' ( 1: -1:ms, e) = normalize ( 1:ms, (e - 1))
normalize' (-1:  1:ms, e) = normalize (-1:ms, (e - 1))





------------------------------

-- mantissa exponent operations

addition :: SBDMantissaExponent -> SBDMantissaExponent -> SBDMantissaExponent
addition (xs, z) (ys, t) = (sAverage (rShift xs (m - z)) (rShift ys (m - t)), m + 1) where
    m = max z t

subtraction :: SBDMantissaExponent -> SBDMantissaExponent -> SBDMantissaExponent
subtraction (xs, z) (ys, t) = (sAverage (rShift xs (m - z)) (sNegation (rShift ys (m - t))), m + 1) where
    m = max z t

multiplication :: SBDMantissaExponent -> SBDMantissaExponent -> SBDMantissaExponent
multiplication (xs, z) (ys, t) = (sMultiplication xs ys, z + t)



zeros :: SBDStream
zeros = 0:zeros

ones :: SBDStream
ones = 1:ones

minusOnes :: SBDStream
minusOnes = -1:minusOnes


zero :: SBDMantissaExponent
zero = (zeros, 0)

one :: SBDMantissaExponent
one = (1:zeros, 1)

minusOne :: SBDMantissaExponent
minusOne = (-1:zeros, 1)

two :: SBDMantissaExponent
two = (1:zeros, 2)



------------------------------

-- utilities

-- la matinssa non è più infinita
approx :: SBDMantissaExponent -> Int -> ([SignedBit], Int)
approx (ms, e) n = (take n ms, e)

sToString :: SBDStream -> Int -> String
sToString xs n = foldr f "" (map (uncurry f') (filter (\x -> fst x /= 0) (zip as [-1,-2..]))) where
    as = take n xs
    f x [] = x
    f x xs = x ++ ('+':xs)
    f'   1  i = "2^(" ++ show i ++ ")"
    f' (-1) i = "(-2^(" ++ show i ++ "))"

toString :: SBDMantissaExponent -> Int -> String
toString (ms, e) n = "(" ++ sums ++ ")*2^" ++ show e where
    sums = sToString ms n

-- https://keisan.casio.com/calculator
toKeisanCasio :: SBDMantissaExponent -> Int -> String -> String
toKeisanCasio x n name = "\n" ++ name ++ " = " ++ toString x n ++ ";\n" ++ name ++ ";\n"

randomSBDStream :: RandomGen g => g -> SBDStream
randomSBDStream gen = (fromIntegral (mod bit 3)) - 1:randomSBDStream gen' where (bit, gen') = genWord8 gen

randomSBDMantissaExponent :: RandomGen g => g -> SBDMantissaExponent
randomSBDMantissaExponent gen = (randomSBDStream gen', fromIntegral x) where (x, gen') = genWord8 gen

randomSBDMantissaExponentBound :: RandomGen g => g -> Int -> Int -> Int -> SBDStream -> SBDMantissaExponent
randomSBDMantissaExponentBound gen a b n s = (if n >= 0 then (take n xs ++ s) else xs, (mod x (b + 1 - a)) + a) where (xs, x) = randomSBDMantissaExponent gen



------------------------------



-- -- prove SBDStream
-- main = do {
--     putStrLn "\n\n\n";
--     gen <- newStdGen;
--     a <- pure $ fst $ randomSBDMantissaExponentBound gen 0 10 200 zeros;
--     gen <- newStdGen;
--     b <- pure $ fst $ randomSBDMantissaExponentBound gen 0 10 200 zeros;
--     gen <- newStdGen;
--     c <- pure $ fst $ randomSBDMantissaExponentBound gen 0 10 200 zeros;
--     gen <- newStdGen;
--     d <- pure $ fst $ randomSBDMantissaExponentBound gen 0 10 200 zeros;
--     gen <- newStdGen;
--     f <- pure $ fst $ randomSBDMantissaExponentBound gen 0 10 200 zeros;
--     gen <- newStdGen;
--     g <- pure $ fst $ randomSBDMantissaExponentBound gen 0 10 200 zeros;
--     putStrLn $ "a = " ++ sToString a 1000 ++ ";\na;\n";
--     putStrLn $ "b = " ++ sToString b 1000 ++ ";\nb;\n";
--     putStrLn $ "c = " ++ sToString c 1000 ++ ";\nc;\n";
--     putStrLn $ "d = " ++ sToString d 1000 ++ ";\nd;\n";
--     putStrLn $ "f = " ++ sToString f 1000 ++ ";\nf;\n";
--     putStrLn $ "g = " ++ sToString g 1000 ++ ";\ng;\n";
--     putStrLn $ "avb = " ++ sToString (sAverage a b) 1000 ++ ";\n(a+b)/2;\navb;\n";
--     putStrLn $ "cvd = " ++ sToString (sAverage c d) 1000 ++ ";\n(c+d)/2;\ncvd;\n";
--     putStrLn $ "fvg = " ++ sToString (sAverage f g) 1000 ++ ";\n(f+g)/2;\nfvg;\n";
--     -- print True;
-- }


-- -- prove SBDMantissaExponent
-- main = do {
--     putStrLn "\n\n\n";
--     gen <- newStdGen;
--     a <- pure $ randomSBDMantissaExponentBound gen 0 10 100 zeros;
--     gen <- newStdGen;
--     b <- pure $ randomSBDMantissaExponentBound gen 0 10 100 zeros;
--     gen <- newStdGen;
--     c <- pure $ randomSBDMantissaExponentBound gen 0 10 100 zeros;
--     gen <- newStdGen;
--     d <- pure $ randomSBDMantissaExponentBound gen 0 10 100 zeros;
--     gen <- newStdGen;
--     f <- pure $ randomSBDMantissaExponentBound gen 0 10 100 zeros;
--     gen <- newStdGen;
--     g <- pure $ randomSBDMantissaExponentBound gen 0 10 100 zeros;
--     putStrLn $ toKeisanCasio a 1000 "a";
--     putStrLn $ toKeisanCasio b 1000 "b";
--     putStrLn $ toKeisanCasio c 1000 "c";
--     putStrLn $ toKeisanCasio d 1000 "d";
--     putStrLn $ toKeisanCasio f 1000 "f";
--     putStrLn $ toKeisanCasio g 1000 "g";
--     putStrLn $ "a*b;" ++ toKeisanCasio (multiplication a b) 1000 "apb";
--     putStrLn $ "c*d;" ++ toKeisanCasio (multiplication c d) 1000 "cpd";
--     putStrLn $ "f*g;" ++ toKeisanCasio (multiplication f g) 1000 "fpg";
--     -- print True;
-- }


main = do {
    putStrLn $ toKeisanCasio zero 1000 "zero";
    putStrLn $ toKeisanCasio one 1000 "one";
    putStrLn $ toKeisanCasio minusOne 1000 "minusOne";
}
