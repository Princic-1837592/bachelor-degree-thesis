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
type SBDMantissaExponent = (Int, SBDStream)


-- dyadic digit
type DyadicDigit = (Int, Int)

-- dyadic digits stream
type DDStream = [DyadicDigit]

-- dyadic digits mantissa exponent
type DDMantissaExponent = (Int, DDStream)



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

-- i SBDStream non sono chiusi rispetto alla somma
-- usare solo se si ha la certezza che non si vada in overflow
sAddition :: SBDStream -> SBDStream -> SBDStream
sAddition xs ys = tail ((sAverage xs ys))

-- i SBDStream non sono chiusi rispetto alla sottrazione
-- usare solo se si ha la certezza che non si vada in overflow
sSubtraction :: SBDStream -> SBDStream -> SBDStream
sSubtraction xs ys = tail ((sAverage xs (sNegation ys)))

sbDiv :: SBDStream -> SBDStream -> DDStream
sbDiv xs (      0:ys) = undefined
sbDiv xs (  1: -1:ys) = undefined
sbDiv xs ( -1:  1:ys) = undefined
sbDiv xs ( 1:ys) = sbDiv' xs (1:ys)
sbDiv xs (-1:ys) = sbDiv' (sNegation xs) (1:sNegation ys)

sbDiv_emit :: Int -> SBDStream -> SBDStream -> DDStream
sbDiv_emit ( 4) xs ys = ( 1, 1):sbDiv' xs ys
sbDiv_emit ( 2) xs ys = ( 1, 2):sbDiv' xs ys
sbDiv_emit ( 1) xs ys = ( 1, 4):sbDiv' xs ys
sbDiv_emit ( 0) xs ys = ( 0, 1):sbDiv' xs ys
sbDiv_emit (-1) xs ys = (-1, 4):sbDiv' xs ys
sbDiv_emit (-2) xs ys = (-1, 2):sbDiv' xs ys
sbDiv_emit (-4) xs ys = (-1, 1):sbDiv' xs ys

sbDiv' :: SBDStream -> SBDStream -> DDStream
sbDiv'   ( 1: -1:xs) ys = sbDiv_emit 0 ( 1:xs) ys
sbDiv'   (-1:  1:xs) ys = sbDiv_emit 0 (-1:xs) ys
sbDiv'   (     0:xs) ys = sbDiv_emit 0 xs ys
sbDiv' x@(     1:xs) ys = case a of {
    (-1) -> if a' == 1 then
                (sbDiv_emit 2 (-1:r'') ys)
            else
                (sbDiv_emit 1 (p 0 (sSubtraction x (0:ys))) ys);
      0  -> sbDiv_emit 2 r' ys;
      1  -> if a' == -1 then
                (sbDiv_emit 2 (1:r'') ys)
            else
                (sbDiv_emit 4 (p 0 (sSubtraction r ys)) ys);
} where r@(a:r'@(a':r'')) = sSubtraction x ys
sbDiv' x@(-1:xs) ys = case a of {
      1  -> if a' == -1 then
                (sbDiv_emit (-2) (1:r'') ys)
            else
                (sbDiv_emit (-1) (p 0 (sAddition x (0:ys))) ys);
      0  -> sbDiv_emit (-2) r' ys;
    (-1) -> if a' == 1 then
                (sbDiv_emit (-1) (-1:r'') ys)
            else
                (sbDiv_emit (-4) (p 0 (sAddition r ys)) ys);
} where r@(a:r'@(a':r'')) = sAddition x ys

fixinput' :: SBDStream -> Int -> SBDMantissaExponent
fixinput' (     0:xs) n = fixinput'     xs  (n + 1)
fixinput' ( 1: -1:xs) n = fixinput' ( 1:xs) (n + 1)
fixinput' (-1:  1:xs) n = fixinput' (-1:xs) (n + 1)
fixinput'         xs  n = (n, xs)

fixinput :: SBDStream -> SBDMantissaExponent
fixinput x = fixinput' x 0

sbfDiv :: SBDMantissaExponent -> SBDMantissaExponent -> SBDMantissaExponent
sbfDiv (z, xs) (t, ys) = dyfToSbf (dyf (z - t + t' + 2, sbDiv xs ys')) where
    (t', ys') = fixinput ys

dyfToSbf :: DDMantissaExponent -> SBDMantissaExponent
dyfToSbf = undefined

dyf = id



------------------------------

-- mantissa exponent operations

normalize :: SBDMantissaExponent -> SBDMantissaExponent
normalize x@(e, ms) = if e <= 0 then x else normalize' x where
normalize' (e,      0:ms) = normalize ((e - 1), ms)
normalize' (e,  1: -1:ms) = normalize ((e - 1), 1:ms)
normalize' (e, -1:  1:ms) = normalize ((e - 1), -1:ms)

addition :: SBDMantissaExponent -> SBDMantissaExponent -> SBDMantissaExponent
addition (z, xs) (t, ys) = (m + 1, sAverage (rShift xs (m - z)) (rShift ys (m - t))) where
    m = max z t

subtraction :: SBDMantissaExponent -> SBDMantissaExponent -> SBDMantissaExponent
subtraction (z, xs) (t, ys) = (m + 1, sAverage (rShift xs (m - z)) (sNegation (rShift ys (m - t)))) where
    m = max z t

multiplication :: SBDMantissaExponent -> SBDMantissaExponent -> SBDMantissaExponent
multiplication (z, xs) (t, ys) = (z + t, sMultiplication xs ys)



zeros :: SBDStream
zeros = 0:zeros

ones :: SBDStream
ones = 1:ones

minusOnes :: SBDStream
minusOnes = -1:minusOnes


zero :: SBDMantissaExponent
zero = (0, zeros)

one :: SBDMantissaExponent
one = (1, 1:zeros)

minusOne :: SBDMantissaExponent
minusOne = (1, -1:zeros)

two :: SBDMantissaExponent
two = (2, 1:zeros)



------------------------------

-- utilities

-- la matinssa non è più infinita
approx :: SBDMantissaExponent -> Int -> (Int, [SignedBit])
approx (e, ms) n = (e, take n ms)

sToString :: SBDStream -> Int -> String
sToString xs n = foldr f "" (map (uncurry f') (filter (\x -> fst x /= 0) (zip as [-1,-2..]))) where
    as = take n xs
    f x [] = x
    f x xs = x ++ ('+':xs)
    f'   1  i = "2^(" ++ show i ++ ")"
    f' (-1) i = "(-2^(" ++ show i ++ "))"

toString :: SBDMantissaExponent -> Int -> String
toString (e, ms) n = "(" ++ sums ++ ")*2^" ++ show e where
    sums = sToString ms n

-- https://keisan.casio.com/calculator
toKeisanCasio :: SBDMantissaExponent -> Int -> String -> String
toKeisanCasio x n name = "\n" ++ name ++ " = " ++ toString x n ++ ";\n" ++ name ++ ";\n"

randomSBDStream :: RandomGen g => g -> SBDStream
randomSBDStream gen = (fromIntegral (mod bit 3)) - 1:randomSBDStream gen' where (bit, gen') = genWord8 gen

randomSBDMantissaExponent :: RandomGen g => g -> SBDMantissaExponent
randomSBDMantissaExponent gen = (fromIntegral x, randomSBDStream gen') where (x, gen') = genWord8 gen

randomSBDMantissaExponentBound :: RandomGen g => g -> Int -> Int -> Int -> SBDStream -> SBDMantissaExponent
randomSBDMantissaExponentBound gen a b n s = ((mod x (b + 1 - a)) + a, if n >= 0 then (take n xs ++ s) else xs) where (x, xs) = randomSBDMantissaExponent gen



------------------------------



-- prove SBDStream
main = do {
    putStrLn "\n\n\n";
    gen <- newStdGen;
    a <- pure $ snd $ randomSBDMantissaExponentBound gen 0 10 50 zeros;
    gen <- newStdGen;
    b <- pure $ snd $ randomSBDMantissaExponentBound gen 0 10 50 zeros;
    gen <- newStdGen;
    c <- pure $ snd $ randomSBDMantissaExponentBound gen 0 10 50 zeros;
    gen <- newStdGen;
    d <- pure $ snd $ randomSBDMantissaExponentBound gen 0 10 50 zeros;
    gen <- newStdGen;
    f <- pure $ snd $ randomSBDMantissaExponentBound gen 0 10 50 zeros;
    gen <- newStdGen;
    g <- pure $ snd $ randomSBDMantissaExponentBound gen 0 10 50 zeros;
    putStrLn $ "a = " ++ sToString a 1000 ++ ";\na;\n";
    putStrLn $ "b = " ++ sToString b 1000 ++ ";\nb;\n";
    -- putStrLn $ "c = " ++ sToString c 1000 ++ ";\nc;\n";
    -- putStrLn $ "d = " ++ sToString d 1000 ++ ";\nd;\n";
    -- putStrLn $ "f = " ++ sToString f 1000 ++ ";\nf;\n";
    -- putStrLn $ "g = " ++ sToString g 1000 ++ ";\ng;\n";
    -- ab@(x:xs) <- pure $ sAddition a b;
    -- putStrLn $ "/* " ++ show (take 100 ab) ++ " */";
    putStrLn $ "ab = " ++ sToString (sAddition a b) 1000 ++ ";\na+b;\nab;\n";
    -- putStrLn $ "cd = " ++ sToString (sSubtraction c d) 1000 ++ ";\nc-d;\ncd;\n";
    -- putStrLn $ "fg = " ++ sToString (sSubtraction f g) 1000 ++ ";\nf-g;\nfg;\n";
    -- print True;
}


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


-- main = do {
--     putStrLn $ toKeisanCasio zero 1000 "zero";
--     putStrLn $ toKeisanCasio one 1000 "one";
--     putStrLn $ toKeisanCasio minusOne 1000 "minusOne";
-- }
