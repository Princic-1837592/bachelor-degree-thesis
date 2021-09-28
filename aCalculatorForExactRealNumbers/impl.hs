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

sDiv :: SBDStream -> SBDStream -> DDStream
sDiv xs (      0:ys) = undefined
sDiv xs (  1: -1:ys) = undefined
sDiv xs ( -1:  1:ys) = undefined
sDiv xs ( 1:ys) = sDiv' xs (1:ys)
sDiv xs (-1:ys) = sDiv' (sNegation xs) (1:sNegation ys)

sbDiv_emit :: Int -> SBDStream -> SBDStream -> DDStream
sbDiv_emit ( 4) xs ys = ( 1, 0):sDiv' xs ys
sbDiv_emit ( 2) xs ys = ( 1, 1):sDiv' xs ys
sbDiv_emit ( 1) xs ys = ( 1, 2):sDiv' xs ys
sbDiv_emit ( 0) xs ys = ( 0, 0):sDiv' xs ys
sbDiv_emit (-1) xs ys = (-1, 2):sDiv' xs ys
sbDiv_emit (-2) xs ys = (-1, 1):sDiv' xs ys
sbDiv_emit (-4) xs ys = (-1, 0):sDiv' xs ys

sDiv' :: SBDStream -> SBDStream -> DDStream
sDiv'   ( 1: -1:xs) ys = sbDiv_emit 0 ( 1:xs) ys
sDiv'   (-1:  1:xs) ys = sbDiv_emit 0 (-1:xs) ys
sDiv'   (     0:xs) ys = sbDiv_emit 0 xs ys
sDiv' x@(     1:xs) ys = case a of {
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
sDiv' x@(-1:xs) ys = case a of {
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



------------------------------

-- dyadic stream operations

dyf :: DDMantissaExponent -> DDMantissaExponent
dyf (e, ms) = (e, map lowestTerms ms)

lowestTerms :: DyadicDigit -> DyadicDigit
lowestTerms (0, b) = (0, 0)
lowestTerms (a, b) = if mod a 2 == 1 then (a, b) else lowestTerms (div a 2, b - 1)

dAverage :: DyadicDigit -> DyadicDigit -> DyadicDigit
dAverage (a, b) (c, d) = if b > d then (a + c * 2 ^ (b - d), b + 1) else (if d > b then (c + a * 2 ^ (d - b), d + 1) else lowestTerms (a + c, b + 1))

dAddition :: DyadicDigit -> DyadicDigit -> DyadicDigit
dAddition x y = (a, b - 1) where
    (a, b) = dAverage x y

dSubtraction :: DyadicDigit -> DyadicDigit -> DyadicDigit
dSubtraction a (c, d) = dAddition a (-c, d)

dMultiplicationByDigit :: Int -> DyadicDigit -> DyadicDigit
dMultiplicationByDigit x (a, b) = lowestTerms (a * x, b)

dMultiplication :: DyadicDigit -> DyadicDigit -> DyadicDigit
dMultiplication (0, b) (c, d) = (0, 0)
dMultiplication (a, b) (0, d) = (0, 0)
dMultiplication (a, b) (c, d) = (a * c, b + d)

dRelational :: DyadicDigit -> DyadicDigit -> (Int -> Int -> Bool) -> Bool
dRelational (a, b) (c, d) op = op a' c' where
    (a', c') = if b == d then (a, c) else (if b > d then (a, c * 2 ^ (b - d)) else (a * 2 ^ (d - b), c))





------------------------------

-- conversion algorithms

dyfToSbf :: DDMantissaExponent -> SBDMantissaExponent
dyfToSbf (e, ms) = (e, d_to_s ms)

d_to_s :: DDStream -> SBDStream
d_to_s x = 0:d_to_s' x where
    d_to_s' (a:(c, d):xs) = if dRelational a' (1, 2) (>=) then
                        (1:d_to_s' (dMultiplicationByDigit 4 (dSubtraction a' (1, 2)):xs))
                    else
                        (if dRelational a' (-1, 2) (<=) then
                            (-1:d_to_s' (dMultiplicationByDigit 4 (dAddition a' (1, 2)):xs))
                        else
                            (0:d_to_s' (dMultiplicationByDigit 4 a':xs))) where
        a' = dAverage a (c, d + 1)

ddToFloat :: DyadicDigit -> Float
ddToFloat (a, b) = (fromIntegral a) / (fromIntegral (2 ^ b))



dzeros :: DDStream
dzeros = (0, 0):dzeros



------------------------------

-- mantissa exponent operations

normalize :: SBDMantissaExponent -> SBDMantissaExponent
normalize x@(e, ms) = if e <= 0 then x else normalize' x where
normalize' (e,      0:ms) = normalize ((e - 1), ms)
normalize' (e,  1: -1:ms) = normalize ((e - 1), 1:ms)
normalize' (e, -1:  1:ms) = normalize ((e - 1), -1:ms)

sbAddition :: SBDMantissaExponent -> SBDMantissaExponent -> SBDMantissaExponent
sbAddition (z, xs) (t, ys) = (m + 1, sAverage (rShift xs (m - z)) (rShift ys (m - t))) where
    m = max z t

sbSubtraction :: SBDMantissaExponent -> SBDMantissaExponent -> SBDMantissaExponent
sbSubtraction (z, xs) (t, ys) = (m + 1, sAverage (rShift xs (m - z)) (sNegation (rShift ys (m - t)))) where
    m = max z t

sbMultiplication :: SBDMantissaExponent -> SBDMantissaExponent -> SBDMantissaExponent
sbMultiplication (z, xs) (t, ys) = (z + t, sMultiplication xs ys)

sbDivision :: SBDMantissaExponent -> SBDMantissaExponent -> SBDMantissaExponent
sbDivision (z, xs) (t, ys) = dyfToSbf (dyf (z - t + t' + 2, sDiv xs ys')) where
    (t', ys') = fixinput ys



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
toString (e, ms) n = "(" ++ sums ++ ")*2^(" ++ show e ++ ")" where
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



-- -- prove SBDStream
-- main = do {
--     putStrLn "\n\n\n";
--     gen <- newStdGen;
--     a <- pure $ snd $ randomSBDMantissaExponentBound gen 0 10 50 zeros;
--     gen <- newStdGen;
--     b <- pure $ snd $ randomSBDMantissaExponentBound gen 0 10 50 zeros;
--     gen <- newStdGen;
--     c <- pure $ snd $ randomSBDMantissaExponentBound gen 0 10 50 zeros;
--     gen <- newStdGen;
--     d <- pure $ snd $ randomSBDMantissaExponentBound gen 0 10 50 zeros;
--     gen <- newStdGen;
--     f <- pure $ snd $ randomSBDMantissaExponentBound gen 0 10 50 zeros;
--     gen <- newStdGen;
--     g <- pure $ snd $ randomSBDMantissaExponentBound gen 0 10 50 zeros;
--     putStrLn $ "a = " ++ sToString a 1000 ++ ";\na;\n";
--     putStrLn $ "b = " ++ sToString b 1000 ++ ";\nb;\n";
--     -- putStrLn $ "c = " ++ sToString c 1000 ++ ";\nc;\n";
--     -- putStrLn $ "d = " ++ sToString d 1000 ++ ";\nd;\n";
--     -- putStrLn $ "f = " ++ sToString f 1000 ++ ";\nf;\n";
--     -- putStrLn $ "g = " ++ sToString g 1000 ++ ";\ng;\n";
--     -- ab@(x:xs) <- pure $ sAddition a b;
--     -- putStrLn $ "/* " ++ show (take 100 ab) ++ " */";
--     putStrLn $ "ab = " ++ sToString (sAddition a b) 1000 ++ ";\na+b;\nab;\n";
--     -- putStrLn $ "cd = " ++ sToString (sSubtraction c d) 1000 ++ ";\nc-d;\ncd;\n";
--     -- putStrLn $ "fg = " ++ sToString (sSubtraction f g) 1000 ++ ";\nf-g;\nfg;\n";
--     -- print True;
-- }


-- prove SBDMantissaExponent
main = do {
    putStrLn "\n\n\n";
    gen <- newStdGen;
    a <- pure $ randomSBDMantissaExponentBound gen 0 10 100 zeros;
    gen <- newStdGen;
    b <- pure $ randomSBDMantissaExponentBound gen 0 10 100 zeros;
    gen <- newStdGen;
    c <- pure $ randomSBDMantissaExponentBound gen 0 10 100 zeros;
    gen <- newStdGen;
    d <- pure $ randomSBDMantissaExponentBound gen 0 10 100 zeros;
    gen <- newStdGen;
    f <- pure $ randomSBDMantissaExponentBound gen 0 10 100 zeros;
    gen <- newStdGen;
    g <- pure $ randomSBDMantissaExponentBound gen 0 10 100 zeros;
    putStrLn $ toKeisanCasio a 1000 "a";
    putStrLn $ toKeisanCasio b 1000 "b";
    putStrLn $ toKeisanCasio c 1000 "c";
    putStrLn $ toKeisanCasio d 1000 "d";
    putStrLn $ toKeisanCasio f 1000 "f";
    putStrLn $ toKeisanCasio g 1000 "g";
    putStrLn $ "a/b;" ++ toKeisanCasio (sbDivision a b) 1000 "ab";
    putStrLn $ "c/d;" ++ toKeisanCasio (sbDivision c d) 1000 "cd";
    putStrLn $ "f/g;" ++ toKeisanCasio (sbDivision f g) 1000 "fg";
    -- print True;
}


-- main = do {
--     putStrLn $ toKeisanCasio zero 1000 "zero";
--     putStrLn $ toKeisanCasio one 1000 "one";
--     putStrLn $ toKeisanCasio minusOne 1000 "minusOne";
-- }


-- -- prove dyadic digits
-- main = do {
--     a <- pure $ (1, 0);
--     b <- pure $ (3, 4);
--     ab <- pure $ dAverage a (fst b, (snd b) + 1);
--     -- c <- pure $ (-42, 1);
--     -- ac <- pure $ dAddition a c;
--     x <- pure $ (1, 1):(1, 1):(1, 2):dzeros;
--     print $ ddToFloat a;
--     print $ ddToFloat b;
--     -- print $ ddToFloat c;
--     print $ ((ddToFloat a) + (ddToFloat b) / 2) / 2;
--     print $ ddToFloat ab;
--     -- print $ dRelational ab ab (==);
--     -- print $ (ddToFloat a) + (ddToFloat c);
--     -- print $ ddToFloat ac;
--     -- print $ ddToFloat (dAddition (0, 0) c);
--     -- print $ ddToFloat (dSubtraction (0, 0) c);
--     print $ take 200 (d_to_s x);
--     print $ ddToFloat (dSubtraction a (1, 2));
-- }
