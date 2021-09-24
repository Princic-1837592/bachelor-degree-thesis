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
type SignedBit = Int

-- signed binary digits stream notation
type SBDStream = [SignedBit]

-- signed binary digits mantissa exponent notation
type SBDMantissaExponent = (SBDStream, Int)

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



------------------------------





------------------------------


randomSBDStream :: RandomGen g => g -> SBDStream
randomSBDStream gen = (fromIntegral (mod bit 3)) - 1:randomSBDStream gen' where (bit, gen') = genWord8 gen

randomSBDMantissaExponent :: RandomGen g => g -> SBDMantissaExponent
randomSBDMantissaExponent gen = (randomSBDStream gen', fromIntegral x) where (x, gen') = genWord8 gen

randomSBDMantissaExponentBound :: RandomGen g => g -> Int -> Int -> Int -> SBDStream -> SBDMantissaExponent
randomSBDMantissaExponentBound gen a b n s = (if n >= 0 then (take n xs ++ s) else xs, (mod x (b + 1 - a)) + a) where (xs, x) = randomSBDMantissaExponent gen

------------------------------

minusOnes :: SBDStream
minusOnes = (-1):minusOnes

zeros :: SBDStream
zeros = 0:zeros

ones :: SBDStream
ones = 1:ones



a :: SBDStream
a = -1:0:1:zeros

-- test per SBDStream
main = do {
    gen <- newStdGen;
    (m', e') <- pure $ randomSBDMantissaExponentBound gen 0 10 20 zeros;
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
    print (e', take 100 m');
    print True;
}
