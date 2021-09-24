-- tipo che rappresenta un bit
data SignedBit = B1 | B0 | BM1 deriving Eq

-- istanza di Num necessaria a rendere compatibile il tipo Bit con i literals 0 e 1
instance Num SignedBit where
    fromInteger   1  = B1
    fromInteger   0  = B0
    fromInteger (-1) = BM1

    negate  B1 = BM1
    negate  B0 =  B0
    negate BM1 =  B1

instance Show SignedBit where
    show  B1 = " 1"
    show  B0 = " 0"
    show BM1 = "-1"

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

------------------------------

minusOnes :: SBDStream
minusOnes = (-1):minusOnes

zeros :: SBDStream
zeros = 0:zeros

ones :: SBDStream
ones = 1:ones



stream :: SBDStream
stream = -1:0:1:zeros


main = do {
    print $ take 10 stream;
    print $ take 10 (sNegation stream);
    print True;
}
