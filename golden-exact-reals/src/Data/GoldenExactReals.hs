module Data.GoldenExactReals where
import Utils.Utils (iterateI, lengthI, takeI)



-- TYPES

-- single bit type. It should only contain digits 1 and 0
type Bit = Int

-- simplified notation type
type SNStream = [Bit]

-- full notation type
-- i preferred using a pair instead of a single stream because it fits more the exponent-mantissa representation
type FNStream = (Integer, SNStream)



------------------------------



-- OPERATIONS

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
addition (z, as) (t, bs) = if z == t then (z + 1, sAddition as bs 1 0) else (if z < t then addition (z + 1, 1:0:as) (t, bs) else addition (z, as) (t + 1, 1:0:bs))

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

-- full multiplication (wrong)
-- Definition 12: P'
multiplication' :: FNStream -> FNStream -> FNStream
multiplication' (z, as) (t, bs) = (z + t + 2, sAddition (sMultiplication as bs) (sComplement (sAddition as bs 0 0)) 1 0)

-- full multiplication (correct)
-- Definition 12: P'
multiplication :: FNStream -> FNStream -> FNStream
multiplication (z, as) (t, bs) = complement (z + t + 2, sAddition (sComplement (sMultiplication as bs)) (sAddition as bs 0 0) 1 0)

-- simplified division
-- Definition 13: D
sDivision :: SNStream -> SNStream -> SNStream
sDivision as (1:bs) = sDivision' (0:0:as) (sComplement bs)
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
division' (z, as) (0:1:bs) = (z + 1, sDivision' (sAddition    as  bs 0 0) (sComplement bs))
division' (z, as) (  1:bs) = (z + 1, sDivision' (sAddition (0:as) bs 0 1) (sComplement bs))



------------------------------



-- CONSTANTS AND CONVERSION

zeros :: SNStream
zeros = 0:zeros

ones :: SNStream
ones = 1:ones

oneZeros :: SNStream
oneZeros = 1:0:oneZeros

minusOne :: FNStream
minusOne = (0, zeros)

zero :: FNStream
zero = (0, oneZeros)

one :: FNStream
one = (1, 1:1:oneZeros)

phi :: FNStream
phi = (2, 1:oneZeros)

two :: FNStream
two = (2, 1:1:0:1:oneZeros)

three :: FNStream
three = (2, 1:1:1:1:0:1:zeros)

four :: FNStream
four = (3, 1:1:0:1:0:1:0:1:zeros)

five :: FNStream
five = (3, 1:1:0:1:0:1:1:1:oneZeros)

six :: FNStream
six = (3, 1:1:0:1:1:1:0:1:oneZeros)

seven :: FNStream
seven = (3, 1:1:1:0:1:1:0:0:0:1:zeros)

eight :: FNStream
eight = (3, 1:1:1:1:0:1:0:0:0:1:zeros)

nine :: FNStream
nine = (3, 1:1:1:1:0:1:1:1:0:1:zeros)

ten :: FNStream
ten = (3, 1:1:1:1:1:1:0:1:0:1:zeros)

-- this function converts an Integer into a golden real using the following property:
-- x = dn, ..., d1, d0
-- x = dn*10^n + ... + d1*10 + d0
-- thanks to the Integer type the input has no size limits
integerToGolden :: Integer -> FNStream
integerToGolden x = if signum x /= -1 then real else multiplication minusOne real where
    integerToGolden' 0 r c = r
    integerToGolden' x r c = case (mod x 10) of
        0 -> integerToGolden' (div x 10)           r                           (multiplication c ten)
        1 -> integerToGolden' (div x 10) (addition r (multiplication one   c)) (multiplication c ten)
        2 -> integerToGolden' (div x 10) (addition r (multiplication two   c)) (multiplication c ten)
        3 -> integerToGolden' (div x 10) (addition r (multiplication three c)) (multiplication c ten)
        4 -> integerToGolden' (div x 10) (addition r (multiplication four  c)) (multiplication c ten)
        5 -> integerToGolden' (div x 10) (addition r (multiplication five  c)) (multiplication c ten)
        6 -> integerToGolden' (div x 10) (addition r (multiplication six   c)) (multiplication c ten)
        7 -> integerToGolden' (div x 10) (addition r (multiplication seven c)) (multiplication c ten)
        8 -> integerToGolden' (div x 10) (addition r (multiplication eight c)) (multiplication c ten)
        9 -> integerToGolden' (div x 10) (addition r (multiplication nine  c)) (multiplication c ten)
    real = integerToGolden' (abs x) zero one

-- this function converts a pair (integral, decimal) into a golden real
-- thanks to the Integer type the input has no size limits
-- it takes an additional Integer parameter s to specify the additional right-shift to apply to the decimal part
-- this is required to represent numbers in the form: x.0y
-- it may be interpreted as the number of zeros on the left of the decimal part
rationalToGolden :: Integer -> Integer -> Integer -> FNStream
rationalToGolden i d s = if signumFix i == signumFix d then real else multiplication minusOne real where
    digits = lengthI . show . abs
    integralPart = integerToGolden (abs i)
    decimalPart = division (integerToGolden (abs d)) (iterateI (multiplication ten) one (digits d + s))
    signumFix x = if signum x == -1 then -1 else 1
    real = addition integralPart decimalPart

-- this function convers a decimal fraction into a golden real
fractionToGolden :: Integer -> Integer -> FNStream
fractionToGolden n d = division (integerToGolden n) (integerToGolden d)



------------------------------



-- FIBONACCI

fibNoRecursion :: Integer -> (Integer, [Bit])
fibNoRecursion n = (n - 1, digits (div (n - 1) 2) (if mod n 2 == 0 then [1] else [1, 1])) where
    digits 0 s = s
    digits x s = 1:0:0:0:digits (x - 1) s

fibRecursion :: Integer -> (Integer, [Bit])
fibRecursion 1 = (0, [1, 1])
fibRecursion 2 = (1, [1])
fibRecursion n = (f + 2, 1:0:0:0:fs) where
    (f, fs) = fibRecursion (n - 2)
