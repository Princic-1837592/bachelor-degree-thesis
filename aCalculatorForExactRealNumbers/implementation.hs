type SignedBinaryDigit = Int

type SBDStream = [SignedBinaryDigit]

type DyadicDigit = (Int, Int)

type DDStream = [DyadicDigit]

type SBDME = (SBDStream, Int)

type DDME = (DDStream, Int)

lowest_terms :: DyadicDigit -> DyadicDigit
lowest_terms (0, b) = (0, 0)
lowest_terms (a, b) = if mod a 2==1 then (a, b) else lowest_terms (div a 2, b-1)

digit_av :: DyadicDigit -> DyadicDigit -> DyadicDigit
digit_av (a, b) (c, d) = if b>d then (a+c*2^(b-d), b+1) else (if d>b then (a*2^(d-b)+c, d+1) else undefined)

average :: DDStream -> DDStream -> DDStream
average (a:as) (b:bs) = digit_av a b:average as bs

d_greater_than :: DyadicDigit -> DyadicDigit -> Bool
d_greater_than x y = a' > c' where
    (a , b)  = lowest_terms x
    (c , d)  = lowest_terms y
    (a', c') = if b==d then (a, c) else (if b>d then (a, c*2^(b-d)) else (a*2^(d-b), c))

d_greater_or_equal :: DyadicDigit -> DyadicDigit -> Bool
d_greater_or_equal x y = (a==c && b==d) || d_greater_than x' y' where
    x'@(a, b) = lowest_terms x
    y'@(c, d) = lowest_terms y

s_to_d :: SBDStream -> DDStream
s_to_d [] = []
s_to_d ( 1:x)  = ddOne     :s_to_d x
s_to_d ( 0:x)  = ddZero    :s_to_d x
s_to_d (-1:x) = ddMinusOne:s_to_d x

-- d_to_s :: DDStream -> SBDStream
-- d_to_s ((a, b):(c, d):x) = if fromIntegral a' / (2^b')>=1/4 then 1:d_to_s (():x) else (if undefined then undefined else undefined)
--     where (a', b') = digit_av (a, b) (c, d+1)

ddZero :: DyadicDigit
ddZero = (0, 0)

ddOne :: DyadicDigit
ddOne = (1, 0)

ddMinusOne :: DyadicDigit
ddMinusOne = (-1, 0)

ddsZero :: DDStream
ddsZero = ddZero:ddsZero

x :: SBDStream
x = [1, 1, -1, 0]



main = do {
    print (s_to_d x);
    print ();
}
