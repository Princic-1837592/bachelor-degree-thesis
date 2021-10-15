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
perche' fa capire meglio la differenza tra le due parti
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
multiplication' :: FNStream -> FNStream -> FNStream
multiplication' (z, as) (t, bs) = (z + t + 2, sAddition (sMultiplication as bs) (sComplement (sAddition as bs 0 0)) 1 0)

-- funzione corretta da me
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



zeros :: SNStream
zeros = 0:zeros

ones :: SNStream
ones = 1:ones

minusOne :: FNStream
minusOne = (0, zeros)

zero :: FNStream
zero = (0, 1:1:zeros)

one :: FNStream
one = (1, 1:1:1:1:zeros)

two :: FNStream
two = (2, 1:1:0:1:1:1:zeros)

three :: FNStream
three = (2, 1:1:1:1:0:1:zeros)

four :: FNStream
four = (3, 1:1:0:1:0:1:0:1:zeros)

five :: FNStream
five = (3, 1:1:0:1:0:1:1:1:1:1:zeros)

six :: FNStream
six = (3, 1:1:0:1:1:1:0:1:1:1:zeros)

seven :: FNStream
seven = (3, 1:1:1:0:1:1:0:0:0:1:zeros)

eight :: FNStream
eight = (3, 1:1:1:1:0:1:0:0:0:1:zeros)

nine :: FNStream
nine = (3, 1:1:1:1:0:1:1:1:0:1:zeros)

ten :: FNStream
ten = (3, 1:1:1:1:1:1:0:1:0:1:zeros)

-- la funzione converte un intero in un reale in golden notation usando la seguente proprieta':
-- x = dn, ..., d1, d0
-- x = dn*10^n + ... + d1*10 + d0
-- grazie al tipo Integer l'input non ha limiti di grandezza
integerToGolden :: Integer -> FNStream
integerToGolden x = if signum x == 1 then real else multiplication minusOne real where
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

-- la funzione converte una coppia (integrale, decimale) in un reale in golden notation
-- grazie al tipo Integer l'input non ha limiti di grandezza
rationalToGolden :: Integer -> Integer -> FNStream
rationalToGolden i d = if signum i == signum d then real else multiplication minusOne real where
    digits = length . show . abs
    integralPart = integerToGolden (abs i)
    decimalPart = division (integerToGolden (abs d)) (iterate (multiplication ten) one !! (digits d))
    real = addition integralPart decimalPart



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
toString (z, as) n = "(-1" ++ (if length sums > 0 then "+" else "") ++ sums ++ ")*phi^(2*(" ++ show z ++ "))" where
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



main = do {
    putStrLn "\n\n\n\nphi = (sqrt(5)+1)/2;\n";
    a <- pure 5;
    b <- pure 354841599316846578573477239448643121619463341779640491211234;
    putStrLn $ show a ++ "." ++ show b ++ ";";
    putStrLn $ toKeisanCasio (integerToGolden a) 1000 "a";
    putStrLn $ toKeisanCasio (integerToGolden b) 1000 "b";
    putStrLn $ toKeisanCasio (rationalToGolden a b) 1000 "x";
}
