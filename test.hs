-- simplified notation stream
type SNStream = [Int]

{-
full notation stream

(z, a) = (-1 + Σ ai * phi^-i) * phi^2z

ho preferito usare una coppia invece che un unico stream
perché fa capire meglio la differenza tra le due parti
-}
type FNStream = (Int, SNStream)

approx :: FNStream -> Int -> FNStream
approx (z, as) n = (z, take n as)

printApprox :: FNStream -> Int -> String
printApprox x n = "(" ++ show z ++ ", " ++ show as ++ ")" where
    (z, as) = approx x n

-- simplified addition
-- Definition 6: A
sAddition :: SNStream -> SNStream -> Int -> Int -> SNStream
sAddition (  0:as)  (  0:bs)  0 b =   0:sAddition    as     bs        b 0
sAddition (0:0:as)  (  0:bs)  1 b =   0:sAddition (b:as)    bs        1 1
sAddition (0:1:as)  (0:1:bs)  1 1 = 1:0:sAddition    as     bs        0 1
sAddition (0:0:as)  (1:0:bs)  1 0 = 0:1:sAddition    as     bs        1 0
sAddition (  0:as)  (  1:bs)  1 1 =   1:sAddition    as     bs        0 0
sAddition (  1:as)  (  1:bs)  1 b =   1:sAddition    as     bs        b 1

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
sComplement (x:as) = (1-x):sComplement as

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
-- Definition 11 P
sMultiplication :: SNStream -> SNStream -> SNStream
sMultiplication (  0:as) (    bs) = 0:sMultiplication as bs
sMultiplication (    as) (  0:bs) = 0:sMultiplication as bs
sMultiplication (1:0:as) (1:0:bs) = 0:sAddition (sAddition    as     bs  0 0) (  0:sMultiplication as bs) 1 0
sMultiplication (1:1:as) (1:0:bs) =   sAddition (sAddition (0:as)    bs  0 0) (0:0:sMultiplication as bs) 1 0
sMultiplication (1:0:as) (1:1:bs) =   sAddition (sAddition    as  (0:bs) 0 0) (0:0:sMultiplication as bs) 1 0
sMultiplication (1:1:as) (1:1:bs) =   sAddition (sAddition    as     bs  0 0) (0:0:sMultiplication as bs) 1 1

-- full multiplication
-- Definition 12 P'
multiplication :: FNStream -> FNStream -> FNStream
multiplication (z, as) (t, bs) = (z+t+2, sAddition (sMultiplication as bs) (sComplement (sAddition as bs 0 0)) 1 0)



---------------------------------------------



toWolfram :: FNStream -> Int -> String
toWolfram x n = "(-1+" ++ sums ++ ")*" ++ sphi ++ "^(2*" ++ show z ++ ")" where
    (z, as) = approx x n
    f' 1 i = sphi ++ "^(" ++ show i ++ ")"
    f' 0 _ = ""
    f x [] = x
    f x xs = x++('+':xs)
    sphi = "((sqrt(5)+1)/2)"
    sums = foldr f "" (filter (\s -> length s > 0) (map (uncurry f') (zip as [-1,-2..])))


-- phi = (sqrt(5)+1)/2
toString :: FNStream -> Int -> String
toString x n = "\nphi = (sqrt(5)+1)/2;\n(-1+" ++ sums ++ ")*phi^(2*" ++ show z ++ ");\n" where
    (z, as) = approx x n
    f x [] = x
    f x xs = x ++ ('+':xs)
    f' 1 i = "phi^(" ++ show i ++ ")"
    f' 0 _ = ""
    sums = foldr f "" (filter (\s -> length s > 0) (map (uncurry f') (zip as [-1,-2..])))

zeros :: SNStream
zeros = 0:zeros

ones :: SNStream
ones = 1:ones

-- (-1 + (((sqrt(5)+1)/2)^(-1) + ((sqrt(5)+1)/2)^(-2) + ((sqrt(5)+1)/2)^(-3))) * ((sqrt(5)+1)/2)^(2*3)
x :: FNStream
x = (3, 1:1:1:zeros)

-- (-1 + (((sqrt(5)+1)/2)^(-2))) * ((sqrt(5)+1)/2)^(2*5)
y :: FNStream
y = (5, 0:1:zeros)

-- ((-1 + (((sqrt(5)+1)/2)^(-1) + ((sqrt(5)+1)/2)^(-5) + ((sqrt(5)+1)/2)^(-6) + ((sqrt(5)+1)/2)^(-9))) * ((sqrt(5)+1)/2)^(2*6))
z :: FNStream
z = addition x y

y' :: FNStream
y' = complement y

maybeZero :: FNStream
maybeZero = addition y y'

maybeZero' :: FNStream
maybeZero' = subtraction x x

xy :: FNStream
xy = multiplication x y

xx :: FNStream
xx = multiplication x x

a = (0, 1:zeros)
b = (0, 0:1:zeros)
ab = multiplication a b

maybeThree = (2, 0:1:0:0:0:1:zeros)

main = do {
    -- putStrLn $ "  x = " ++ printApprox  x 10;
    -- putStrLn $ "  y = " ++ printApprox  y 10;
    -- putStrLn $ " xy = " ++ printApprox xy 20;
    -- putStrLn $ " xx = " ++ printApprox xx 20;
    -- putStrLn "\n\n\n";
    -- putStrLn $ " x:" ++ toString x 10;
    -- putStrLn $ " y:" ++ toString y 10;
    -- putStrLn $ "xy:" ++ toString xy 200;
    -- putStrLn $ "xx:" ++ toString xx 200;
    -- putStrLn "\n\n\n\n\n";
    -- putStrLn $ "  a = " ++ printApprox  a 10;
    -- putStrLn $ "  b = " ++ printApprox  b 10;
    -- putStrLn $ " ab = " ++ printApprox ab 10;
    -- putStrLn $ " a:" ++ toString a 10;
    -- putStrLn $ " b:" ++ toString b 10;
    -- putStrLn $ "ab:" ++ toString ab 200;
    putStrLn $ toString maybeThree 20;
    print True;
}
