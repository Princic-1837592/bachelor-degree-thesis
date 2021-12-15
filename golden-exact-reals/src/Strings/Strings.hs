module Strings.Strings where
import Data.GoldenExactReals (FNStream, SNStream, Bit)
import Utils.Utils (takeI)



-- the second element is not infinite anymore
approx :: FNStream -> Integer -> (Integer, [Bit])
approx (z, as) n = (z, takeI n as)

stringApprox :: FNStream -> Integer -> String
stringApprox x n = "(" ++ show z ++ ", " ++ show as ++ ")" where
    (z, as) = approx x n

sToString :: SNStream -> Integer -> String -> String
sToString xs n p = foldr f "" (map (uncurry f') (filter (\x -> fst x == 1) (zip as [-1,-2..]))) where
    as = takeI n xs
    f x [] = x
    f x xs = x ++ ('+':xs)
    f' 1 i = p ++ "^(" ++ show i ++ ")"

-- phi = (sqrt(5)+1)/2
toString :: FNStream -> Integer -> String -> String
toString (z, as) n p = "(-1" ++ (if length sums > 0 then "+" else "") ++ sums ++ ")*"++ p ++ "^(2*(" ++ show z ++ "))" where
    sums = sToString as n p