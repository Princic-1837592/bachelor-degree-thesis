module Utils.Utils where



-- iterate function for Integer type
iterateI :: (a -> a) -> a -> Integer -> a
iterateI f z 0 = z
iterateI f z i = f (iterateI f z (i-1))

-- length function for Integer type
lengthI :: [a] -> Integer
lengthI [] = 0
lengthI (x:xs) = 1 + lengthI xs

-- take function to support Integer argument
takeI :: Integer -> [a] -> [a]
takeI n [] = []
takeI n (x:xs) = if n <= 0 then [] else (x:takeI (n - 1) xs)
