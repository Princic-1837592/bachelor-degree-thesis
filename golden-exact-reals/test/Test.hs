module Main (main) where
import Data.GoldenExactReals (integerToGolden, rationalToGolden, fractionToGolden)
import Strings.Strings (toString)



main = do {
    a <- pure $ rationalToGolden 98765 43201 1;
    b <- pure $ fractionToGolden 98765 43210;
    putStrLn "phi = (sqrt(5)+1)/2;\nphi;\n";
    -- putStrLn $ "a = " ++ (toString a 1000 "phi") ++ ";\na;\n";
    putStrLn $ "b = " ++ (toString b 3000 "phi") ++ ";\n98765/43210;\nb;\n";
}