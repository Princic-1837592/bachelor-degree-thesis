module Main (main) where
import Data.GoldenExactReals (integerToGolden, rationalToGolden)
import Strings.Strings (toString)



main = do {
    a <- pure $ rationalToGolden 98765 43201 1;
    putStrLn "phi = (sqrt(5)+1)/2;\nphi;\n";
    putStrLn $ "a = " ++ (toString a 1000 "phi") ++ ";\na;\n";
}