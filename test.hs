import System.Random

randomSNStream gen = (mod bit 2):randomSNStream gen' where (bit, gen') = next gen

pureGen = mkStdGen 137


main = do {
    print $ take 100 (randomSNSStream pureGen);
}