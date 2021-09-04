import System.Random

randomSNSStream gen = (mod bit 2):randomSNSStream gen' where (bit, gen') = next gen

pureGen = mkStdGen 137


main = do {
    print $ take 100 (randomSNSStream pureGen);
}