import Control.Monad (replicateM)
import Data.List (nub)

main :: IO ()
main = do
  n <- readLn :: IO Int
  diameters <- replicateM n (readLn :: IO Int)
  print $ maxKagamiMochi diameters

maxKagamiMochi :: [Int] -> Int
maxKagamiMochi = length . nub