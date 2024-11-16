import Data.List (group, sort)

main :: IO ()
main = do
  input <- getLine
  let [a, b, c, d] = map read . words $ input :: [Int]
  print $ maxOperations [a, b, c, d]

maxOperations :: [Int] -> Int
maxOperations = sum . map (\x -> length x `div` 2) . group . sort
