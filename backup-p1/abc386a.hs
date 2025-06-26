import Data.List (group, sort)

main :: IO ()
main = do
  input <- getLine
  let cards = map read $ words input :: [Int]
  putStrLn $ solve cards

solve :: [Int] -> String
solve cards
  | any isFullHouse possibleHands = "Yes"
  | otherwise = "No"
  where
      possibleHands = [cards ++ [x] | x <- [1..13]]
      isFullHouse hand = let counts = map length . group . sort $ hand
                         in sort counts == [2, 3]
