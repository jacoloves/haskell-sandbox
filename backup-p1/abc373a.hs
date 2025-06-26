import Control.Monad (replicateM)

main :: IO ()
main = do
  list <- replicateM 12 getLine
  print $ listCountCheck list

listCountCheck :: [String] -> Int
listCountCheck list = length $ filter (\(i, s) -> length s == i) indexedStrings
  where
    indexedStrings = zip [1 .. 12] list