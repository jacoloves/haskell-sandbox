import Control.Monad (replicateM)

main :: IO ()
main = do
  input <- getLine
  let [n, c] = map read . words $ input :: [Int]
  t <- replicateM n (readLn :: IO Int)
  print $ abc376A c t

abc376A :: Int -> [Int] -> Int
abc376A c times = go times 0 0
 where
  go [] _ count = count
  go (t : ts) lastTime count
    | t - lastTime >= c = go ts t (count + 1)
    | otherwise = go ts t count
