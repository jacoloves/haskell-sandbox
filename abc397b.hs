-- Control.Monad
-- replicateM
import Control.Monad (replicateM)

isSubsequence :: Eq a => [a] -> [a] -> Bool
isSubsequence [] _ = True
isSubsequence _ [] = False
isSubsequence (x : xs) (y : ys)
  | x == y = isSubsequence xs ys
  | otherwise = isSubsequence (x : xs) ys

solver :: String -> Int
solver s =
  let n = length s
      infPattern = take 300 (cycle "io")
      go l
        | isSubsequence s (take l infPattern) = l - n
        | otherwise = go (l + 2)
   in let startLength = if even n then n else n + 1
       in go startLength

main :: IO ()
main = do
  s <- getStr
  print $ solver s

-- read double Int score
readDoubleIntScore :: Int -> IO [(Int, Int)]
readDoubleIntScore n = replicateM n $ do
  [x, y] <- map read . words <$> getLine
  return (x, y)

-- read String Array
getStrArray :: IO [String]
getStrArray = words <$> getLine

-- read Int
getInt :: IO Int
getInt = readLn :: IO Int

-- read String
getStr :: IO String
getStr = getLine

-- read Int array
getIntArray :: IO [Int]
getIntArray = map read . words <$> getLine :: IO [Int]