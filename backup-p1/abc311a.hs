-- Control.Monad
-- replicateM
import Control.Monad (replicateM)
-- Data.Set
-- Set
import Data.Set (Set)
import Data.Set qualified as Set

solver :: String -> Int
solver s = go s Set.empty 1
  where
    go [] _ _ = 0
    go (x : xs) seen idx
      | Set.size newSeen == 3 = idx
      | otherwise = go xs newSeen (idx + 1)
      where
        newSeen = Set.insert x seen

main :: IO ()
main = do
  _ <- getInt
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

-- read Double
getDouble :: IO Double
getDouble = readLn :: IO Double