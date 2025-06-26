-- Control.Monad
-- replicateM
import Control.Monad (replicateM)

pi100 :: String
pi100 = "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"

solver :: Int -> String
solver n = take (n + 2) pi100

main :: IO ()
main = do
  n <- getInt
  putStrLn $ solver n

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