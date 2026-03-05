import Control.Monad (foldM, forM_, replicateM)
import Data.Array (Array, bounds, listArray, (!))
import Data.Binary.Get (remaining)
import Data.Char (digitToInt, isDigit, isLower, isUpper)
import Data.Function (on)
import Data.List (elemIndex, group, isSuffixOf, minimumBy, nub, permutations, sort, sortBy)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), comparing)
import Data.Set qualified as Set

main :: IO ()
main = do
  training002

training002 :: IO ()
training002 = do
  s <- getStr
  k <- getBigInt
  let bigNum = k + 1
      widths = map (\c -> if c == '1' then 1 else bigNum) s
      cumulative = scanl1 (+) (map (min bigNum) widths)
      result = fst $ head $ filter (\(_, cum) -> cum >= k) (zip s cumulative)
  putChar result
  putStrLn ""

training001 :: IO ()
training001 = do
  [d, n] <- getIntArray
  let notDiv100 = filter (\x -> x `mod` 100 /= 0) [1 ..]
  let nthNotDiv = notDiv100 !! (n - 1)
  print (nthNotDiv * 100 ^ d)

-- read big Int
getBigInt :: IO Integer
getBigInt = readLn :: IO Integer

-- read big Int Array
getBigIntArray :: IO [Integer]
getBigIntArray = map read . words <$> getLine :: IO [Integer]

-- read Float
getFloat :: IO Float
getFloat = readLn :: IO Float

-- readt String NLines
getNLines :: Int -> IO [String]
getNLines n = replicateM n getLine

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

-- read Int Array
getIntArray :: IO [Int]
getIntArray = map read . words <$> getLine :: IO [Int]

-- read Double
getDouble :: IO Double
getDouble = readLn :: IO Double
