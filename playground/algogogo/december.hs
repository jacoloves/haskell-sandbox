import Control.Monad (forM_, replicateM)
import Data.Array (Array, bounds, listArray, (!))
import Data.Char (isDigit, isLower, isUpper)
import Data.Function (on)
import Data.List (group, isSuffixOf, minimumBy, nub, sort, sortBy)
import Data.Map qualified as Map
import Data.Set qualified as Set

main :: IO ()
main = do
  training011

training011 :: IO ()
training011 = do
  s <- getStr
  let lengths = findAllACGTLengths s
  print $ if null lengths then 0 else maximum lengths

findAllACGTLengths :: String -> [Int]
findAllACGTLengths s = go s 0
  where
    go [] currentLen = [currentLen | currentLen > 0]
    go (c : cs) currentLen
      | isACGT c = go cs (currentLen + 1)
      | currentLen > 0 = currentLen : go cs 0
      | otherwise = go cs 0

    isACGT c = c `elem` "ACGT"

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