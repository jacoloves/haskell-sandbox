import Control.Monad (foldM, forM_, replicateM)
import Data.Array (Array, bounds, listArray, (!))
import Data.Char (isDigit, isLower, isUpper)
import Data.Function (on)
import Data.List (elemIndex, group, isSuffixOf, minimumBy, nub, permutations, sort, sortBy)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set

main :: IO ()
main = do
  training029

training029 :: IO ()
training029 = do
  s <- getStr
  let allChars = ['a' .. 'z']
  let miss = filter (`notElem` s) allChars
  putStrLn $ case miss of
    (c : _) -> [c]
    [] -> "None"

training028 :: IO ()
training028 = do
  a <- getInt
  b <- getInt
  c <- getInt
  x <- getInt

  let cnt =
        length
          [(i, j, k) | i <- [0 .. a], j <- [0 .. b], k <- [0 .. c], 500 * i + 100 * j + 50 * k == x]

  print cnt

training027 :: IO ()
training027 = do
  [a, b] <- getIntArray
  s <- getStr
  let (before, after) = splitAt a s
  let isValid = case after of
        ('-' : rest) -> all isDigit before && all isDigit rest
        _ -> False
  putStrLn $ if isValid then "Yes" else "No"

training026 :: IO ()
training026 = do
  putStrLn "Hello"

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
