import Control.Monad (forM_, replicateM)
import Data.Array (Array, listArray, (!))
import Data.Char (isDigit, isLower, isUpper)
import Data.Function (on)
import Data.List (group, isSuffixOf, minimumBy, nub, sort, sortBy)
import Data.Map qualified as Map
import Text.Printf (printf)

main :: IO ()
main = do
  abc430a

abc430a :: IO ()
abc430a = do
  [a, b, c, d] <- getIntArray
  let ans = candyCookie a b c d
  putStrLn ans

candyCookie :: Int -> Int -> Int -> Int -> String
candyCookie a b c d
  | a <= c && b <= d = "No"
  | a > c = "No"
  | otherwise = "Yes"

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
