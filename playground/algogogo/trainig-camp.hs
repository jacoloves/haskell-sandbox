-- !!! Traning Camp in Kanazawa !!!

import Control.Monad (forM_, replicateM)
import Data.Char (isDigit, isLower, isUpper)
import Data.Function (on)
import Data.List (minimumBy, nub, sort, sortBy)
import Data.Map qualified as Map
import Text.Printf (printf)

main :: IO ()
main = do
  abc421a

abc421a :: IO ()
abc421a = do
  n <- getInt
  sArr <- getStrArray
  x <- getInt
  y <- getStr
  let result = f sArr x y
  putStrLn result

f :: [String] -> Int -> String -> String
f sArr x y =
  let ar = sArr !! (x - 1)
   in if ar == y
        then "Yes"
        else "No"

abc422a :: IO ()
abc422a = do
  s <- getStr
  let res = f s
  putStrLn res

f :: String -> String
f s =
  let (w, st) = f2 s
   in if st < 8
        then f3 w (st + 1)
        else f3 (w + 1) 1

f2 :: String -> (Int, Int)
f2 s =
  let p = words $ map (\c -> if c == '-' then ' ' else c) s
      w = read (head p) :: Int
      st = read (p !! 1) :: Int
   in (w, st)

f3 :: Int -> Int -> String
f3 w st = show w ++ "-" ++ show st

abc423a :: IO ()
abc423a = do
  [x, c] <- getIntArray
  let res = maxDrawal x c
  print res

maxDrawal :: Int -> Int -> Int
maxDrawal x c =
  let tfr = 1000 + c
      mawf = (x * 1000) `div` tfr
      mtu = mawf `div` 1000
   in mtu * 1000

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
