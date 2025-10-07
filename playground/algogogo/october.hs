import Control.Monad (forM_, replicateM)
import Data.Char (isDigit, isLower, isUpper)
import Data.Function (on)
import Data.List (isSuffixOf, minimumBy, nub, sort, sortBy)
import Data.Map qualified as Map
import Text.Printf (printf)

main :: IO ()
main = do
  abc416a

abc415a :: IO ()
abc415a = do
  n <- getInt
  a <- getIntArray
  x <- getInt
  let res = checkContains a x
  putStrLn res

checkContains :: [Int] -> Int -> String
checkContains a x
  | x `elem` a = "Yes"
  | otherwise = "No"

abc416a :: IO ()
abc416a = do
  [n, l, r] <- getIntArray
  s <- getStr
  let ans = checkVacation l r s
  putStrLn ans

checkVacation :: Int -> Int -> String -> String
checkVacation l r s =
  let substring = extractRange l r s
   in if all (== 'o') substring
        then "Yes"
        else "No"

extractRange :: Int -> Int -> String -> String
extractRange l r s =
  let start = l - 1
      count = r - l + 1
   in take count (drop start s)

abc417a :: IO ()
abc417a = do
  [n, a, b] <- getIntArray
  s <- getStr
  let ans = removeEnds a b s
  putStrLn ans

removeEnds :: Int -> Int -> String -> String
removeEnds a b s =
  let afterDropFront = drop a s
      lengthToTake = length s - a - b
   in take lengthToTake afterDropFront

abc418a :: IO ()
abc418a = do
  n <- getInt
  s <- getStr
  let ans = endsWithTea s
  putStrLn ans

endsWithTea :: String -> String
endsWithTea s
  | "tea" `isSuffixOf` s = "Yes"
  | otherwise = "No"

abc419a :: IO ()
abc419a = do
  s <- getStr
  let ans = checkAtcoderLang s
  putStrLn ans

checkAtcoderLang :: String -> String
checkAtcoderLang s
  | s == "red" = "SSS"
  | s == "blue" = "FFF"
  | s == "green" = "MMM"
  | otherwise = "Unknown"

abc420a :: IO ()
abc420a = do
  [x, y] <- getIntArray
  let ans = checkXplusY x y
  print ans

checkXplusY :: Int -> Int -> Int
checkXplusY x y =
  let ansXY = x + y
   in if ansXY > 12
        then ansXY - 12
        else ansXY

abc421a :: IO ()
abc421a = do
  n <- getInt
  regidents <- replicateM n getStr
  [xStr, y] <- getStrArray
  let x = read xStr :: Int
  let r = checkD regidents x y
  putStrLn r

checkD :: [String] -> Int -> String -> String
checkD regidents x y =
  let actualResident = regidents !! (x - 1)
   in if actualResident == y
        then "Yes"
        else "No"

abc424a :: IO ()
abc424a = do
  [a, b, c] <- getIntArray
  let ans = if a == b || b == c || c == a then "Yes" else "No"
  putStrLn ans

abc425a :: IO ()
abc425a = do
  n <- getInt
  let r = f n
  print r

f :: Int -> Int
f n = sum [f2 i | i <- [1 .. n]]

f2 :: Int -> Int
f2 i = (-1) ^ i * i ^ 3

hello :: IO ()
hello = do
  print "hello"

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
