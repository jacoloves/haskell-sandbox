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
  training023

training023 :: IO ()
training023 = do
  [x1, y1, x2, y2] <- getIntArray

  let dx = x2 - x1
      dy = y2 - y1

  let x3 = x2 - dy
      y3 = y2 + dx

  let x4 = x1 - dy
      y4 = y1 + dx

  putStrLn $ unwords $ map show [x3, y3, x4, y4]

training022 :: IO ()
training022 = do
  s <- getStr
  let threeDigits = [take 3 (drop i s) | i <- [0 .. length s - 3]]
  let differences = [abs (753 - read digits) | digits <- threeDigits]

  print $ minimum differences

training021 :: IO ()
training021 = do
  n <- getInt
  numbers <- getIntArray

  let counts = map countDivisions numbers

  print $ minimum counts

countDivisions :: Int -> Int
countDivisions n = go n 0
 where
  go num count
    | odd num = count
    | otherwise = go (num `div` 2) (count + 1)

training020 :: IO ()
training020 = do
  [n, a, b] <- getIntArray
  let cycle = a + b
  let fullCycles = n `div` cycle
  let remainder = n `mod` cycle
  let blueInFullCycles = fullCycles * a
      blueInRemainder = min remainder a
      totalBlue = blueInFullCycles + blueInRemainder

  print totalBlue

training019 :: IO ()
training019 = do
  h <- getInt
  print $ countAttacks h 0
 where
  countAttacks 0 acc = acc
  countAttacks h acc = countAttacks (h `div` 2) (2 * acc + 1)

training018 :: IO ()
training018 = do
  n <- getInt
  p <- getIntArray
  q <- getIntArray

  let allPerms = sort $ permutations [1 .. n]

  let indexP = fromJust $ elemIndex p allPerms
      indexQ = fromJust $ elemIndex q allPerms

  print $ abs (indexP - indexQ)

training017Adv :: IO ()
training017Adv = do
  [a, b, m] <- getIntArray
  refri <- getIntArray
  microw <- getIntArray

  let refArray = listArray (1, a) refri
      micArray = listArray (1, b) microw
      minRef = minimum refri
      minMic = minimum microw

  let noCoupon = minRef + minMic

  minWithCoupon <-
    foldM
      ( \acc _ -> do
          [x, y, c] <- getIntArray
          let price = refArray ! x + micArray ! y - c
          return $ min acc price
      )
      noCoupon
      [1 .. m]

  print minWithCoupon

training017 :: IO ()
training017 = do
  [a, b, m] <- getIntArray

  refri <- getIntArray
  microw <- getIntArray
  coupons <- replicateM m getIntArray

  let noCoupon = minimum refri + minimum microw

  let withCoupons =
        [ refri !! (x - 1) + microw !! (y - 1) - c
        | [x, y, c] <- coupons
        ]

  print $ minimum (noCoupon : withCoupons)

training016 :: IO ()
training016 = do
  n <- getInt
  [d, x] <- getIntArray
  periods <- replicateM n getInt

  let ans = sum [countChocolates d a | a <- periods]
  print $ ans + x

countChocolates :: Int -> Int -> Int
countChocolates d a = (d - 1) `div` a + 1

training015 :: IO ()
training015 = do
  [n, x] <- getIntArray
  candies <- sort <$> getIntArray
  let ans = maxHappyChildren x candies
  print ans

maxHappyChildren :: Int -> [Int] -> Int
maxHappyChildren x candies = go x candies 0
 where
  go remaining [] count = count
  go remaining (c : cs) count
    | remaining < c = count
    | remaining == c = count + 1
    | null cs = count
    | otherwise = go (remaining - c) cs (count + 1)

training014 :: IO ()
training014 = do
  x <- getInt
  let ans = head [n | n <- [x ..], isPrime n]
  print ans

isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = all (\d -> n `mod` d /= 0) [3, 5 .. limit]
 where
  limit = floor $ sqrt $ fromIntegral n

training013 :: IO ()
training013 = do
  s <- getInt
  let ans = findCycle s
  print ans

findCycle :: Int -> Int
findCycle s = go s Set.empty 1
 where
  go current seen index
    | current `Set.member` seen = index
    | otherwise = go (f current) (Set.insert current seen) (index + 1)

  f n = if even n then n `div` 2 else 3 * n + 1

training012 :: IO ()
training012 = do
  [n, m, x] <- getIntArray
  tollGates <- getIntArray

  let leftCost = length $ filter (< x) tollGates
      rightCost = length $ filter (> x) tollGates

  print $ min leftCost rightCost

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
