import Control.Monad (foldM, forM_, replicateM)
import Data.Array (Array, bounds, listArray, (!))
import Data.Binary.Get (getInt16be, remaining)
import Data.Char (digitToInt, isDigit, isLower, isUpper)
import Data.Function (on)
import Data.List (elemIndex, group, isSuffixOf, minimumBy, nub, permutations, sort, sortBy)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), comparing)
import Data.Set qualified as Set

main :: IO ()
main = do
  training019

training019 :: IO ()
training019 =
  do
    [_n, a, b] <- getIntArray
    s <- getStr

    let states = scanl (training019update a b) (0, 0) s
    mapM_
      (putStrLn . training019Judge a b)
      (zip s states)

training019update :: Int -> Int -> (Int, Int) -> Char -> (Int, Int)
training019update a b (total, overseas) c
  | c == 'a' && total < a + b = (total + 1, overseas)
  | c == 'b' && total < a + b && overseas < b = (total + 1, overseas + 1)
  | otherwise = (total, overseas)

training019Judge :: Int -> Int -> (Char, (Int, Int)) -> String
training019Judge a b (c, (total, overseas))
  | c == 'a' && total < a + b = "Yes"
  | c == 'b' && total < a + b && overseas < b = "Yes"
  | otherwise = "No"

training018 :: IO ()
training018 = do
  _n <- getInt
  xs <- getIntArray

  let lo = minimum xs
      hi = maximum xs

  let cost p = sum $ map (\x -> (x - p) ^ 2) xs

  print $ minimum $ map cost [lo .. hi]

training017 :: IO ()
training017 = do
  [a, b] <- getIntArray
  let result = (b - 1 + (a - 2)) `div` (a - 1)
  print result

training016 :: IO ()
training016 = do
  n <- getInt
  as <- getIntArray

  let arr = listArray (0, n - 1) (map (subtract 1) as) :: Array Int Int
  let count =
        length
          [i | i <- [0 .. n - 1], let j = arr ! i, j > i, arr ! j == i]
  print count

training015 :: IO ()
training015 = do
  [n, k] <- getIntArray
  print $ k * (k - 1) ^ (n - 1)

training014 :: IO ()
training014 = do
  [a, b, c] <- getBigIntArray

  let diffA = if even a then 0 else b * c
      diffB = if even b then 0 else a * c
      diffC = if even c then 0 else a * b

  print $ minimum [diffA, diffB, diffC]

training013 :: IO ()
training013 =
  do
    [n, m] <- getIntArray
    stores <- replicateM n $ do
      [a, b] <- getIntArray
      return (fromIntegral a :: Integer, fromIntegral b :: Integer)

    let sorted = sortBy (comparing fst) stores
    let mI = fromIntegral m :: Integer
        (_, totalCost) = foldl buyGreedy (mI, 0) sorted
    print totalCost

buyGreedy :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
buyGreedy (remaining, cost) (a, b)
  | remaining <= 0 = (0, cost)
  | otherwise =
      let bought = min remaining b
       in (remaining - bought, cost + a * bought)

training012 :: IO ()
training012 = do
  n <- getInt
  as <- getIntArray
  let total = 3 ^ n
  let oddChoices = map (\a -> if even a then 2 else 1) as
  let allOdd = product oddChoices
  print (total - allOdd)

training011 :: IO ()
training011 = do
  [a, b, c] <- getIntArray
  let m0 = maximum [a, b, c]
      cost m =
        let d = (m - a) + (m - b) + (m - c)
         in if even d then Just (d `div` 2) else Nothing
      candidates = [cost m | m <- [m0, m0 + 1, m0 + 2]]
      answer = minimum [v | Just v <- candidates]

  print answer

training010 :: IO ()
training010 = do
  s <- getStr
  let [a, b, c, d] = map digitToInt s
  let opCombinations = replicateM 3 "+-"

  let Just ops =
        lookup
          True
          [ (a + s1 * b + s2 * c + s3 * d == 7, [op1, op2, op3])
            | [op1, op2, op3] <- opCombinations,
              let s1 = if op1 == '+' then 1 else -1,
              let s2 = if op2 == '+' then 1 else -1,
              let s3 = if op3 == '+' then 1 else -1
          ]

  let [op1, op2, op3] = ops
  putStrLn $ show a ++ [op1] ++ show b ++ [op2] ++ show c ++ [op3] ++ show d ++ "=7"

training009 :: IO ()
training009 = do
  _n <- getInt
  as <- getIntArray

  let totalSum = sum as
  let prefixSums = init $ scanl1 (+) as
  let costs = map (\p -> abs (2 * p - totalSum)) prefixSums
  print $ minimum costs

training008 :: IO ()
training008 = do
  s <- getStr
  let zeros = length $ filter (== '0') s
  let ones = length $ filter (== '1') s

  print $ 2 * min zeros ones

training007 :: IO ()
training007 = do
  n <- getInt
  coords <- replicateM n $ do
    [x, y] <- getIntArray
    return (fromIntegral x :: Double, fromIntegral y :: Double)

  let dist (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

  let pathLen perm =
        let pts = map (coords !!) perm
         in sum $ zipWith dist pts (tail pts)

  let perms = permutations [0 .. n - 1]
      total = sum $ map pathLen perms
      avg = total / fromIntegral (length perms)

  putStrLn $ show avg

training006 :: IO ()
training006 = do
  n <- getInt
  let modVal = 10 ^ 9 + 7

  let result = foldl (\acc i -> (acc * i) `mod` modVal) 1 [1 .. n]
  print result

training005 :: IO ()
training005 = do
  [x, y] <- getBigIntArray

  print $ length $ takeWhile (<= y) $ iterate (* 2) x

training004 :: IO ()
training004 = do
  n <- getInt
  blues <- getNLines n
  m <- getInt
  reds <- getNLines m

  let blueMap = Map.fromListWith (+) [(s, 1) | s <- blues]
      redMap = Map.fromListWith (+) [(s, 1) | s <- reds]

      scores =
        [ Map.findWithDefault 0 s blueMap
            - Map.findWithDefault 0 s redMap
          | s <- Map.keys blueMap
        ]

  print $ maximum (0 : scores)

training003 :: IO ()
training003 = do
  _n <- getInt
  as <- getIntArray

  print $ sum $ map (\x -> x - 1) as

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
