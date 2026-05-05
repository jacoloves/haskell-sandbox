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
  training30

training30 :: IO ()
training30 = do
  [n, k] <- getIntArray
  l <- getIntArray

  let ll = descendingSort l
  let ans = sum $ take k ll

  print ans

training29 :: IO ()
training29 = do
  n <- getInt
  a <- getIntArray
  if isPermutation n a
    then putStrLn "Yes"
    else putStrLn "No"

isPermutation :: Int -> [Int] -> Bool
isPermutation n a = sort a == [1 .. n]

training28 :: IO ()
training28 = do
  n <- getInt
  [t, a] <- getIntArray
  hs <- getIntArray

  let tmp = map (\h -> fromIntegral t - fromIntegral h * 0.006) hs
  let tmp2 = map (\x -> abs (x - fromIntegral a)) tmp
  let indexes = zip [1 ..] tmp2

  let (ans, _) = minimumBy (compare `on` snd) indexes
  print ans

training27 :: IO ()
training27 = do
  n <- getInt
  m <- replicateM n $ do
    line <- getLine
    let p = words line
    let name = head p
    let height = read (p !! 1) :: Int
    return (name, height)

  let sortP = sortBy (flip compare `on` snd) m
  let ans = fst (sortP !! 1)

  putStrLn ans

training26 :: IO ()
training26 = do
  n <- getInt
  h <- getIntArray
  let ans = countSeaViewHotes h
  print ans

countSeaViewHotes :: [Int] -> Int
countSeaViewHotes [] = 0
countSeaViewHotes (h : hs) = 1 + go h hs
 where
  go _ [] = 0
  go maxSoFar (x : xs)
    | x >= maxSoFar = 1 + go x xs
    | otherwise = go maxSoFar xs

training25 :: IO ()
training25 = do
  [n, x] <- getIntArray
  m <- replicateM n getInt

  let tmp1 = sum m
  let tmp2 = x - tmp1
  let minM = minimum m
  let addD = tmp2 `div` minM

  print (n + addD)

training24 :: IO ()
training24 = do
  [_, x] <- getIntArray
  ls <- getIntArray

  let p = scanl (+) 0 ls

  let ans = length $ filter (<= x) p

  print ans

training23 :: IO ()
training23 = do
  [n, d] <- getIntArray
  p <- replicateM n getIntArray

  let ans = length $ filter (\[x, y] -> x * x + y * y <= d * d) p
  print ans

training22 :: IO ()
training22 = do
  k <- getInt
  [aStr, bStr] <- getStrArray
  let a = fromBaseK k aStr
      b = fromBaseK k bStr
  print (a * b)

fromBaseK :: Int -> String -> Int
fromBaseK k str = foldl (\acc digit -> acc * k + digitToInt digit) 0 str
 where
  digitToInt c = read [c]

training21 :: IO ()
training21 = do
  [a, b] <- getIntArray

  print $ length $ filter isPalindrome [a .. b]

isPalindrome :: Int -> Bool
isPalindrome n = s == reverse s
 where
  s = show n

training20 :: IO ()
training20 = do
  n <- getInt
  let p = takeWhile (<= n) $ iterate (* 2) 1
  print $ last p

training19 :: IO ()
training19 = do
  [a, b] <- getIntArray
  let tmp =
        [ x | x <- [1 .. 2500], x * 8 `div` 100 == a, x * 10 `div` 100 == b
        ]

  if null tmp
    then print (-1)
    else print (head tmp)

training18 :: IO ()
training18 = do
  [a, b, k] <- getIntArray
  let smallSide = [a .. min b (a + k - 1)]
      largeSide = [max a (b - k + 1) .. b]
  let result = sort $ nub $ smallSide ++ largeSide

  mapM_ print result

training17 :: IO ()
training17 = do
  p <- getInt
  let factorials = map factorial [10, 9 .. 1]
  let (_, totalCoins) = foldl (useCoins) (p, 0) factorials
  print totalCoins

useCoins :: (Int, Int) -> Int -> (Int, Int)
useCoins (remaining, count) coin =
  let use = min 100 (remaining `div` coin)
   in (remaining - use * coin, count + use)

factorial :: Int -> Int
factorial n = product [1 .. n]

training16 :: IO ()
training16 = do
  [a, b, c, d] <- getIntArray
  let kA = (c + b - 1) `div` b
      kT = (a + d - 1) `div` d

  if kA <= kT
    then putStrLn "Yes"
    else putStrLn "No"

training15 :: IO ()
training15 = do
  [n, k] <- getIntArray

  let step x
        | x `mod` 200 == 0 = x `div` 200
        | otherwise = read (show x ++ "200")

  print $ iterate step n !! k

training14 :: IO ()
training14 = do
  x <- getBigInt

  let balances = iterate (\b -> b * 101 `div` 100) 100
      p1 = zip [1 ..] (tail balances)
      ans = fst $ head $ filter (\(_, x2) -> x2 >= x) p1

  print ans

training13 :: IO ()
training13 = do
  n <- getBigInt

  let savings = zip [1 ..] (scanl1 (+) [1 ..])
      ans = fst $ head $ filter (\(_, s) -> s >= n) savings

  print ans

training12 :: IO ()
training12 = do
  n <- getBigInt

  let ans = sum [x | x <- [1 .. n], x `mod` 3 /= 0, x `mod` 5 /= 0]

  print ans

training11 :: IO ()
training11 = do
  [n, x, t] <- getIntArray
  let divSeed = n `div` x

  if n `mod` x /= 0
    then print $ (divSeed + 1) * t
    else print $ divSeed * t

training10 :: IO ()
training10 = do
  n <- getInt
  let ans = n `div` 2

  if even n
    then print ans
    else print (ans + 1)

training09 :: IO ()
training09 = do
  n <- getInt

  let tmp1 = n `mod` 1000

  if tmp1 == 0
    then print 0
    else print (1000 - tmp1)

training08 :: IO ()
training08 = do
  [a, b, t] <- getIntArray

  let ans = b * (t `div` a)

  print ans

training07 :: IO ()
training07 = do
  [a, p] <- getIntArray

  let pA = a * 3
      ans = (pA + p) `div` 2

  print ans

training06 :: IO ()
training06 = do
  rgb <- getIntArray

  let rgb_str = concatMap show rgb
      rgb_int = read rgb_str :: Int

  if rgb_int `mod` 4 == 0
    then putStrLn "YES"
    else putStrLn "NO"

training05 :: IO ()
training05 = do
  x <- getInt
  a <- getInt
  b <- getInt

  let ans = (x - a) `mod` b

  print ans

training04Improv :: IO ()
training04Improv = do
  n <- getInt
  a <- getInt

  let ans
        | n `mod` 500 > a = "No"
        | otherwise = "Yes"

  putStrLn ans

training04 :: IO ()
training04 = do
  n <- getInt
  a <- getInt

  let tmp1 = n `mod` 500
  let ans
        | tmp1 > a = "No"
        | otherwise = "Yes"
  putStrLn ans

training03 :: IO ()
training03 = do
  [a, b] <- getIntArray
  let ans
        | b `mod` a == 0 = a + b
        | otherwise = b - a

  print ans

training02 :: IO ()
training02 = do
  [a, b] <- getIntArray

  let ans
        | a >= 13 = b
        | a >= 6 = b `div` 2
        | otherwise = 0

  print ans

training01Improv :: IO ()
training01Improv = do
  [a, b] <- getIntArray

  print $ maximum [a + b, a - b, a * b]

training01 :: IO ()
training01 = do
  [a, b] <- getIntArray

  let plusAB = a + b
      minusAB = a - b
      multiAB = a * b

  print $ maximum [plusAB, minusAB, multiAB]

-- descendingSort
descendingSort :: (Ord a) => [a] -> [a]
descendingSort = sortBy (flip compare)

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
