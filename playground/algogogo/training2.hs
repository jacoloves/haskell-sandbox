import Control.Monad (foldM, forM_, replicateM)
import Data.Array (Array, bounds, listArray, (!))
import Data.Binary.Get (getInt16be, remaining)
import Data.Char (chr, digitToInt, intToDigit, isDigit, isLower, isUpper, ord)
import Data.Function (on)
import Data.List (elemIndex, group, groupBy, isPrefixOf, isSuffixOf, minimumBy, nub, permutations, sort, sortBy, tails)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), comparing)
import Data.Set qualified as Set

main :: IO ()
main = do
  training63

training63 :: IO ()
training63 = do
  s <- getStr
  let n = length s
  let mid = [s !! i | i <- [2 .. n - 2]]

  let cond1 = head s == 'A'
  let cond2 = length (filter (== 'C') mid) == 1

  let outsideMiddle = [s !! 1] ++ [s !! (n - 1)]
  let cond3 =
        all isLower (filter (/= 'C') (drop 1 s))
          && all (/= 'C') outsideMiddle

  putStrLn $ if cond1 && cond2 && cond3 then "AC" else "WA"

training62 :: IO ()
training62 = do
  n <- getInt
  arr <- getIntArray

  let s = reverse $ sort arr
  let indexed = zip [0 ..] s

  let aliceScore = sum [card | (i, card) <- indexed, even i]
  let bobScore = sum [card | (i, card) <- indexed, odd i]

  print (aliceScore - bobScore)

training61 :: IO ()
training61 = do
  n <- getInt
  a <- getIntArray
  let ans = minimum $ map countDivisions a
  print ans

countDivisions :: Int -> Int
countDivisions n
  | even n = 1 + countDivisions (n `div` 2)
  | otherwise = 0

training60 :: IO ()
training60 = do
  [n, a, b] <- getIntArray
  let result =
        sum
          [ i
          | i <- [1 .. n]
          , let digitSum = sumOfDigits i
          , digitSum >= a && digitSum <= b
          ]

  print result

sumOfDigits :: Int -> Int
sumOfDigits 0 = 0
sumOfDigits n = n `mod` 10 + sumOfDigits (n `div` 10)

training59 :: IO ()
training59 = do
  k <- getInt
  [a, b] <- getIntArray
  if any (\i -> i `mod` k == 0) [a .. b]
    then putStrLn "OK"
    else putStrLn "NG"

training58 :: IO ()
training58 = do
  [h, a] <- getIntArray
  let ans = h `div` a
  let tmp = h `mod` a

  if tmp /= 0
    then print (ans + 1)
    else print ans

training57 :: IO ()
training57 = do
  [a, b] <- getIntArray
  if even (a * b)
    then putStrLn "Even"
    else putStrLn "Odd"

training56 :: IO ()
training56 = do
  [a, b] <- getIntArray
  print (a * b)

training55 :: IO ()
training55 = do
  [h, w] <- getIntArray
  grid <- replicateM h getStr

  let fr = filter (elem '#') grid
  let colHasHash j = any (\row -> row !! j == '#') grid
  let validCols = filter colHasHash [0 .. w - 1]

  let ans = map (\row -> map (row !!) validCols) fr

  mapM_ putStrLn ans

training54 :: IO ()
training54 = do
  [h, w, x, y] <- getIntArray
  grid <- replicateM h getStr
  let r = x - 1
      c = y - 1
  let up = countVisible grid r c (-1) 0
      down = countVisible grid r c 1 0
      left = countVisible grid r c 0 (-1)
      right = countVisible grid r c 0 1
  print (1 + up + down + left + right)

countVisible :: [String] -> Int -> Int -> Int -> Int -> Int
countVisible grid r c dr dc =
  let steps = tail $ iterate (\(ri, ci) -> (ri + dr, ci + dc)) (r, c)
      visible =
        takeWhile
          ( \(ri, ci) ->
              ri >= 0
                && ri < length grid
                && ci >= 0
                && ci < length (grid !! 0)
                && (grid !! ri) !! ci /= '#'
          )
          steps
   in length visible

training53 :: IO ()
training53 = do
  [h, w] <- getIntArray
  rows <- replicateM h getStr
  mapM_ putStrLn (concatMap (\row -> [row, row]) rows)

training52 :: IO ()
training52 = do
  r1 <- getIntArray
  r2 <- getIntArray
  r3 <- getIntArray
  let c = r1 ++ r2 ++ r3

  n <- getInt
  bs <- replicateM n getInt
  let called = Set.fromList bs

  let lines =
        [ [0, 1, 2]
        , [3, 4, 5]
        , [6, 7, 8]
        , [0, 3, 6]
        , [1, 4, 7]
        , [2, 5, 8]
        , [0, 4, 8]
        , [2, 4, 6]
        ]

  let bingo = any (isLine c called) lines
  putStrLn $ if bingo then "Yes" else "No"

isLine :: [Int] -> Set.Set Int -> [Int] -> Bool
isLine card called indices =
  all (\i -> Set.member (card !! i) called) indices

training51 :: IO ()
training51 = do
  s <- getStr
  t <- getStr
  let m = length t

  let w = map (take m) (tails s)
  let d = map (countDiff t) w
  print $ minimum $ filter (const True) (take (length s - m + 1) d)

countDiff :: String -> String -> Int
countDiff t w = length $ filter id $ zipWith (/=) t w

training50 :: IO ()
training50 = do
  [s, t] <- getIntArray
  let ans =
        [(a, b, c) | a <- [0 .. s], b <- [0 .. s], c <- [0 .. s], a + b + c <= s, a * b * c <= t]

  print $ length ans

training49 :: IO ()
training49 = do
  n <- getInt
  ls <- getIntArray

  let t = [(i, j, k) | i <- [0 .. n - 3], j <- [i + 1 .. n - 2], k <- [j + 1 .. n - 1]]
  let ans = length $ filter (isValid ls) t

  print ans

isValid :: [Int] -> (Int, Int, Int) -> Bool
isValid ls (i, j, k) =
  let [a, b, c] = sort [ls !! i, ls !! j, ls !! k]
   in a /= b && b /= c && a + b > c

training48 :: IO ()
training48 = do
  [n, d] <- getIntArray
  p <- replicateM n getIntArray

  let pairs = [(i, j) | i <- [0 .. n - 2], j <- [i + 1 .. n - 1]]
  let ans = length $ filter (isIntegerDist p) pairs
  print ans

isIntegerDist :: [[Int]] -> (Int, Int) -> Bool
isIntegerDist points (i, j) =
  let pi = points !! i
      pj = points !! j

      sumSq = sum $ zipWith (\a b -> (a - b) ^ 2) pi pj
      r = floor (sqrt (fromIntegral sumSq) :: Double)
   in r * r == sumSq

training47 :: IO ()
training47 = do
  n <- getInt

  let ans = any (\b -> (n - 7 * b) `mod` 4 == 0) [0 .. n `div` 7]
  putStrLn $ if ans then "Yes" else "No"

training46 :: IO ()
training46 = do
  n <- getInt

  let p = [i * j | i <- [1 .. 9], j <- [1 .. 9]]
  putStrLn $ if n `elem` p then "Yes" else "No"

training45 :: IO ()
training45 = do
  s <- getStr
  t <- getStr
  let sMin = sort s
  let tMax = sortBy (flip compare) t
  putStrLn $ if sMin < tMax then "Yes" else "No"

training44 :: IO ()
training44 = do
  [a, b] <- getIntArray
  let sa = replicate b (intToDigit a)
  let sb = replicate a (intToDigit b)
  putStrLn $ min sa sb

training43 :: IO ()
training43 = do
  s <- getStr
  t <- getStr
  let swapped = [swapAt i s | i <- [0 .. length s - 2]]
  let ans = s == t || any (== t) swapped
  putStrLn $ if ans then "Yes" else "No"

swapAt :: Int -> String -> String
swapAt i s =
  let (front, rest) = splitAt i s
   in case rest of
        (a : b : back) -> front ++ [b, a] ++ back
        _ -> s

training42 :: IO ()
training42 = do
  s <- getStr
  t <- getStr

  let d = s ++ s
  let ans = any (isPrefixOf t) (tails d)
  putStrLn $ if ans then "Yes" else "No"

training41 :: IO ()
training41 = do
  s <- getStr
  let candidates = [length s - 2, length s - 4 .. 2]
  let ans = head $ filter (isEvenString s) candidates
  print ans

isEvenString :: String -> Int -> Bool
isEvenString s n =
  let t = take n s
      h = n `div` 2
   in take h t == drop h t

training40 :: IO ()
training40 = do
  o <- getStr
  e <- getStr
  let z = concatMap (\(a, b) -> [a, b]) (zip o e)
  let ans =
        if length o > length e
          then z ++ [last o]
          else z

  putStrLn ans

training39 :: IO ()
training39 = do
  p <- getIntArray
  putStrLn $ map (\x -> chr (x - 1 + ord 'a')) p

training38 :: IO ()
training38 = do
  s <- getStr
  let h = take (length s `div` 2) s
  let hr = reverse s

  let mismathes = length $ filter id $ zipWith (/=) h hr
  print mismathes

training37 :: IO ()
training37 = do
  n <- getInt
  s <- getStr

  putStrLn $ map (shiftChar n) s

shiftChar :: Int -> Char -> Char
shiftChar n c =
  let offset = (ord c - ord 'A' + n) `mod` 26
   in chr (offset + ord 'A')

training36 :: IO ()
training36 = do
  [a, b] <- getIntArray
  s <- getStr

  let indexed = zip [0 ..] s

  let isValid = all (checkChar a) indexed

  putStrLn $ if isValid then "Yes" else "No"

checkChar :: Int -> (Int, Char) -> Bool
checkChar a (i, c)
  | i == a = c == '-'
  | otherwise = isDigit c

training35 :: IO ()
training35 = do
  s <- getStr

  if s == "Hello,World!"
    then putStrLn "AC"
    else putStrLn "WA"

training34 :: IO ()
training34 = do
  [n, m] <- getIntArray
  cities <- replicateM m $ do
    [p, y] <- getIntArray
    return (p, y)

  let indexed = zipWith (\i (p, y) -> (p, y, i)) [1 ..] cities

  let sorted = sortBy (comparing (\(p, y, _) -> (p, y))) indexed

  let grouped = groupBy (\(p1, _, _) (p2, _, _) -> p1 == p2) sorted

  let withRank = concatMap rankGroup grouped

  let result = sortBy (comparing (\(i, _, _) -> i)) withRank

  mapM_ (\(_, p, x) -> putStrLn $ formatID p x) result

rankGroup :: [(Int, Int, Int)] -> [(Int, Int, Int)]
rankGroup grp = zipWith (\x (_, _, i) -> (i, p, x)) [1 ..] grp
 where
  (p, _, _) = head grp

formatID :: Int -> Int -> String
formatID p x = pad6 p ++ pad6 x

pad6 :: Int -> String
pad6 n = replicate (6 - length s) '0' ++ s
 where
  s = show n

training33 :: IO ()
training33 = do
  [n, m] <- getIntArray
  shops <- replicateM n $ do
    [a, b] <- getBigIntArray
    return (a, b)

  let sorted = sortBy (compare `on` fst) shops

  let (_, ans) = foldl buy (fromIntegral m, 0) sorted
  print ans

buy :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
buy (remaining, cost) (a, b)
  | remaining <= 0 = (0, cost)
  | otherwise =
      let bought = min remaining b
       in (remaining - bought, cost + a * bought)

training32 :: IO ()
training32 = do
  [n, k] <- getIntArray
  hs <- replicateM n getInt

  let s = sort hs
  let d = zipWith (-) (drop (k - 1) s) s
  print $ minimum d

training31 :: IO ()
training31 = do
  n <- getInt
  d <- getIntArray

  let s = sort d
  let halfN = n `div` 2
  let lowerHalf = take halfN s
  let upperHalf = drop halfN s
  let maxLower = maximum lowerHalf
  let minUpper = minimum upperHalf

  let ans =
        if maxLower < minUpper
          then minUpper - maxLower
          else 0

  print ans

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
