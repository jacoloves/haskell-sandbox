import Control.Monad (forM_, replicateM)
import Data.Char (isLower, isUpper, isDigit)
import Data.Function (on)
import Data.List (minimumBy, nub, sort, sortBy)
import Data.Map qualified as Map
import Text.Printf (printf)

main :: IO ()
main = do
  abc084b

abc084b :: IO ()
abc084b = do
  [a, b] <- getIntArray 
  s <- getStr
  if isValidPostalCode a b s
    then putStrLn "Yes"
    else putStrLn "No"

isValidPostalCode :: Int -> Int -> String -> Bool
isValidPostalCode a b s =
  let hyphenPos = a
      (prefix, rest) = splitAt hyphenPos s
      (hyphen, suffix) = splitAt 1 rest
  in length s == a + b + 1 &&
     all isDigit prefix &&
     hyphen == "-" &&
     all isDigit suffix

abc215a :: IO ()
abc215a = do
  s <- getStr
  if judgeHelloWorld s
    then putStrLn "AC"
    else putStrLn "WA"

judgeHelloWorld :: String -> Bool
judgeHelloWorld s =
  "Hello,World!" == s

abc104b :: IO ()
abc104b = do
  s <- getStr
  if isValidString s
    then putStrLn "AC"
    else putStrLn "WA"

isValidString :: String -> Bool
isValidString s =
  let len = length s
   in isFirstCharA s
        && hasExactlyOneCInRange s
        && areOtherCharsLowercase s

isFirstCharA :: String -> Bool
isFirstCharA [] = False
isFirstCharA (x : _) = x == 'A'

hasExactlyOneCInRange :: String -> Bool
hasExactlyOneCInRange s =
  let len = length s
   in if len < 4
        then False
        else
          let startIndex = 2
              endIndex = len - 2
              rangeChars = take (endIndex - startIndex + 1) (drop startIndex s)
              cCount = length $ filter (== 'C') rangeChars
           in cCount == 1

areOtherCharsLowercase :: String -> Bool
areOtherCharsLowercase s =
  let len = length s
      otherChars = drop 1 s
      isValidChar i c
        | i >= 2 && i <= len - 2 && c == 'C' = True
        | otherwise = isLower c
   in all (\(i, c) -> isValidChar i c) (zip [1 ..] otherChars)

abc113c :: IO ()
abc113c = do
  [n, m] <- getIntArray
  cities <- replicateM m $ do
    [p, y] <- getIntArray
    return (p, y)

  let res = generateCityIds cities
  mapM_ putStrLn res

generateCityIds :: [(Int, Int)] -> [String]
generateCityIds cities =
  let
    indexedCities = zip [0 ..] cities

    prefectureGroups = groupByPrefecture indexedCities

    cityOrders = Map.fromList $ concatMap assignOrders (Map.toList prefectureGroups)

    cityIds = map (generateId cityOrders) indexedCities
   in
    cityIds

groupByPrefecture :: [(Int, (Int, Int))] -> Map.Map Int [(Int, (Int, Int))]
groupByPrefecture cities =
  foldr
    ( \city@(_, (prefecture, _)) acc ->
        Map.insertWith (++) prefecture [city] acc
    )
    Map.empty
    cities

assignOrders :: (Int, [(Int, (Int, Int))]) -> [(Int, (Int, Int))]
assignOrders (prefecture, citiesInPrefecture) =
  let
    sortedCities = sortBy (compare `on` (\(_, (_, year)) -> year)) citiesInPrefecture
    orderedCities = zip [1 ..] sortedCities
   in
    map
      ( \(order, (originalIndex, (pref, year))) ->
          (originalIndex, (pref, order))
      )
      orderedCities

generateId :: Map.Map Int (Int, Int) -> (Int, (Int, Int)) -> String
generateId orderMap (originalIndex, (prefecture, year)) =
  case Map.lookup originalIndex orderMap of
    Just (pref, order) -> printf "%06d%06d" pref order
    Nothing -> error "City not found in order map"

abc121c :: IO ()
abc121c = do
  [n, m] <- getIntArray
  shops <- replicateM n $ do
    [a, b] <- getIntArray
    return (a, b)

  let res = minCostToBuyDrinks m shops
  print res

minCostToBuyDrinks :: Int -> [(Int, Int)] -> Int
minCostToBuyDrinks targetCount shops =
  let sortedShops = sortBy (compare `on` fst) shops
   in buyGreedily targetCount sortedShops

buyGreedily :: Int -> [(Int, Int)] -> Int
buyGreedily 0 _ = 0
buyGreedily _ [] = 0
buyGreedily remaining ((price, stock) : rest) =
  let buyCount = min remaining stock
      cost = price * buyCount
      newRemaining = remaining - buyCount
   in cost + buyGreedily newRemaining rest

abc115c :: IO ()
abc115c = do
  [n, k] <- getIntArray
  heights <- replicateM n getInt
  let sortedHeights = sort heights
  let minDiff = findMinDifference sortedHeights k

  print minDiff

findMinDifference :: [Int] -> Int -> Int
findMinDifference heights k = minimum $ zipWith (-) (drop (k - 1) heights) heights

abc132c :: IO ()
abc132c = do
  n <- getInt
  difficulties <- getIntArray
  let sortedDifficulties = sort difficulties
  let halfN = n `div` 2
  let lowerHalf = take halfN sortedDifficulties
  let upperHalf = drop halfN sortedDifficulties
  let maxLower = maximum lowerHalf
  let minUpper = minimum upperHalf
  let result =
        if maxLower < minUpper
          then minUpper - maxLower
          else 0
  print result

abc067b :: IO ()
abc067b = do
  [n, k] <- getIntArray
  lengths <- getIntArray
  let res = maxSnakeLength k lengths
  print res

maxSnakeLength :: Int -> [Int] -> Int
maxSnakeLength k lengths = sum $ take k $ sortBy (flip compare) lengths

abc205b :: IO ()
abc205b = do
  n <- getInt
  a <- getIntArray
  if isPermutation n a
    then putStrLn "Yes"
    else putStrLn "No"

isPermutation :: Int -> [Int] -> Bool
isPermutation n a = sort a == [1 .. n]

abc113b :: IO ()
abc113b = do
  n <- getInt
  [t, a] <- getIntArray
  heights <- getIntArray
  let result = findBestLocation (fromIntegral t) (fromIntegral a) heights
  print result

findBestLocation :: Double -> Double -> [Int] -> Int
findBestLocation t a heights =
  let temperatures = map (calculateTemperarue t) heights
      differences = map (abs . subtract a) temperatures
      indexedDifferences = zip [1 ..] differences
   in fst $ minimumBy (compare `on` snd) indexedDifferences

calculateTemperarue :: Double -> Int -> Double
calculateTemperarue t height = t - fromIntegral height * 0.006

abc201b :: IO ()
abc201b = do
  n <- getInt
  mountains <- replicateM n $ do
    line <- getLine
    let parts = words line
    let name = head parts
    let height = read (parts !! 1) :: Int
    return (name, height)

  let sortedMountains = sortBy (flip compare `on` snd) mountains

  let secondHighest = fst (sortedMountains !! 1)
  putStrLn secondHighest

countSeaViewHotes :: [Int] -> Int
countSeaViewHotes [] = 0
countSeaViewHotes (h : hs) = 1 + go h hs
 where
  go _ [] = 0
  go maxSoFar (x : xs)
    | x >= maxSoFar = 1 + go x xs
    | otherwise = go maxSoFar xs

abc124b :: IO ()
abc124b = do
  n <- getInt
  heights <- getIntArray

  let res = countSeaViewHotes heights
  print res

abc095b :: IO ()
abc095b = do
  [n, x] <- getIntArray
  materials <- replicateM n getIntArray
  let flatMaterials = concat materials

  let totalForOne = sum flatMaterials
  let remaining = x - totalForOne
  let minMaterial = minimum flatMaterials
  let additionalDonuts = remaining `div` minMaterial
  let totalDounuts = n + additionalDonuts

  print totalDounuts

abc130b :: IO ()
abc130b = do
  [n, x] <- getIntArray
  ls <- getIntArray

  let positions = scanl (+) 0 ls
  let countInRange = length $ filter (<= x) positions
  print countInRange

abc174b :: IO ()
abc174b = do
  [n, d] <- getIntArray
  points <- replicateM n getIntArray

  let inRange = length $ filter (\[x, y] -> x * x + y * y <= d * d) points
  print inRange

abc088b :: IO ()
abc088b = do
  n <- getInt
  arr <- getIntArray

  let sortedCards = reverse $ sort arr
  let indexed = zip [0 ..] sortedCards

  let aliceScore = sum [card | (i, card) <- indexed, even i]
  let bodScore = sum [card | (i, card) <- indexed, odd i]

  print (aliceScore - bodScore)

abc081b :: IO ()
abc081b = do
  n <- getInt
  arr <- getIntArray

  let minDivisions = minimum $ map countDivisions arr
  print minDivisions

countDivisions :: Int -> Int
countDivisions n
  | even n = 1 + countDivisions (n `div` 2)
  | otherwise = 0

abc220b :: IO ()
abc220b = do
  k <- getInt
  [aStr, bStr] <- getStrArray

  let a = fromBaseK k aStr
  let b = fromBaseK k bStr

  print (a * b)

fromBaseK :: Int -> String -> Int
fromBaseK k str = foldl (\acc digit -> acc * k + digitToInt digit) 0 str
 where
  digitToInt c = read [c]

abc090b :: IO ()
abc090b = do
  [a, b] <- getIntArray
  let palindromes = [i | i <- [a .. b], isPalindrome i]
  print (length palindromes)

isPalindrome :: Int -> Bool
isPalindrome n =
  let s = show n
   in s == reverse s

abc068b :: IO ()
abc068b = do
  n <- getInt
  let result = findMaxPowerOfTwo n 1
  print result

findMaxPowerOfTwo :: Int -> Int -> Int
findMaxPowerOfTwo n current
  | current * 2 > n = current
  | otherwise = findMaxPowerOfTwo n (current * 2)

abc158c :: IO ()
abc158c = do
  [a, b] <- getIntArray
  let result = findTaxFreePrice a b
  print result

findTaxFreePrice :: Int -> Int -> Int
findTaxFreePrice a b =
  let candidates =
        [ x | x <- [1 .. 2500], (x * 8) `div` 100 == a
                                  && (x * 10) `div` 100 == b
        ]
   in if null candidates then -1 else head candidates

abc093b :: IO ()
abc093b = do
  [a, b, k] <- getIntArray

  let smallSide = [a .. min b (a + k - 1)]
  let largeSide = [max a (b - k + 1) .. b]

  let result = sort $ nub $ smallSide ++ largeSide

  mapM_ print result

abc208b :: IO ()
abc208b = do
  p <- getInt
  let factorials = map factorial [10, 9 .. 1]
  let result = greedyCoins p factorials 0
  print result

factorial :: Int -> Int
factorial n = product [1 .. n]

greedyCoins :: Int -> [Int] -> Int -> Int
greedyCoins 0 _ count = count
greedyCoins _ [] count = count
greedyCoins remaining (coin : coins) count
  | remaining >= coin =
      let useCoins = min 100 (remaining `div` coin)
       in greedyCoins (remaining - useCoins * coin) coins (count + useCoins)
  | otherwise = greedyCoins remaining coins count

abc164b :: IO ()
abc164b = do
  [a, b, c, d] <- getIntArray

  let turnsToWinA = (c + b - 1) `div` b
  let turnsToWinB = (a + d - 1) `div` d

  if turnsToWinA <= turnsToWinB
    then putStrLn "Yes"
    else putStrLn "No"

abc200b :: IO ()
abc200b = do
  [n, k] <- getIntArray
  let result = performOperations n k
  print result

performOperations :: Int -> Int -> Int
performOperations n 0 = n
performOperations n k
  | n `mod` 200 == 0 = performOperations (n `div` 200) (k - 1)
  | otherwise = performOperations (appendTwoHundred n) (k - 1)

appendTwoHundred :: Int -> Int
appendTwoHundred n = read (show n ++ "200")

abc165b :: IO ()
abc165b = do
  x <- getInt
  let cnt = findYear 100 x 0
  print cnt

findYear :: Int -> Int -> Int -> Int
findYear current target years
  | current >= target = years
  | otherwise = findYear newAmount target (years + 1)
 where
  newAmount = current + (current `div` 100)

abc206b :: IO ()
abc206b = do
  n <- getInt

  -- for (int i = 0; i < n; i++) {
  --     res += i;
  --     cnt = i
  --     if (res >= n) {
  --        cout << cnt << endl;
  --        return 0;
  --     }
  -- }
  let result = head [i | i <- [1 ..], let res = sum [0 .. i], res >= n]
  print result

abc162b :: IO ()
abc162b = do
  n <- getInt

  let result = sum [i | i <- [1 .. n], i `mod` 3 /= 0 && i `mod` 5 /= 0]
  print result

abc083b :: IO ()
abc083b = do
  [n, a, b] <- getIntArray

  let result = sum [i | i <- [1 .. n], let digitSum = sumOfDigits i, digitSum >= a && digitSum <= b]
  print result

sumOfDigits :: Int -> Int
sumOfDigits 0 = 0
sumOfDigits n = n `mod` 10 + sumOfDigits (n `div` 10)

abc165a :: IO ()
abc165a = do
  k <- getInt
  [a, b] <- getIntArray
  -- for (i=a; i<=b; i++) {
  --    if (i % k == 0) {
  --        cout  << "OK" << endl;
  --        return 0;
  --   }
  -- }
  -- cout << "NG" << endl;

  if any (\i -> i `mod` k == 0) [a .. b]
    then putStrLn "OK"
    else putStrLn "NG"

abc176a :: IO ()
abc176a = do
  [n, x, t] <- getIntArray
  if n `mod` x == 0
    then
      let tt = n `div` x
       in print (tt * t)
    else
      let tt = n `div` x + 1
       in print (tt * t)

abc157a :: IO ()
abc157a = do
  n <- getInt
  if even n
    then print (n `div` 2)
    else print (n `div` 2 + 1)

abc173a :: IO ()
abc173a = do
  n <- getInt
  if n `mod` 1000 == 0
    then print 0
    else print (1000 - (n `mod` 1000))

abc125a :: IO ()
abc125a = do
  [a, b, t] <- getIntArray
  let totalBiscuts = (t `div` a) * b
  print totalBiscuts

abc128a :: IO ()
abc128a = do
  [a, p] <- getIntArray
  let peaceApple = a * 3 + p
  let applePie = peaceApple `div` 2
  print applePie

abc064a :: IO ()
abc064a = do
  [r, g, b] <- getIntArray
  let sumRGB = 100 * r + 10 * g + b
  if sumRGB `mod` 4 == 0
    then putStrLn "YES"
    else putStrLn "NO"

abc087a :: IO ()
abc087a = do
  x <- getInt
  a <- getInt
  b <- getInt
  let takeCake = x - a
  print (takeCake `mod` b)

abc088a :: IO ()
abc088a = do
  n <- getInt
  a <- getInt
  let mod500 = n `mod` 500
  if mod500 <= a
    then putStrLn "Yes"
    else putStrLn "No"

abc118a :: IO ()
abc118a = do
  [a, b] <- getIntArray
  if b `mod` a == 0
    then print (a + b)
    else print (b - a)

abc127a :: IO ()
abc127a = do
  [a, b] <- getIntArray
  if a >= 13
    then print b
    else
      if a >= 6
        then print (b `div` 2)
        else print 0

abc137a :: IO ()
abc137a = do
  [a, b] <- getIntArray
  let aPb = a + b
  let aMb = a - b
  let aMltb = a * b
  let abArray = [aPb, aMb, aMltb]
  print $ maximum abArray

abc153a :: IO ()
abc153a = do
  [h, a] <- getIntArray
  let attacks = h `div` a
  if h `mod` a == 0
    then print attacks
    else print (attacks + 1)

abc086a :: IO ()
abc086a = do
  [a, b] <- getIntArray
  let c = a * b
  if even c
    then putStrLn "Even"
    else putStrLn "Odd"

abc169a :: IO ()
abc169a = do
  [a, b] <- getIntArray
  let c = a * b
  print c

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
