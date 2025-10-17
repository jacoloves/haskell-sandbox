import Control.Monad (forM_, replicateM)
import Data.Char (isDigit, isLower, isUpper)
import Data.Function (on)
import Data.List (group, isSuffixOf, minimumBy, nub, sort, sortBy)
import Data.Map qualified as Map
import Text.Printf (printf)

main :: IO ()
main = do
  abc395aEx

abc395aEx :: IO ()
abc395aEx = do
  n <- getInt
  a <- getIntArray
  let ans = strictlyInc a
  putStrLn ans

strictlyInc :: [Int] -> String
strictlyInc a
  | isStrictlyIncreasing a = "Yes"
  | otherwise = "No"

isStrictlyIncreasing :: [Int] -> Bool
isStrictlyIncreasing a = all (uncurry (<)) (zip a (tail a))

abc3xxa :: IO ()
abc3xxa = do
  n <- getInt
  s <- getNLines n
  let ans = canEatAll s
  putStrLn ans

canEatAll :: [String] -> String
canEatAll s
  | hasBadConsecutive s = "No"
  | otherwise = "Yes"

hasBadConsecutive :: [String] -> Bool
hasBadConsecutive s = any isBadPair (zip [0 ..] (pairs s))
 where
  n = length s
  isBadPair (i, (d1, d2)) = d1 == "sweet" && d2 == "sweet" && i < n - 2

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

abc395a :: IO ()
abc395a = do
  y <- getInt
  let ans = leapYear y
  print ans

leapYear :: Int -> Int
leapYear y
  | y `mod` 4 == 0 && y `mod` 100 /= 0 = 366
  | y `mod` 100 == 0 && y `mod` 400 /= 0 = 365
  | y `mod` 400 == 0 = 366
  | otherwise = 365

abc396a :: IO ()
abc396a = do
  n <- getInt
  a <- getIntArray
  let ans = hasTripleConsecutive a
  putStrLn ans

hasTripleConsecutive :: [Int] -> String
hasTripleConsecutive xs
  | any (\g -> length g >= 3) (group xs) = "Yes"
  | otherwise = "No"

abc397a :: IO ()
abc397a = do
  x <- getFloat
  let ans = thermometer x
  print ans

thermometer :: Float -> Int
thermometer x
  | x >= 38.0 = 1
  | x >= 37.5 && x < 38.0 = 2
  | otherwise = 3

abc398a :: IO ()
abc398a = do
  n <- getInt
  let ans = buildPalindrome n
  putStrLn ans

buildPalindrome :: Int -> String
buildPalindrome n
  | odd n = replicate leftCount '-' ++ "=" ++ replicate leftCount '-'
  | otherwise = replicate leftCount '-' ++ "==" ++ replicate leftCount '-'
 where
  leftCount = (n - if odd n then 1 else 2) `div` 2

abc400a :: IO ()
abc400a = do
  a <- getInt
  let ans = abcParty a
  print ans

abcParty :: Int -> Int
abcParty a
  | 400 `mod` a == 0 = 400 `div` a
  | otherwise = -1

abc399a :: IO ()
abc399a = do
  n <- getInt
  s <- getStr
  t <- getStr
  let ans = n - hamingCompare s t
  print ans

hamingCompare :: String -> String -> Int
hamingCompare s t =
  length [1 | (c1, c2) <- zip s t, c1 == c2]

abc401a :: IO ()
abc401a = do
  s <- getInt
  let ans = judgeLimit401A s
  putStrLn ans

judgeLimit401A :: Int -> String
judgeLimit401A s
  | 200 <= s && s <= 299 = "Success"
  | otherwise = "Failure"

abc402a :: IO ()
abc402a = do
  s <- getStr
  let ans = judgeUpper s
  putStrLn ans

judgeUpper :: String -> String
judgeUpper = filter isUpper

abc403a :: IO ()
abc403a = do
  n <- getInt
  a <- getIntArray
  let ans = sumOddPositions a
  print ans

sumOddPositions :: [Int] -> Int
sumOddPositions xs = sum [x | (i, x) <- zip [1 ..] xs, odd i]

abc404a :: IO ()
abc404a = do
  s <- getStr
  let ans = findNotFound s
  putStrLn [ans]

findNotFound :: String -> Char
findNotFound s = head [c | c <- ['a' .. 'z'], c `notElem` s]

abc405a :: IO ()
abc405a = do
  [r, x] <- getIntArray
  let ans = isItRated r x
  putStrLn ans

isItRated :: Int -> Int -> String
isItRated r x
  | x == 1 && (1600 <= r && r <= 2999) = "Yes"
  | x == 2 && (1200 <= r && r <= 2399) = "Yes"
  | otherwise = "No"

abc406a :: IO ()
abc406a = do
  [a, b, c, d] <- getIntArray
  let ans = notAcceptable a b c d
  putStrLn ans

notAcceptable :: Int -> Int -> Int -> Int -> String
notAcceptable a b c d
  | a > c = "Yes"
  | a >= c && b >= d = "Yes"
  | otherwise = "No"

abc407a :: IO ()
abc407a = do
  [a, b] <- getIntArray
  let ans = approximateInteger a b
  print ans

approximateInteger :: Int -> Int -> Int
approximateInteger a b = round (fromIntegral a / fromIntegral b)

abc408a :: IO ()
abc408a = do
  [n, s] <- getIntArray
  t <- getIntArray
  let ans = staysAwake s t
  putStrLn ans

staysAwake :: Int -> [Int] -> String
staysAwake s t
  | all (<= s) intervals = "Yes"
  | otherwise = "No"
 where
  intervals = getIntervals t

getIntervals :: [Int] -> [Int]
getIntervals t = zipWith (-) t (0 : t)

abc409a :: IO ()
abc409a = do
  n <- getInt
  t <- getStr
  a <- getStr
  let ans = hasConflict t a
  putStrLn ans

hasConflict :: String -> String -> String
hasConflict t a
  | any bothWant (zip t a) = "Yes"
  | otherwise = "No"

bothWant :: (Char, Char) -> Bool
bothWant (t, a) = t == 'o' && a == 'o'

abc410a :: IO ()
abc410a = do
  n <- getInt
  a <- getIntArray
  k <- getInt
  let ans = judgeAarray a k
  print ans

judgeAarray :: [Int] -> Int -> Int
judgeAarray a k = length (filter (== True) [ax >= k | ax <- a])

abc411a :: IO ()
abc411a = do
  p <- getStr
  l <- getInt
  let res = countWhereVer p l
  putStrLn res

countStringJudge :: String -> Int -> String
countStringJudge p l =
  let pLength = length p
   in if pLength >= l
        then "Yes"
        else "No"

countWhereVer :: String -> Int -> String
countWhereVer p l
  | pLen >= l = "Yes"
  | otherwise = "No"
 where
  pLen = length p

abc412a :: IO ()
abc412a = do
  n <- getInt
  pairs <- readDoubleIntScore n
  let res = filterPair pairs
  print res

filterPair :: [(Int, Int)] -> Int
filterPair pairs = length (filter comparePair pairs)

comparePair :: (Int, Int) -> Bool
comparePair (x, y) = x < y

abc413a :: IO ()
abc413a = do
  [n, m] <- getIntArray
  a <- getIntArray
  let sumA = sum a
  let res = judgeSumA sumA m
  putStrLn res

judgeSumA :: Int -> Int -> String
judgeSumA sumA m
  | sumA <= m = "Yes"
  | otherwise = "No"

abc414a :: IO ()
abc414a = do
  [n, l, r] <- getIntArray
  pairs <- readDoubleIntScore n
  let res = countFullViewers l r pairs
  print res

countFullViewers :: Int -> Int -> [(Int, Int)] -> Int
countFullViewers l r pairs = length (filter (canWatchFull l r) pairs)

canWatchFull :: Int -> Int -> (Int, Int) -> Bool
canWatchFull l r (x, y) = x <= l && r <= y

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
