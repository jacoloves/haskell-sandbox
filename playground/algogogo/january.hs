import Control.Monad (foldM, forM_, replicateM)
import Data.Array (Array, bounds, listArray, (!))
import Data.Char (isDigit, isLower, isUpper)
import Data.Function (on)
import Data.List (elemIndex, group, isSuffixOf, minimumBy, nub, permutations, sort, sortBy)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Set qualified as Set

main :: IO ()
main = do
  training055

training055 :: IO ()
training055 = do
  [h, w] <- getIntArray
  rows <- getNLines h
  mapM_ (\row -> putStrLn row >> putStrLn row) rows

training054 :: IO ()
training054 = do
  s <- getStr
  t <- getStr
  let rotations = take (length s) $ iterate rotate s
  putStrLn $ if t `elem` rotations then "Yes" else "No"

rotate :: String -> String
rotate [] = []
rotate s = last s : init s

training053 :: IO ()
training053 = do
  n <- getInt
  ts <- getIntArray
  m <- getInt
  drinks <- replicateM m $ do
    [p, x] <- getIntArray
    return (p, x)

  let baseTime = sum ts
  forM_ drinks $ \(p, x) -> do
    let originalTime = ts !! (p - 1)
    let newTime = baseTime - originalTime + x
    print newTime

training052 :: IO ()
training052 = do
  s <- getStr
  let isValid = checkConditions s
  putStrLn $ if isValid then "AC" else "WA"

checkConditions :: String -> Bool
checkConditions s
  | length s < 4 = False
  | head s /= 'A' = False
  | otherwise =
      let middle = drop 2 $ init s
          cCount = length $ filter (== 'C') middle
          allLowers = all isLower $ filter (/= 'A') $ filter (/= 'C') s
       in cCount == 1 && allLowers

training051 :: IO ()
training051 = do
  n <- getInt
  [t, a] <- getIntArray
  hs <- getIntArray
  let tmps = map (\h -> fromIntegral t - fromIntegral h * 0.006) hs
  let diffs = map (\tmp -> abs (tmp - fromIntegral a)) tmps
  let indexed = zip [1 ..] diffs
  let (idx, _) = minimumBy (comparing snd) indexed
  print idx

training050 :: IO ()
training050 = do
  [a, b] <- getIntArray
  let candidates = [x | x <- [1 .. 10000], x * 8 `div` 100 == a, x * 10 `div` 100 == b]
  case candidates of
    [] -> print (-1)
    (x : _) -> print x

training049 :: IO ()
training049 = do
  [a, b, k] <- getIntArray
  let smallest = [a .. min (a + k - 1) b]
  let largest = [max a (b - k + 1) .. b]
  let result = nub $ sort $ smallest ++ largest
  mapM_ print result

training048 :: IO ()
training048 = do
  [n, d] <- getIntArray
  points <- replicateM n getIntArray
  let pairs = [(i, j) | i <- [0 .. n - 2], j <- [i + 1 .. n - 1]]
  let validPairs = filter (isIntegerDistance points) pairs
  print $ length validPairs

isIntegerDistance :: [[Int]] -> (Int, Int) -> Bool
isIntegerDistance points (i, j) = isPerfectSquare distSquared
 where
  p1 = points !! i
  p2 = points !! j
  distSquared = sum $ zipWith (\a b -> (a - b) ^ 2) p1 p2

isPerfectSquare :: Int -> Bool
isPerfectSquare n = sqrtN * sqrtN == n
 where
  sqrtN = floor $ sqrt $ fromIntegral n

training047 :: IO ()
training047 = do
  n <- getInt
  words <- getNLines n
  let isUnique = length words == length (nub words)
  let isChained = all (\(w1, w2) -> last w1 == head w2) (zip words (tail words))
  putStrLn $ if isUnique && isChained then "Yes" else "No"

training046 :: IO ()
training046 = do
  [n, m] <- getIntArray
  edges <- replicateM m $ do
    [a, b] <- getIntArray
    return (a, b)
  let degrees = [countDegree city edges | city <- [1 .. n]]
  mapM_ print degrees

countDegree :: Int -> [(Int, Int)] -> Int
countDegree city edges = length $ filter (\(a, b) -> a == city || b == city) edges

training045 :: IO ()
training045 = do
  n <- getInt
  bs <- getIntArray
  let a1 = head bs
  let an = last bs
  let middle = zipWith min bs (tail bs)
  print $ a1 + sum middle + an

training044 :: IO ()
training044 = do
  [n, a, b] <- getIntArray
  let ans = filter (\x -> let s = digitSum x in a <= s && s <= b) [1 .. n]
  print $ sum ans

digitSum :: Int -> Int
digitSum n = sum $ map (\c -> read [c]) $ show n

trainging043adv :: IO ()
trainging043adv = do
  a <- readLn :: IO Integer
  b <- readLn :: IO Integer
  putStrLn $ case compare a b of
    GT -> "GREATER"
    LT -> "LESS"
    EQ -> "EQUAL"

training043 :: IO ()
training043 = do
  a <- getInt
  b <- getInt
  let ans
        | a > b = "GREATER"
        | a < b = "LESS"
        | otherwise = "EQUAL"
  putStrLn ans

training042 :: IO ()
training042 = do
  [h, w] <- getIntArray
  rows <- getNLines h
  let border = replicate (w + 2) '#'
  putStrLn border
  mapM_ (\row -> putStrLn $ '#' : row ++ "#") rows
  putStrLn border

training041 :: IO ()
training041 = do
  n <- getInt
  print $ lucas !! n

lucas :: [Integer]
lucas = 2 : 1 : zipWith (+) lucas (tail lucas)

training040 :: IO ()
training040 = do
  [a, b, c, k] <- getIntArray
  let d = if even k then a - b else b - a
  if abs d > 10 ^ 18
    then putStrLn "Unfair"
    else print d

training039 :: IO ()
training039 = do
  [a, b] <- getIntArray
  let palindromes = filter isPalindrome [a .. b]
  print $ length palindromes

isPalindrome :: Int -> Bool
isPalindrome n = s == reverse s
 where
  s = show n

training038 :: IO ()
training038 = do
  s <- getStr
  let n = length s
  let p1 = take n $ cycle "01"
  let p2 = take n $ cycle "10"
  let cnt1 = countDiff s p1
  let cnt2 = countDiff s p2
  print $ min cnt1 cnt2

countDiff :: String -> String -> Int
countDiff s1 s2 = length $ filter not $ zipWith (==) s1 s2

training037 :: IO ()
training037 = do
  w <- getStr
  let counts = map length $ group $ sort w
  let isBeautiful = all even counts
  putStrLn $ if isBeautiful then "Yes" else "No"

training036 :: IO ()
training036 = do
  [n, m] <- getIntArray
  preferences <- replicateM n $ do
    (_ : foods) <- getIntArray
    return (Set.fromList foods)
  let common = foldl1 Set.intersection preferences
  print $ Set.size common

training035 :: IO ()
training035 = do
  n <- getInt
  ps <- getIntArray
  let cnt = training035F ps
  print cnt

training035F :: [Int] -> Int
training035F [] = 0
training035F (p : ps) = fst $ foldl step (1, p) ps
 where
  step (count, minVal) x
    | x <= minVal = (count + 1, x)
    | otherwise = (count, minVal)

training034 :: IO ()
training034 = do
  [a, b] <- getIntArray
  let result
        | a <= 0 && b >= 0 = "Zero"
        | a > 0 = "Positive"
        | otherwise = if even (b - a + 1) then "Positive" else "Negative"
  putStrLn result

training033 :: IO ()
training033 = do
  n <- getInt
  as <- replicateM n getInt

  let sorted = sortBy (flip compare) as
  let max1 = head sorted
  let max2 = head (tail sorted)

  forM_ as $ \a -> do
    if a == max1
      then print max2
      else print max1

training032Adv :: IO ()
training032Adv = do
  times <- replicateM 5 getInt

  let allPerms = permutations times
  let allTimes = map calculateTime allPerms

  print $ minimum allTimes

calculateTime :: [Int] -> Int
calculateTime dishes = go 0 dishes
 where
  go currentTime [] = currentTime
  go currentTime (dish : rest) =
    let nextOrderTime = ((currentTime + 9) `div` 10) * 10
        deliveryTime = nextOrderTime + dish
     in go deliveryTime rest

training032 :: IO ()
training032 = do
  times <- replicateM 5 getInt

  let totalTime = sum times

  let roundups = map (\t -> (10 - (t `mod` 10)) `mod` 10) times

  let ans = totalTime + sum roundups - minimum roundups

  print ans

training031 :: IO ()
training031 = do
  [n, k, q] <- getIntArray

  answers <- replicateM q getInt

  let correctCounts = Map.fromListWith (+) [(a, 1) | a <- answers]

  let threshold = q - k

  forM_ [1 .. n] $ \i -> do
    let count = Map.findWithDefault 0 i correctCounts
    if count > threshold
      then putStrLn "Yes"
      else putStrLn "No"

training030 :: IO ()
training030 = do
  [n, m] <- getIntArray
  gates <- replicateM m $ do
    [l, r] <- getIntArray
    return (l, r)

  let leftMost = maximum $ map fst gates
  let rightMost = minimum $ map snd gates
  let ans = max 0 (rightMost - leftMost + 1)

  print ans

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
