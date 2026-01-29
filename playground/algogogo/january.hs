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
  training045

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
