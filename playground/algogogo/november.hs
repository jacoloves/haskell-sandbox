import Control.Monad (forM_, replicateM)
import Data.Array (Array, bounds, listArray, (!))
import Data.Char (isDigit, isLower, isUpper)
import Data.Function (on)
import Data.List (group, isSuffixOf, minimumBy, nub, sort, sortBy)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Text.Printf (printf)

main :: IO ()
main = do
  cf2016qb

cf2016qb :: IO ()
cf2016qb = do
  [n, a, b] <- getIntArray

  s <- getStr

  let ans = simulateQualification a b s 0 0
  mapM_ putStrLn ans

-- a: internal, b: external, s: list, total: passed count, overseas: overseas count
simulateQualification :: Int -> Int -> String -> Int -> Int -> [String]
simulateQualification _ _ [] _ _ = []
simulateQualification a b (c : cs) total overseas
  | c == 'c' = "No" : simulateQualification a b cs total overseas
  | total >= a + b = "No" : simulateQualification a b cs total overseas
  | c == 'a' = "Yes" : simulateQualification a b cs (total + 1) overseas
  | c == 'b' && overseas < b = "Yes" : simulateQualification a b cs (total + 1) (overseas + 1)
  | otherwise = "No" : simulateQualification a b cs total overseas

abc156c :: IO ()
abc156c = do
  n <- getInt
  x <- getIntArray

  let minPos = minimum x
      maxPos = maximum x

  let costs = [calcTotalCost p x | p <- [minPos .. maxPos]]

  print $ minimum costs

-- 座標pで集会を開いた時の体力総和を計算
calcTotalCost :: Int -> [Int] -> Int
calcTotalCost p x = sum [square (xi - p) | xi <- x]
  where
    square n = n * n

abc139b :: IO ()
abc139b = do
  [a, b] <- getIntArray

  -- 天井関数: (x + y - 1) `div` y は　　[x / y] と同じ
  let numerator = b - 1
      denominator = a - 1
      result = (numerator + denominator - 1) `div` denominator

  print result

abc431c :: IO ()
abc431c = do
  [n, m, k] <- getIntArray
  heads <- sort <$> getIntArray
  bodies <- sort <$> getIntArray

  let result = canMakeRobots k heads bodies

  putStrLn $ if result then "Yes" else "No"

-- K体のロボットを作れるか判定する関数
canMakeRobots :: Int -> [Int] -> [Int] -> Bool
canMakeRobots k heads bodies =
  -- 最も軽い頭K個を取り出す
  let lightestHeads = take k heads
   in -- 各頭に対して体を割り当てられるか試す
      matchHeadsWithBodies lightestHeads bodies == k

-- 頭パーツのリストに体パーツを割り当て、成功した個数を返す
matchHeadsWithBodies :: [Int] -> [Int] -> Int
matchHeadsWithBodies [] _ = 0
matchHeadsWithBodies _ [] = 0
matchHeadsWithBodies (h : hs) bodies =
  -- 現在の頭 h に対して使える体を探す
  case dropWhile (< h) bodies of
    [] -> 0
    (b : bs) -> 1 + matchHeadsWithBodies hs bs

abc431b :: IO ()
abc431b = do
  x <- getInt
  n <- getInt
  weights <- getIntArray
  q <- getInt
  queries <- replicateM q getInt
  let results = processQueries x weights queries
  mapM_ print results

processQueries :: Int -> [Int] -> [Int] -> [Int]
processQueries initialWeight weights queries =
  reverse $ thd $ foldl processOne (Set.empty, initialWeight, []) queries
  where
    -- 部品の重さをMapに(1-indexed)
    weightMap = Map.fromList $ zip [1 ..] weights

    thd (_, _, x) = x

    -- 各クエリを処理（取り付け状態, 現在の重さ, 結界リスト)
    processOne (attached, currentWeight, results) partId =
      let partWeight = weightMap Map.! partId
          (newAttached, newWeight) =
            if Set.member partId attached
              then (Set.delete partId attached, currentWeight - partWeight) -- 取り外す
              else (Set.insert partId attached, currentWeight + partWeight) -- 取り付ける
       in (newAttached, newWeight, newWeight : results)

abc431a :: IO ()
abc431a = do
  [h, b] <- getIntArray
  let ans = robotBalance h b
  print ans

robotBalance :: Int -> Int -> Int
robotBalance h b
  | h >= b = h - b
  | otherwise = 0

abc430c :: IO ()
abc430c = do
  [n, a, b] <- getIntArray
  s <- getStr
  let ans = countValidRanges n a b s
  print ans

countValidRanges :: Int -> Int -> Int -> String -> Int
countValidRanges n minA maxB s = sum [countForLeft l | l <- [1 .. n]]
  where
    cumA = listArray (0, n) $ 0 : scanl1 (+) [if c == 'a' then 1 else 0 | c <- s]
    cumB = listArray (0, n) $ 0 : scanl1 (+) [if c == 'b' then 1 else 0 | c <- s]

    countA l r = cumA ! r - cumA ! (l - 1)
    countB l r = cumB ! r - cumB ! (l - 1)

    findMinR l = bsearch l n (\r -> countA l r >= minA)
    findMaxR l = bsearch l n (\r -> countB l r >= maxB)

    bsearch l r check = go l r
      where
        go left right
          | left > right = n + 1
          | left == right = if check left then left else n + 1
          | check mid = go left mid
          | otherwise = go (mid + 1) right
          where
            mid = (left + right) `div` 2

    countForLeft l =
      let r1 = findMinR l
          r2 = findMaxR l
       in max 0 (r2 - r1)

abc430b :: IO ()
abc430b = do
  [n, m] <- getIntArray
  grid <- getNLines n
  let ans = countDistinctSubgrids n m grid
  print ans

countDistinctSubgrids :: Int -> Int -> [String] -> Int
countDistinctSubgrids n m grid = Set.size (Set.fromList allSubgrids)
  where
    positions = [(i, j) | i <- [0 .. n - m], j <- [0 .. n - m]]
    allSubgrids = [extractSubgrid i j m grid | (i, j) <- positions]

extractSubgrid :: Int -> Int -> Int -> [String] -> [[Char]]
extractSubgrid startRow startCol size grid =
  [ take size (drop startCol row)
    | row <- take size (drop startRow grid)
  ]

abc430a :: IO ()
abc430a = do
  [a, b, c, d] <- getIntArray
  let ans = candyCookie a b c d
  putStrLn ans

candyCookie :: Int -> Int -> Int -> Int -> String
candyCookie a b c d
  | a <= c && b <= d = "No"
  | a > c = "No"
  | otherwise = "Yes"

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
