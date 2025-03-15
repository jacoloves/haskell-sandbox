{-# LANGUAGE BangPatterns #-}

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.STRef (modifySTRef, newSTRef, readSTRef)
import Data.Vector.Unboxed.Mutable qualified as VM

-- 入力を取る部分（単純化のため readLn + words 版）
main :: IO ()
main = do
  n <- readLn
  arr <- map read . words <$> getLine
  print $ solver arr

solver :: [Int] -> Int
solver arr = runST $ do
  let n = length arr

  -- prefix[i] = A[0..i] に含まれる distinct の数
  prefix <- VM.new n
  freqP <- VM.replicate (n + 1) (0 :: Int) -- freqP[x] = x の出現回数 (0-based)
  distinctP <- newSTRef 0

  -- 左から走査
  forM_ [0 .. n - 1] $ \i -> do
    let x = arr !! i
    oldCount <- VM.read freqP x
    if oldCount == 0
      then do
        VM.write freqP x (oldCount + 1)
        modifySTRef distinctP (+ 1)
      else VM.write freqP x (oldCount + 1)

    d <- readSTRef distinctP
    VM.write prefix i d

  -- suffix[i] = A[i..(n-1)] に含まれる distinct の数
  suffix <- VM.new n
  freqS <- VM.replicate (n + 1) (0 :: Int)
  distinctS <- newSTRef 0

  -- 右から走査
  forM_ [n - 1, n - 2 .. 0] $ \i -> do
    let x = arr !! i
    oldCount <- VM.read freqS x
    if oldCount == 0
      then do
        VM.write freqS x (oldCount + 1)
        modifySTRef distinctS (+ 1)
      else VM.write freqS x (oldCount + 1)

    d <- readSTRef distinctS
    VM.write suffix i d

  -- 区切り位置 i (0 <= i < n-1) ごとに prefix[i] + suffix[i+1] の最大値を求める
  let go !mx !i
        | i >= n - 1 = return mx
        | otherwise = do
            p <- VM.read prefix i
            s <- VM.read suffix (i + 1)
            let !val = p + s
            go (max mx val) (i + 1)
  go 0 0

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

-- read Int array
getIntArray :: IO [Int]
getIntArray = map read . words <$> getLine :: IO [Int]

-- read Double
getDouble :: IO Double
getDouble = readLn :: IO Double