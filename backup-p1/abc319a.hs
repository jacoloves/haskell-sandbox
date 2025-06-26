-- Control.Monad
-- replicateM
import Control.Monad (replicateM)

solver :: String -> Int
solver s
  | s == "tourist" = 3858
  | s == "ksun48" = 3679
  | s == "Benq" = 3658
  | s == "Um_nik" = 3648
  | s == "apiad" = 3638
  | s == "Stonefeang" = 3630
  | s == "ecnerwala" = 3613
  | s == "mnbvmar" = 3555
  | s == "newbiedmy" = 3516
  | s == "semiexp" = 3481

main :: IO ()
main = do
  s <- getStr
  print $ solver s

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