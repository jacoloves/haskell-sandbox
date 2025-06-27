import Control.Monad (replicateM)
main :: IO ()
main = do
    abc169a

abc086a :: IO ()
abc086a = do
    [a, b] <- getIntArray
    

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