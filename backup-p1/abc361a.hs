main :: IO ()
main = do
    [n, k, x] <- map read . words <$> getLine :: IO [Int]
    a <- map read . words <$> getLine :: IO [Int]
    putStrLn $ funcA a k x

funcA :: [Int] -> Int -> Int -> String
funcA a k x = unwords $ map show (funcB k x a)

funcB :: Int -> Int -> [Int] -> [Int]
funcB i x xs = take i xs ++ [x] ++ drop i xs