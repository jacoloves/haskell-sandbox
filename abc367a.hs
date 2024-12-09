main :: IO ()
main = do
    [a, b, c] <- map read . words <$> getLine :: IO [Int]
    putStrLn $ abc367a a b c

abc367a :: Int -> Int -> Int -> String
abc367a a b c
    | b < c = funcA a b c
    | otherwise = funcB a b c

funcA :: Int -> Int -> Int -> String
funcA a b c
    | b < a && a < c = "No"
    | otherwise = "Yes"

funcB :: Int -> Int -> Int -> String
funcB a b c
    | c < a && a < b = "Yes"
    | otherwise = "No"