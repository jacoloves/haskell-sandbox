main :: IO()
main = do
    [a, b] <- map read . words <$> getLine :: IO [Int]
    print $ solve a b

solve :: Int -> Int -> Int
solve a b
    | (a == 1 && b == 2) || (a == 2 && b == 1) = 3
    | (a == 1 && b == 3) || (a == 3 && b == 1) = 2 
    | (a == 2 && b == 3) || (a == 3 && b == 2) = 1
    | otherwise = -1