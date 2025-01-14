main :: IO ()
main = do
    [n, l, r] <- map read . words <$> getLine :: IO [Int]
    let a = [1..n]
    putStrLn $ unwords $ map show $ solve l r a

solve :: Int -> Int -> [Int] -> [Int]
solve l r a =
    let (prefix, rest) = splitAt (l-1) a
        (middle, suffix) = splitAt (r - l + 1) rest
    in prefix ++ reverse middle ++ suffix