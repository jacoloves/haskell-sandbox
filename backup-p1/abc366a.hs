main :: IO ()
main = do
    [n, t, a] <- map read . words <$> getLine :: IO [Int]
    putStrLn $ funcA n t a

funcA :: Int -> Int -> Int -> String
funcA n t a 
    | t == a = "No"
    | otherwise = funcB n t a

funcB :: Int -> Int -> Int -> String
funcB n t a = ans
    where
        remnantV = n - (t + a)
        ans 
            | t > a && (a + remnantV) > t = "No"
            | t < a && (t + remnantV) > a = "No"
            | otherwise = "Yes"
    