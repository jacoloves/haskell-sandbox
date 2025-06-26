main :: IO ()
main = do
    [l, r] <- map read . words <$> getLine :: IO [Int]
    putStrLn $ abc370a l r

abc370a :: Int ->Int -> String
abc370a l r = if l == 1 && r == 0 then "Yes"
                else if l == 0 && r == 1 then "No"
                else "Invalid"