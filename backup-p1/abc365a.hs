main :: IO ()
main = do
    y <- readLn :: IO Int
    putStrLn $ funcA y

funcA :: Int -> String
funcA y
    | y `mod` 4 == 0 && y `mod` 100 /= 0 = "366"
    | y `mod` 100 == 0 && y `mod` 400 /= 0 = "365"
    | y `mod` 400 == 0 = "366"
    | otherwise = "365"
