main :: IO ()
main = do
    n <- getLine
    s <- getLine
    putStrLn $ show $ funcCount s

funcCount :: String -> Int
funcCount = length . filter (== '.')
