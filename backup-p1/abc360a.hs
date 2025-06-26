main :: IO ()
main = do
    s <- getLine
    putStrLn $ funcA s

funcA :: String -> String
funcA s 
    | s == "RSM" = "Yes"
    | s == "RMS" = "Yes"
    | s == "SRM" = "Yes"
    | otherwise = "No"