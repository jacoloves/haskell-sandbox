main :: IO ()
main = do
    s <- getLine
    putStrLn $ deletedot s

deletedot :: String->String
deletedot s = filter (/= '.') s