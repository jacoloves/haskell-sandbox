import Control.Monad (replicateM)

main :: IO ()
main = do
    n <- readLn :: IO Int
    s <- replicateM n getLine
    putStrLn $ funcA n s

funcA :: Int -> [String] -> String
funcA n s
    | n < 2 = "Yes"
    | otherwise = funcB n s

funcB :: Int -> [String] -> String
funcB n s
    | any (\(x, y) -> x == "sweet" && y == "sweet") (zip (take (n-2) s) (drop 1 s)) = "No"
    | otherwise = "Yes"