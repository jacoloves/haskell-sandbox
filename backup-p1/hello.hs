import Control.Monad (replicateM)

main :: IO ()
main = do
    [a, line2, s] <- replicateM 3 getLine
    let [b, c] = words line2
    let a' = read a :: Int
        b' = read b :: Int
        c' = read c :: Int
    let d = a' + b' + c'
    putStrLn $ show d ++ " " ++ s