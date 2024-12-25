import Control.Monad (replicateM)
import Data.List (filter)

main :: IO ()
main = do
    n <- readLn :: IO Int
    s <- replicateM n getLine
    print $ funcA s

funcA :: [String] -> Int
funcA s = length (filter (\x -> x == "Takahashi") s)