main :: IO ()
main = do
    h <- readLn :: IO Int
    print $ solve h

solve :: Int -> Int
solve h = findI 0 0
    where
        findI i sum
            | sum > h = i
            | otherwise = findI (i + 1) (sum + 2^i)