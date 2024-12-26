import Control.Monad (replicateM)
import Data.List

main :: IO ()
main = do
    n <- readLn :: IO Int
    tv <- replicateM n $ do
        [t, v] <- map read . words <$> getLine :: IO [Int]
        return (t, v)
    print $ solve tv

solve :: [(Int, Int)] -> Int
solve tv = foldl' cnt 0 $ zip tv $ [(0,0)] ++ tv
    where
        cnt cum ((t1, v1),(t2, _)) = (+v1) . max 0 $ cum+t2-t1