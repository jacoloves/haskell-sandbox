main :: IO ()
main = do
  input <- getLine
  let [n, y] = map read (words input) :: [Int]
  putStrLn $ findCombination n y

findCombination :: Int -> Int -> String
findCombination n y =
  case [(x, y, z) | x <- [0 .. n], y' <- [0 .. n - x], let z = n - x - y', 10000 * x + 5000 * y' + 1000 * z == y] of
    (x, y', z) : _ -> unwords (map show [x, y', z])
    [] -> "-1 -1 -1"