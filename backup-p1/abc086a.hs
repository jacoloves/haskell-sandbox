main :: IO ()
main = do
  input <- getLine
  let [a, b] = map read (words input) :: [Int]
  let c = a * b
  putStrLn $ if even c then "Even" else "Odd"