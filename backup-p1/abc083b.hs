main :: IO ()
main = do
  input <- getLine
  let [n, a, b] = map read (words input) :: [Int]
  print $ sumCount n a b

sumCount :: Int -> Int -> Int -> Int
sumCount n a b = sum [x | x <- [1 .. n], let s = digitSum x, s >= a, s <= b]

digitSum :: Int -> Int
digitSum 0 = 0
digitSum x = x `mod` 10 + digitSum (x `div` 10)