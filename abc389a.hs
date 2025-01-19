import Data.Char (digitToInt)

main :: IO ()
main = do
  s <- getLine
  let a = digitToInt (head s)
  let b = digitToInt (last s)
  print (a * b)
