import Data.Char (isLower, isUpper)

main :: IO ()
main = do
  s <- getLine
  putStrLn $ solver s

solver :: String -> String
solver s
  | isUpper (head s) && all isLower (tail s) = "Yes"
  | otherwise = "No"