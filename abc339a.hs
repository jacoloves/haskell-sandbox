import Data.List.Split

main :: IO ()
main = do
  s <- getLine
  putStrLn $ solver s

solver :: String -> String
solver s = last $ splitOn "." s