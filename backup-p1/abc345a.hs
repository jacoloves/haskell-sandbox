main :: IO ()
main = do
  s <- getLine
  putStrLn $ solver s

solver :: String -> String
solver s
  | head s == '<' && last s == '>' && all (== '=') (init (tail s)) = "Yes"
  | otherwise = "No"