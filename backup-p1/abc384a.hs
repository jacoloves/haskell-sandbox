main :: IO()

main = do
  [n, c1, c2] <- words <$> getLine
  s <- getLine
  putStrLn $ solve (head c1) (head c2) s

solve :: Char -> Char -> String -> String
solve c1 c2 = map (\ch -> if ch == c1 then c1 else c2)
