main :: IO ()
main = do
  [s, t] <- words <$> getLine
  putStrLn $ solve s t

solve :: String -> String -> String
solve s t
  | s == "AtCoder" && t == "Land" = "Yes"
  | otherwise = "No"
