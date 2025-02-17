solver :: String -> String -> Int
solver s1 s2
  | s1 == "sick" && s2 == "sick" = 1
  | s1 == "sick" && s2 == "fine" = 2
  | s1 == "fine" && s2 == "sick" = 3
  | otherwise = 4

main :: IO ()
main = do
  [s1, s2] <- getStrArray
  print $ solver s1 s2

-- read String Array
getStrArray :: IO [String]
getStrArray = words <$> getLine