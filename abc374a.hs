main :: IO ()
main = do
  s <- getLine
  putStrLn $ searchSan s

searchSan :: String -> String
searchSan s
  | "san" `sanCheck` s = "Yes"
  | otherwise = "No"
  where
    sanCheck :: String -> String -> Bool
    sanCheck suffix str = suffix == drop (length str - length suffix) str