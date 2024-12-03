main :: IO ()
main = do
  s <- getLine
  let chars = splitChars s
  putStrLn $ jiroFunc chars

splitChars :: String -> [String]
splitChars = map (: []) . filter (/= ' ')

jiroFunc :: [String] -> String
jiroFunc ["<", "<", "<"] = "B"
jiroFunc ["<", "<", ">"] = "C"
jiroFunc ["<", ">", ">"] = "A"
jiroFunc [">", "<", "<"] = "A"
jiroFunc [">", ">", "<"] = "C"
jiroFunc [">", ">", ">"] = "B"
jiroFunc _ = "Invalid input"