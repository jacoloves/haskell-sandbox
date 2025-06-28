main :: IO ()
main = do
  n <- getLine
  putStrLn $ if checkConditions n then "Yes" else "No"

checkConditions :: String -> Bool
checkConditions n = count '1' n == 1 && count '2' n == 2 && count '3' n == 3

count :: Char -> String -> Int
count c = length . filter (== c)
