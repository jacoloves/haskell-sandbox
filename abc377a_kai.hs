import Data.List (sort)

main :: IO ()
main = do
  n <- getLine
  putStrLn $ if checkFunc n then "Yes" else "No"

checkFunc :: String -> Bool
checkFunc n = sort n == "ABC"