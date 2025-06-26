main :: IO ()
main = do
  n <- getLine
  putStrLn $ if checkFunc n then "Yes" else "No"

checkFunc :: String -> Bool
checkFunc n = checkABC n "ABC" || checkABC n "BAC" || checkABC n "CAB" || checkABC n "ACB" || checkABC n "BCA" || checkABC n "CBA"

checkABC :: String -> String -> Bool
checkABC n abc = n == abc