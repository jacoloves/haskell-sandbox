main :: IO()
main = do
  s <- getLine
  putStrLn $ solve (drop 3 s)



solve :: String -> String
solve b =  
    let intB = read b :: Int
    in if  (intB >= 1) && (intB < 350) && (intB /= 316)
      then "Yes"
      else "No"
