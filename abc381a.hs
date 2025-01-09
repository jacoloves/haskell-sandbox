main :: IO ()
main = do
  _ <- getLine
  s <- getLine
  putStrLn $ if isValid11_22 s then "Yes" else "No"

isValid11_22 :: String -> Bool
isValid11_22 s =
  let l = length s
  in odd l &&
    let mid = (l+1) `div` 2-1
    in s !! mid == '/' &&
      all (== '1') (take mid s) &&
      all (== '2') (drop (mid + 1) s)
