main :: IO ()
main = do
  s <- getLine
  let digits = map (read . (: [])) s :: [Int]
  let countOnes = length . filter (== 1) $ digits
  print countOnes