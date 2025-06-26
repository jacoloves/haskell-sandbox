import Data.List (isPrefixOf)

main :: IO ()
main = do
  s <- getLine
  putStrLn $ if canForm s then "YES" else "NO"

canForm :: String -> Bool
canForm "" = True
canForm s
  | "dreamer" `isPrefixOf` s && canForm (drop 7 s) = True
  | "eraser" `isPrefixOf` s && canForm (drop 6 s) = True
  | "dream" `isPrefixOf` s && canForm (drop 5 s) = True
  | "erase" `isPrefixOf` s && canForm (drop 5 s) = True
  | otherwise = False
