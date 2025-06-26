import Data.List (group, sort, maximumBy)

main :: IO ()
main = do
  s <- getLine
  print $ solver s

solver :: String -> Int
solver s = (+1) . length . takeWhile (== commonChar) $ s
  where
    commonChar = head . maximumBy (\a b -> compare (length a) (length b)) . group . sort $ s