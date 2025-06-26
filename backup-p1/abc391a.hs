main :: IO ()
main = do
  s <- getLine
  putStrLn $ solver s

solver :: String -> String
solver s
  | s == "N" = "S"
  | s == "S" = "N"
  | s == "E" = "W"
  | s == "W" = "E"
  | s == "NE" = "SW"
  | s == "SW" = "NE"
  | s == "NW" = "SE"
  | s == "SE" = "NW"