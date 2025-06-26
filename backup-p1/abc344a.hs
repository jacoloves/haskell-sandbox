main :: IO ()
main = do
  s <- getLine
  putStrLn $ solver s

solver :: String -> String
solver s = let (before, rest) = break (== '|') s
               afterFirstPipe = tail rest
               (_, afterSecondPipe) = break (== '|') afterFirstPipe
            in before ++ tail afterSecondPipe