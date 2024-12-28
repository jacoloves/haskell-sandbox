remove test [] = []
remove test (x : xs) =
  if test x
    then remove test xs
    else x : remove test xs

myProduct xs = foldl (*) 1 xs
