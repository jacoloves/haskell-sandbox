simple = (\x -> x)

makeChange =
  ( \owend given ->
      if given - owend > 0
        then given - owend
        else 0
  )

inc = (\x -> x + 1)
double = (\x -> x * 2)
square = (\x -> x ^ 2)
