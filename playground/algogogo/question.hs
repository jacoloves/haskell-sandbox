import Control.Monad (replicateM, forM_)

main :: IO ()
main = do
  abc162b

abc162b:: IO ()
abc162b = do
    n <- getInt
    
    let result = sum[i | i <- [1..n], i `mod` 3 /= 0 && i `mod` 5 /= 0]
    print result

abc083b:: IO ()
abc083b = do
    [n, a, b] <- getIntArray

    let result = sum [i | i <- [1..n], let digitSum = sumOfDigits i, digitSum >= a && digitSum <= b]
    print result

sumOfDigits :: Int -> Int
sumOfDigits 0 = 0
sumOfDigits n = n `mod` 10 + sumOfDigits (n `div` 10)

abc165a:: IO ()
abc165a = do
    k <- getInt
    [a, b] <- getIntArray
    -- for (i=a; i<=b; i++) {
    --    if (i % k == 0) {
    --        cout  << "OK" << endl;
    --        return 0;
    --   }
    -- }
    -- cout << "NG" << endl;
    
    if any (\i -> i `mod` k == 0) [a..b]
        then putStrLn "OK"
        else putStrLn "NG"
    


abc176a:: IO ()
abc176a = do
    [n, x, t] <- getIntArray
    if n `mod` x == 0
        then let tt = n `div` x
             in print (tt * t)
        else let tt = n `div` x + 1
             in print (tt * t)

abc157a:: IO ()
abc157a = do
    n <- getInt
    if even n
        then print (n `div` 2)
        else print (n `div` 2 + 1)

abc173a:: IO ()
abc173a = do
    n <- getInt
    if n `mod` 1000 == 0
        then print 0
        else print (1000 - (n `mod` 1000))

abc125a:: IO ()
abc125a = do
    [a, b, t] <- getIntArray
    let totalBiscuts = (t `div` a) * b
    print totalBiscuts


abc128a:: IO ()
abc128a = do
    [a, p] <- getIntArray
    let peaceApple = a * 3 + p
    let applePie = peaceApple `div` 2
    print applePie


abc064a :: IO ()
abc064a = do
  [r, g, b] <- getIntArray
  let sumRGB = 100 * r + 10 * g + b
  if sumRGB `mod` 4 == 0
    then putStrLn "YES"
    else putStrLn "NO"

abc087a :: IO ()
abc087a = do
  x <- getInt
  a <- getInt
  b <- getInt
  let takeCake = x - a
  print (takeCake `mod` b)

abc088a :: IO ()
abc088a = do
  n <- getInt
  a <- getInt
  let mod500 = n `mod` 500
  if mod500 <= a
    then putStrLn "Yes"
    else putStrLn "No"

abc118a :: IO ()
abc118a = do
  [a, b] <- getIntArray
  if b `mod` a == 0
    then print (a + b)
    else print (b - a)

abc127a :: IO ()
abc127a = do
  [a, b] <- getIntArray
  if a >= 13
    then print b
    else
      if a >= 6
        then print (b `div` 2)
        else print 0

abc137a :: IO ()
abc137a = do
  [a, b] <- getIntArray
  let aPb = a + b
  let aMb = a - b
  let aMltb = a * b
  let abArray = [aPb, aMb, aMltb]
  print $ maximum abArray

abc153a :: IO ()
abc153a = do
  [h, a] <- getIntArray
  let attacks = h `div` a
  if h `mod` a == 0
    then print attacks
    else print (attacks + 1)

abc086a :: IO ()
abc086a = do
  [a, b] <- getIntArray
  let c = a * b
  if even c
    then putStrLn "Even"
    else putStrLn "Odd"

abc169a :: IO ()
abc169a = do
  [a, b] <- getIntArray
  let c = a * b
  print c

-- read double Int score
readDoubleIntScore :: Int -> IO [(Int, Int)]
readDoubleIntScore n = replicateM n $ do
  [x, y] <- map read . words <$> getLine
  return (x, y)

-- read String Array
getStrArray :: IO [String]
getStrArray = words <$> getLine

-- read Int
getInt :: IO Int
getInt = readLn :: IO Int

-- read String
getStr :: IO String
getStr = getLine

-- read Int Array
getIntArray :: IO [Int]
getIntArray = map read . words <$> getLine :: IO [Int]

-- read Double
getDouble :: IO Double
getDouble = readLn :: IO Double