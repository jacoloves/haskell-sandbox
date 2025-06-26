import Control.Monad (when)

main :: IO ()
main = do
  maxVal <- readLn :: IO Int
  let primesUpToMax = takeWhile (<= fromIntegral maxVal) primes
  print primesUpToMax

primes :: [Integer]
primes = sieve [2 ..]

sieve :: [Integer] -> [Integer]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]