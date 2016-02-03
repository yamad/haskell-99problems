-- problem 29, determine if number is prime

isPrime :: Int -> Bool
isPrime a = (==a) . last $ sieveTo a

-- | Sieve of Eratothsenes lists all prime numbers up to n
sieveTo :: Int -> [Int]
sieveTo n = sieveIter [2..n]
  where sieveIter []     = []
        sieveIter (x:xs) = x : sieveIter (filter ((0/=) . (`mod` x)) xs)

main = do
  print $ sieveTo 10
  print $ isPrime 7
