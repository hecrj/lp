absValue :: Int -> Int
absValue x = if x > 0 then x else -x

power :: Int -> Int -> Int
power x p = x^p

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime x = not $ any divisible $ takeWhile notTooBig [2..] where
     divisible y = x `mod`y == 0
     notTooBig y = y*y <= x

slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib(n-1) + slowFib(n-2)

quickFib :: Int -> Int
quickFib n = fibs!!n
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
