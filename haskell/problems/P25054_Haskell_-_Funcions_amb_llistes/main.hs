myLength :: [Int] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x:xs) = max x (myMaximum xs)

average :: [Int] -> Float
average x = (fromIntegral $ sum x) / (fromIntegral $ length x)

buildPalindrome :: [Int] -> [Int]
buildPalindrome x = palindrome x []
  where
    palindrome [] y = y
    palindrome (x:xs) y = palindrome xs (x : y ++ [x])

remove :: [Int] -> [Int] -> [Int]
remove x y = filter (\x -> not (x `elem` y)) x

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = x ++ (flatten xs)

oddsNevens :: [Int] -> ([Int], [Int])
oddsNevens x = ((filter odd x), (filter even x))

primeDivisors :: Int -> [Int]
primeDivisors 0 = []
primeDivisors 1 = []
primeDivisors n
  | factor == [] = [n]
  | otherwise = factor ++ divisors
  where
    factor = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]
    divisors = primeDivisors $ factdiv n $ head factor
      where
        factdiv n factor
          | n `mod` factor == 0 = factdiv (n `div` factor) factor
          | otherwise = n
