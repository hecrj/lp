myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x:xs) = max x (myMaximum xs)

average :: [Int] -> Float
average x = (fromIntegral (foldl (+) 0 x)) / (fromIntegral (myLength x))

buildPalindrome :: [Int] -> [Int]
buildPalindrome x = palindrome x []

palindrome [] y = y
palindrome (x:xs) y = palindrome xs ([x] ++ y ++ [x])

remove :: [Int] -> [Int] -> [Int]
remove x y = filter (\x -> not (x `elem` y)) x

flatten :: [[Int]] -> [Int]
flatten [x] = x
flatten (x:xs) = x ++ (flatten xs)

oddsNevens :: [Int] -> ([Int], [Int])
oddsNevens x = ((filter odd x), (filter even x))

primeDivisors :: Int -> [Int]
primeDivisors 0 = []
primeDivisors 1 = []
primeDivisors n = if factors == [] then [n]
	else factors ++ primeDivisors (n `div` (head factors))
	where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]
