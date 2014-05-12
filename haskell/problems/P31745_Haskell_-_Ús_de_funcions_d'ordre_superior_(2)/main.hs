flatten :: [[Int]] -> [Int]
flatten = foldr (++) []

myLength :: String -> Int
myLength = length

myReverse :: [Int] -> [Int]
myReverse = foldl (\x y -> y : x) []

countIn :: [[Int]] -> Int -> [Int]
countIn l n = map (\x -> length $ filter (==n) x) l

firstWord :: String -> String
firstWord = head . words
