eql :: [Int] -> [Int] -> Bool
eql = (==)

prod :: [Int] -> Int
prod = foldr (*) 1

prodOfEvens :: [Int] -> Int
prodOfEvens = prod . filter even

powersOf2 :: [Int]
powersOf2 = [2^i | i <- [0..]]

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct l = sum . zipWith (*) l
