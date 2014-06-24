myMap :: (a -> b) -> [a] -> [b]
myMap f xs = [ f x | x <- xs ]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = [ x | x <- xs, f x ]

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f as bs = [ f a b | (a,b) <- zip as bs ]

thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify xs ys = [ (x,y) | x <- xs, y <- ys, x `mod` y == 0 ]

factors :: Int -> [Int]
factors x = [ factor | factor <- [1..x], x `mod` factor == 0 ]