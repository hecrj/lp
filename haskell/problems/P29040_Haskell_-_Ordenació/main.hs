
insert :: [Int] -> Int -> [Int]
insert [] y = [y]
insert (x:xs) y =
    if x < y
        then x:(insert xs y)
        else y:x:xs

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert (isort xs) x

remove :: [Int] -> Int -> [Int]
remove (x:xs) y =
    if x == y
        then xs
        else x:(remove xs y)

ssort :: [Int] -> [Int]
ssort [] = []
ssort l =
    let m = foldr1 min l
    in m:(ssort (remove l m))

merge :: [Int] -> [Int] -> [Int]
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys) =
    if x < y
        then x:(merge xs (y:ys))
        else y:(merge (x:xs) ys)

split :: [Int] -> ([Int], [Int])
split [] = ([],[])
split [x] = ([x],[])
split (x:y:zs) = (x:xs, y:ys)
    where (xs, ys) = split zs

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort l = merge (msort xs) (msort ys)
    where (xs, ys) = split l

qsort :: [Int] -> [Int]
qsort [] = []
qsort (p:xs) = qsort [x | x <- xs, x < p] ++ [p] ++ qsort [x | x <- xs, x >= p]

genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort (p:xs) = genQsort [x | x <- xs, x < p] ++ [p] ++ genQsort [x | x <- xs, x >= p]
