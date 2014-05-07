class Point p where
    sel :: Int -> p -> Double
    dim :: p -> Int    
    listToPoint :: [Double] -> p

    child :: p -> p -> [Int] -> Int
    child e1 e2 coords = child' e1 e2 coords 0 where
        child' :: Point p => p -> p -> [Int] -> Int -> Int
        child' _ _ [] result = result `div` 2
        child' e1 e2 (x:xs) current = if sel x e1 <= sel x e2
            then child' e1 e2 xs (current*2)
            else child' e1 e2 xs ((current+1)*2)

    dist :: p -> p -> Double
    dist e1 e2 = sqrt (sum diffComponents) where
        diffComponents = [(sel i e2 - sel i e1)^2 | i <- [1..dim e1]]

    components :: p -> [Double]
    components p = [(sel i p) | i <- [1..dim p]]

    pointToString :: p -> String
    pointToString p = "(" ++ intercalate "," (components p) ++ ")" where
        intercalate :: String -> [Double] -> String
        intercalate delim [] = ""
        intercalate delim [x] = show x
        intercalate delim (x:xs) = show x ++ delim ++ intercalate delim xs

    pointEquals :: p -> p -> Bool
    pointEquals e1 e2 = components e1 == components e2

data Point3d = Point3d { x :: Double, y :: Double, z :: Double }

instance Point Point3d where
    sel 1 p = x p
    sel 2 p = y p
    sel 3 p = z p

    dim p = 3

    listToPoint [x, y, z] = Point3d x y z


instance Show Point3d where
    show p = pointToString p

instance Eq Point3d where
    e1 == e2 = pointEquals e1 e2


data Kd2nTree p = Empty | Node { point :: p, list :: [Int], children :: [Kd2nTree p] }

instance (Show p) => Show (Kd2nTree p) where
    show Empty = ""
    show node = showNode node 0 where
        showNode (Node point list children) level = show point ++ " " ++
            show list ++ "\n" ++ showChildren children (level+1) 0 where
                showChildren [] _ _ = ""
                showChildren (x:xs) level current = showChild x level current ++
                    showChildren xs level (current+1) where
                        showChild Empty _ _ = ""
                        showChild node level current = (replicate (level*4) ' ') ++
                            "<" ++ show current ++ "> " ++ showNode x level


exampleSet :: Kd2nTree Point3d
exampleSet = (Node (Point3d 3 (-1) 2.1) [1,3] [
        (Node (Point3d 3 5.1 0) [2] [
            (Node (Point3d 1.8 1.1 (-2)) [1,2] []),
            (Node (Point3d 1.5 8 1.5) [1] [])
        ]),
        (Node (Point3d 3 (-1.7) 3.1) [1,2,3] []),
        (Node (Point3d 3.5 0 2.1) [3] []),
        (Node (Point3d 3.5 2.8 3.1) [1,2] [
            (Node (Point3d 3.3 2.8 2.5) [3] []),
            (Node (Point3d 3.1 3.8 4.8) [1,3] []),
            Empty,
            (Node (Point3d 4.0 5.1 3.8) [2] [])
        ])
    ])
