-- Useful functions
intercalate :: Show s => String -> [s] -> String
intercalate delim [] = ""
intercalate delim [x] = show x
intercalate delim (x:xs) = show x ++ delim ++ intercalate delim xs

-- variant of map that passes each element's index as first argument to f
each :: (Int -> a -> b) -> [a] -> [b]
each f = zipWith f [0..]

mapSelect :: (a -> b) -> (a -> b) -> Int -> [a] -> [b]
mapSelect f1 f2 selected = each (ifSelected f1 f2 selected)
  where
    ifSelected f1 f2 selected current
      | selected == current = f1
      | otherwise = f2


-- Type class that represents a Point
class Point p where
  sel :: Int -> p -> Double
  dim :: p -> Int
  listToPoint :: [Double] -> p

  child :: p -> p -> [Int] -> Int
  child e1 e2 coords = (foldl (child' e1 e2) 0 coords) `div` 2
    where
      child' e1 e2 result x
        | sel x e1 <= sel x e2 = result*2
        | otherwise = (result+1)*2

  dist :: p -> p -> Double
  dist e1 e2 = sqrt $ sum diffComponents
    where
      diffComponents = [(sel i e2 - sel i e1)^2 | i <- [1..dim e1]]

  components :: p -> [Double]
  components p = [(sel i p) | i <- [1..dim p]]

  pointToString :: p -> String
  pointToString p = "(" ++ intercalate "," (components p) ++ ")"

  pointEquals :: p -> p -> Bool
  pointEquals e1 e2 = components e1 == components e2


-- Point3d definition
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


-- Kd2nTree definition
data Kd2nTree p = Empty | Node { point :: p, list :: [Int], children :: [Kd2nTree p] }

instance (Show p) => Show (Kd2nTree p) where
  show Empty = ""
  show node = show' node 0
    where
      show' node level = nodeStr ++ "\n" ++ childrenStr
        where
          nodeStr = show (point node) ++ " " ++ show (list node)
          childrenStr = foldr (++) "" (each (showChild (level+1)) (children node))
            where
              showChild _ _ Empty = ""
              showChild level current node = indent ++ index ++ show' node level
                where
                  indent = replicate (level*4) ' '
                  index  = "<" ++ show current ++ "> "


-- Kd2nTree functions
insert :: Point p => Kd2nTree p -> p -> [Int] -> Kd2nTree p
insert Empty point list = Node point list (take (2^(length list)) (repeat Empty))
insert (Node np nl nc) point list = Node np nl newChildren
  where
    newChildren = mapSelect (insert' point list) id selected nc
      where
        insert' point list node = insert node point list
        selected = child point np nl

build :: Point p => [(p, [Int])] -> Kd2nTree p
build = foldl (\t (p, l) -> insert t p l) Empty

buildIni :: Point p => [([Double], [Int])] -> Kd2nTree p
buildIni = foldl (\t (d, l) -> insert t (listToPoint d) l) Empty


-- Example set
exampleSet :: Kd2nTree Point3d
exampleSet = buildIni [
    ([3.0, -1.0, 2.1], [1, 3]),
    ([3.5, 2.8, 3.1], [1, 2]),
    ([3.5, 0.0, 2.1], [3]),
    ([3.0, -1.7, 3.1], [1, 2, 3]),
    ([3.0, 5.1, 0.0], [2]),
    ([1.5, 8.0, 1.5], [1]),
    ([3.3, 2.8, 2.5], [3]),
    ([4.0, 5.1, 3.8], [2]),
    ([3.1, 3.8, 4.8], [1, 3]),
    ([1.8, 1.1, -2.0], [1, 2])
  ]
