-- Useful functions
-- variant of map that passes each element's index as second argument to f
each :: (a -> Int -> b) -> [a] -> [b]
each f l = zipWith f l [0..]


-- Type class that represents a Point
class Point p where
  sel :: Int -> p -> Double
  dim :: p -> Int
  listToPoint :: [Double] -> p

  child :: p -> p -> [Int] -> Int
  child e1 e2 coords = (foldl (child' e1 e2) 0 coords) `div` 2 where
    child' e1 e2 result x = if sel x e1 <= sel x e2
      then result*2
      else (result+1)*2

  dist :: p -> p -> Double
  dist e1 e2 = sqrt $ sum diffComponents where
      diffComponents = [(sel i e2 - sel i e1)^2 | i <- [1..dim e1]]

  components :: p -> [Double]
  components p = [(sel i p) | i <- [1..dim p]]

  pointToString :: p -> String
  pointToString p = "(" ++ intercalate "," (components p) ++ ")" where
    intercalate delim [] = ""
    intercalate delim [x] = show x
    intercalate delim (x:xs) = show x ++ delim ++ intercalate delim xs

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
  show node = show' node 0 where
    show' node level = nodeStr ++ "\n" ++ childrenStr where
      nodeStr = show (point node) ++ " " ++ show (list node)
      childrenStr = foldr (++) "" (each (showChild (level+1)) (children node)) where
        showChild _ Empty _ = ""
        showChild level node current = indent ++ index ++ show' node level where
          indent = replicate (level*4) ' '
          index  = "<" ++ show current ++ "> "


-- Kd2nTree functions
insert :: Point p => Kd2nTree p -> p -> [Int] -> Kd2nTree p
insert Empty p l = Node p l (take (2^(length l)) (repeat Empty))
insert (Node point list children) p l = Node point list newChildren where
  newChildren = each (insert' p l (child p point list)) children where
    insert' p l selected child current = if selected == current
      then insert child p l
      else child

build :: Point p => [(p, [Int])] -> Kd2nTree p
build nodes = foldl (\t (p, l) -> insert t p l) Empty nodes

buildIni :: Point p => [([Double], [Int])] -> Kd2nTree p
buildIni nodes = foldl (\t (d, l) -> insert t (listToPoint d) l) Empty nodes


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
