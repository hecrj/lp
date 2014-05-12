-- Useful functions
-- intercalates the first argument with the elements of the second argument
intercalate :: Show s => String -> [s] -> String
intercalate delim [] = ""
intercalate delim [x] = show x
intercalate delim (x:xs) = show x ++ delim ++ intercalate delim xs

-- variant of map that passes each element's index as first argument to f
each :: (Int -> a -> b) -> [a] -> [b]
each f = zipWith f [0..]

-- applies the first argument to the selected element and the second argument
-- to the others
mapSelect :: (a -> b) -> (a -> b) -> Int -> [a] -> [b]
mapSelect f1 f2 selected = each (ifSelected f1 f2 selected)
  where
    ifSelected f1 f2 selected current
      | selected == current = f1
      | otherwise = f2

-- obtains the minimum of the second argument given the min function
minimumBy :: Ord b => (a -> b) -> [a] -> a
minimumBy f (x:xs) = minimumBy' f xs x $ f x
  where
    minimumBy' f [] min _ = min
    minimumBy' f (x:xs) min val
      | f x < val = minimumBy' f xs x $ f x
      | otherwise = minimumBy' f xs min val


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

  components :: p -> [Double]
  components p = [(sel i p) | i <- [1..dim p]]

  dist :: p -> p -> Double
  dist e1 e2 = sqrt $ sum diffComponents
    where
      diffComponents = zipWith (\a b -> (a - b)^2) (components e2) (components e1)

  pointToString :: p -> String
  pointToString p = "(" ++ intercalate "," (components p) ++ ")"

  pointEquals :: p -> p -> Bool
  pointEquals e1 e2 = components e1 == components e2

  ptrans :: [Double] -> p -> p
  ptrans t = listToPoint . zipWith (+) t . components

  pscale :: Double -> p -> p
  pscale s = listToPoint . map (*s) . components


-- Point3d definition
data Point3d = Point3d { x :: Double, y :: Double, z :: Double }

instance Point Point3d where
  sel 1 = x
  sel 2 = y
  sel 3 = z

  dim _ = 3

  listToPoint [x, y, z] = Point3d x y z

instance Show Point3d where
  show = pointToString

instance Eq Point3d where
  (==) = pointEquals


-- Kd2nTree definition
data Kd2nTree p = Empty | Node { point :: p, list :: [Int], children :: [Kd2nTree p] }

instance (Point p, Eq p) => Eq (Kd2nTree p) where
  t1 == t2 = (all (contains t2) t1Points) && length t1Points == length t2Points
    where
      t1Points = get_points t1
      t2Points = get_points t2


instance (Show p) => Show (Kd2nTree p) where
  show node = show' node 0 $ -1
    where
      show' Empty _ _ = ""
      show' node level current = indent level ++ prefix current ++ nodeStr ++ childrenStr
        where
          nodeStr = show (point node) ++ " " ++ show (list node) ++ "\n"
          childrenStr = foldr (++) "" (each (\x y -> show' y (level+1) x) (children node))
          indent 0 = ""
          indent 1 = " "
          indent level = replicate (((level-1)*5)+1) ' '
          prefix (-1) = ""
          prefix index = '<' : show index ++ "> "


-- Kd2nTree functions
insert :: Point p => Kd2nTree p -> p -> [Int] -> Kd2nTree p
insert Empty point list = Node point list (take (2^(length list)) (repeat Empty))
insert (Node np nl nc) point list = Node np nl newChildren
  where
    newChildren = mapSelect (\x -> insert x point list) id selected nc
      where
        selected = child point np nl

build :: Point p => [(p, [Int])] -> Kd2nTree p
build = foldl (\t (p, l) -> insert t p l) Empty

buildIni :: Point p => [([Double], [Int])] -> Kd2nTree p
buildIni = foldl (\t (d, l) -> insert t (listToPoint d) l) Empty

get_all :: Kd2nTree p -> [(p, [Int])]
get_all Empty = []
get_all (Node point list children) = (point, list) : foldr (++) [] (map get_all children)

get_points :: Kd2nTree p -> [p]
get_points = map fst . get_all

remove :: (Point p, Eq p) => Kd2nTree p -> p -> Kd2nTree p
remove Empty _ = Empty
remove node@(Node np nl nc) point
  | point == np = build $ tail $ get_all node
  | otherwise = Node np nl newChildren
  where
    newChildren = mapSelect (\x -> remove x point) id selected nc
      where
        selected = child point np nl

contains :: (Point p, Eq p) => Kd2nTree p -> p -> Bool
contains Empty _ = False
contains (Node np nl nc) point
  | point == np = True
  | otherwise = contains (nc!!(child point np nl)) point

nearest :: Point p => Kd2nTree p -> p -> p
nearest tree point = minimumBy (dist point) $ get_points tree

kdmap :: (p -> q) -> Kd2nTree p -> Kd2nTree q
kdmap _ Empty = Empty
kdmap f (Node point list children) = Node (f point) list $ map (kdmap f) children

translation :: Point p => [Double] -> Kd2nTree p -> Kd2nTree p
translation = kdmap . ptrans

scale :: Point p => Double -> Kd2nTree p -> Kd2nTree p
scale = kdmap . pscale


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
