import Data.List

class Point p where
    sel :: Int -> p -> Double
    dim :: p -> Int
    child :: p -> p -> [Int] -> Int
    dist :: p -> p -> Double
    listToPoint :: [Double] -> p
    toString :: p -> String

    dist e1 e2 = sqrt (sum components) where
        components = [(sel i e2 - sel i e1)^2 | i <- [1..dim e1]]

    toString p = "(" ++ intercalate "," coordinates ++ ")" where
        coordinates = [show (sel i p) | i <- [1..dim p]]

data Point3d = Point3d { x :: Double, y :: Double, z :: Double }

instance Point Point3d where
    sel 1 p = x p
    sel 2 p = y p
    sel 3 p = z p

    dim p = 3

    child e1 e2 list = 2

    listToPoint [x, y, z] = Point3d x y z


instance Show Point3d where
    show p = toString p
