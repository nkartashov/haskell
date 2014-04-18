data Point = Point Float Float deriving (Show)
plusp :: Point -> Point -> Point
plusp (Point x y) (Point x' y') = Point (x + x') (y + y')
