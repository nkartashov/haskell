data Point = Point {x :: Double, y :: Double} deriving (Show)

data Rectangle = Rectangle {leftUp :: Point, rightDown :: Point}

surface :: Rectangle -> Double
surface (Rectangle p1 p2) = (abs ((x p1) - (x p2))) * (abs ((y p1) - (y p2)))
