module Shapes
( Point (..),
  Shape (..), -- It means Shape (Circle, Rectangle)
  -- If export like below, you can't use the type constructor directly(It is like private?)
  -- In this case, you can only make values of the types through baseCircle or baseRect
  -- (Point, Shape)
  area,
  nudge,
  baseCircle,
  baseRect
) where

data Point = Point Float Float deriving (Show)
-- Shape is called type constructor
-- Circle and Rectangle are called data constructor
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
    Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- Apply partially
baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

-- Apply partially
baseRect :: Point -> Shape
baseRect = Rectangle (Point 0 0)
