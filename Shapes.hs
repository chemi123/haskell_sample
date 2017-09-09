module Shapes
( Point (..),
  Shape (..), -- It means Shape (Circle, Rectangle)
  area,
  nudge,
  baseCircle,
  baseRect
) where
-- If export like below, you can't use the type constructor directly(It is like private?)
-- In this case, you can only make values of the types through baseCircle or baseRect
--
-- module Shapes
-- (Point, Shape) where

data Point = Pt Float Float deriving (Show)
-- Shape is called type constructor
-- Circle and Rectangle are called data constructor
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Pt x1 y1) (Pt x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Pt x y) r) a b = Circle (Pt (x+a) (y+b)) r
nudge (Rectangle (Pt x1 y1) (Pt x2 y2)) a b =
    Rectangle (Pt (x1+a) (y1+b)) (Pt (x2+a) (y2+b))

-- Apply partially
baseCircle :: Float -> Shape
baseCircle = Circle (Pt 0 0)

-- Apply partially
baseRect :: Point -> Shape
baseRect = Rectangle (Pt 0 0)
