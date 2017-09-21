import qualified Data.Map as Map

-- So verbose way to define type constructor which brings you a headache
data Person' = Person' String String Int Float String String deriving (Show)

-- firstName :: Person -> String
-- firstName (Person firstname _ _ _ _ _) = firstname
-- 
-- lastName :: Person -> String
-- lastName (Person _ lastname _ _ _ _) = lastname
-- 
-- age :: Person -> Int
-- age (Person _ _ age _ _ _) = age
-- 
-- height :: Person -> Float
-- height (Person _ _ _ height _ _) = height
-- 
-- phoneNumber :: Person -> String
-- phoneNumber (Person _ _ _ _ phonenumber _) = phonenumber
-- 
-- flavor :: Person -> String
-- flavor (Person _ _ _ _ _ flavor) = flavor

-- record syntax

data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

instance YesNo (BinaryTree a) where
    yesno EmptyTree = False
    yesno _ = True

instance Functor BinaryTree where
    fmap _ EmptyTree = EmptyTree
    fmap f (Node x left right)
        = Node (f x) (fmap f left) (fmap f right)

singleton :: a -> BinaryTree a
singleton x = Node x EmptyTree EmptyTree

insertTree :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insertTree x EmptyTree = singleton x
insertTree x (Node a left right)
    | x == a = Node a left right
    | x < a  = Node a (insertTree x left) right
    | x > a  = Node a left (insertTree x right)

elemTree :: (Ord a) => a -> BinaryTree a -> Bool
elemTree x (Node a left right)
    | x == a = True
    | x < a  = elemTree x left
    | x > a  = elemTree x right

data Person = Person { firstName :: String,
                       lastName :: String,
                       age :: Int
                     } deriving (Eq, Show, Read)

-- Car data type
data Car = Car { company :: String,
                 model :: String,
                 year :: Int
               } deriving (Show)

-- Define Car data type that has type parameters, which is not a good way though. It's just for learn.
data Car' a b c = Car' { company' :: a,
                         model' :: b,
                         year' :: c
                       } deriving (Show)

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y})
    = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

tellCar' :: (Show a) => Car' String String a -> String
tellCar' (Car' {company' = c, model' = m, year' = y})
    = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Point = Pt Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle (Pt _ _) r) = pi * r ^ 2
area (Rectangle (Pt x1 y1) (Pt x2 y2)) =
    (abs $ x1 - x2) * (abs $ y1 - y2)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Pt x y) r) a b = Circle (Pt (x+a) (y+b)) r
nudge (Rectangle (Pt x1 y1) (Pt x2 y2)) a b =
    Rectangle (Pt (x1+a) (y1+b)) (Pt (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle = Circle (Pt 0 0)

baseRect :: Point -> Shape
baseRect = Rectangle (Pt 0 0)

data Day = Monday | Tuesday | Wednesday | Thursdasy | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Type synonyms, which gives another name to any type
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: PhoneBook
phoneBook = [("hoge", "0000"), ("piyo", "1111"), ("fuga", "2222")]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

type AssocList k v = [(k, v)]

assocList :: AssocList k v -> AssocList k v
assocList kv = kv

-- just for practice
-- data Either a b = Left a | Right b deriving (Eq, Ord, Show, Read)

maybeA :: Maybe a -> Maybe a
maybeA a = a

-- Either sample start --
data LockerState = Taken | Free deriving(Show, Eq)

type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockers :: LockerMap
lockers = Map.fromList [(100, (Taken, "ZD391")),
                        (101, (Free, "JAH3I")),
                        (103, (Free, "IQSA9")),
                        (105, (Free, "QOTSA")),
                        (109, (Taken, "893JJ")),
                        (110, (Taken, "99292"))
                       ]

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup number map = case Map.lookup number map of
    Nothing -> Left $ "The locker " ++ show number ++ " does not exist!"
    Just (state, code) -> if state /= Taken
                          then Right code
                          else Left $ "The locker " ++ show number ++ " is already taken!"

-- Either sample end --

-- recurisive data structure samples --
infixr 5 :-
data List a = Empty | a :- (List a) deriving (Show)

data List' a = Empty' | Cons a (List' a) deriving (Show)

-- recurisive data structure ends --

-- type class samples --
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno (Just _) = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

instance YesNo (List a) where
    yesno Empty = False
    yesno _ = True

instance YesNo (List' a) where
    yesno Empty' = False
    yesno _ = True

yesnoIf :: (YesNo y) => y -> String
yesnoIf yesnoVal =
    if yesno yesnoVal
    then "Yes"
    else "No"
-- type class samples ends --
