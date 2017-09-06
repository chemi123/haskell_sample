-- Samples for pattern match

-- This is not pattern match though...
boomBang :: (Integral a) => [a] -> [String]
boomBang xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

lucky :: Integral a => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"   

-- Only takes parameter 'a', 'b', 'c', otherwise it causes an error
charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil"  

-- Define charName with case 
charName2 :: Char -> String
charName2 c = case c of 'a' -> "Albert"
                        'b' -> "Broseph"
                        'c' -> "Cecil"

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "singleton list."
                                               xs -> "a longer list."

-- Define with where
describeList2 :: [a] -> String
describeList2 xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "singleton list."
          what xs = "a longer list."

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
--addVectors a b = (fst a + fst b, snd a + snd b)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- リストのパターンマッチ
head' :: [a] -> a
head' [] = error "head' can't take empty list"
head' (x:_) = x

-- Note:
-- If defined as (x:xs), it means x is a head and xs is a tail
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- length' :: [a] -> Int  // OK
-- length' :: [a] -> Num  // NG
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:x) = 1 + length' x

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- If you define xs@(x:xs), xs means x:xs
capital :: String -> String
capital "" = "Empty String"
capital all@(x:xs) = "The first letter of " ++ all ++ "is " ++ [x] ++ " and the rest is " ++ xs

-- guards sample
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- guards + recursion
fibo' :: Integer -> Integer
fibo' n
    | n < 0 = error "Negative Number is not allowed"
    | n < 2 = 1
    | otherwise = fibo' (n - 1) + fibo' (n - 2)

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname
-- initials also can be described as below. One liner with let
-- initials firstname lastname = let (f:_) = firstname; (l:_) = lastname in [f] ++ ". " ++ [l] ++ "." 

-- Define with where
-- You can define with type in where
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi :: (RealFloat a) => a -> a -> a
          bmi weight height = weight / height ^ 2

-- It gets simpler if you combine let with list
-- It's a matter of fact which you find it simpler, where or let
calcBmis2 :: (RealFloat a) => [(a, a)] -> [a]
calcBmis2 xs = [let bmi w h = w / h ^ 2 in bmi w h | (w, h) <- xs]

-- You can also give some contditions
calcBmis3 :: (RealFloat a) => [(a, a)] -> [a]
calcBmis3 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

-- Use let
cylinder :: (RealFloat a) => a -> a -> a
cylinder h r =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in sideArea + topArea * 2
