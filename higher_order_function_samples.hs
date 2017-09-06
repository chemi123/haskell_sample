-- This means max' takes a function which returns a and a function returns a
-- As a result, it looks like max' takes 2 parameters
-- This is called "currying"
max' :: (Ord a) => a -> (a -> a)
max' a b | a >= b = a
         | otherwise = b

-- same as max' function. it just takes 3 parameters
multThree :: (Num a) => a -> (a -> (a -> a))
multThree x y z = x * y * z 

-- returns a function which compares with 100
compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100

-- returns a function which devide by ten
devideByTen :: (Floating a) => a -> a
devideByTen = (/10)

-- returns a function which checks if a Char is from A to Z
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- not a curried funcion
add' :: (Int, Int) -> Int
add' (x, y) = x * y

-- Sample of a function which takes a function
-- it could apply a function partially
-- ex.) applyTwice (multThree 2 2) 9
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- sample for applyTwice
addOne :: Int -> Int
addOne x = x + 1

-- sample for applyTwice
subtractOne :: Int -> Int
subtractOne = (subtract 1)

-- starndard higher order function
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- type also can be described as below
-- flip' :: (a -> b -> c) -> (b -> a -> c)
flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- define map with foldr and lambda
map2' :: (a -> b) -> [a] -> [b]
map2' f xs = foldr (\x acc-> f x : acc) [] xs

-- filterと同じ
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

-- p stands for predicate
-- predicate in programing means a function which takes a parameter and returns bool
filter2' :: (a -> Bool) -> [a] -> [a]
filter2' p = foldr (\x acc -> if p x then x : acc else acc) []

-- define quicksort with filter'
-- filter' enables to define list simpler
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = let smaller = qsort (filter' (<x) xs)
                   larger  = qsort (filter' (>=x) xs)
               in smaller ++ [x] ++ larger

collatzSeq :: (Integral a) => a -> [a]
collatzSeq 1 = [1]
collatzSeq n
    | n <= 0 = error "number must be natural"
    | even n = n : collatzSeq (n `div` 2)
    | odd n  = n : collatzSeq (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map collatzSeq [1..100]))
    where isLong xs = length xs > 15

-- lambda sample 
numLongChains2 :: Int
numLongChains2 = length (filter (\xs -> length xs > 15) (map collatzSeq [1..100]))

-- foldr
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' k z xs = go xs
    where
        go [] = z
        go (y:ys) = y `k` go ys

-- foldl
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z xs = lgo z xs
    where
        lgo z [] = z
        lgo z (x:xs) = lgo (f z x) xs

-- foldl sample usage with lambda
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- It also can be defined more easily as below
sum'2 :: (Num a) => [a] -> a
sum'2 = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs

elem2' :: (Ord a) => a -> [a] -> Bool
elem2' _ [] = False
elem2' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- foldr1(and also foldl1) expects initial value of accumulator,
-- so it doesn't need init acc param
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: (Ord a) => [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

-- partial application sample with main
add :: Integer -> Integer -> Integer
add x y = x + y

main = print val
    where
        val = tmp 2
        tmp = add 5
