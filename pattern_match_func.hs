-- ghciで確認する用のもの
-- パターンマッチの関数を色々試している

-- これはパターンマッチではないが一応型を意識してみたかった
boomBang :: (Integral a) => [a] -> [String]
boomBang xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

lucky :: Integral a => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"   

charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil"  
-- 'a', 'b', 'c'以外を引数に取るとエラー

-- caseを使ってcharNameを別表現する
charName2 :: Char -> String
charName2 c = case c of 'a' -> "Albert"
                        'b' -> "Broseph"
                        'c' -> "Cecil"

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "singleton list."
                                               xs -> "a longer list."

-- whereで定義
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
-- (x:xs)と表現されている場合はxはheadでxsはtailを表す
-- 少しややこしいので注意
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- length' :: [a] -> Int  // OK
-- length' :: [a] -> Num  // NG
-- typeclassの場合はaのように何か変数的なもので表現しないとダメっぽい
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:x) = 1 + length' x

-- Note:
-- 何度も補足する場xはheadでxsはtailとなる
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- xs@(x:y:ys)と定義するとxsだけでx:y:ysと同等の表現ができる
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

-- 型クラスOrderingを満たすを型変数はGT, EQ, LTなど
-- 今の所なんのことかよくわかってない
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

-- where句でString引数のheadを定義
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname
-- let句を使った場合のワンライナー
-- initials firstname lastname = let (f:_) = firstname; (l:_) = lastname in [f] ++ ". " ++ [l] ++ "." 

-- where句で関数を定義
-- 型もしっかり定義できる
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi :: (RealFloat a) => a -> a -> a
          bmi weight height = weight / height ^ 2

-- letとリストを組み合わせるとよりシンプルにかける　
-- where句の方が読みやすい。letの方が可読性は落ちるがシンプル
calcBmis2 :: (RealFloat a) => [(a, a)] -> [a]
calcBmis2 xs = [let bmi w h = w / h ^ 2 in bmi w h | (w, h) <- xs]

-- 以下の書き方も可能。条件も与えられる
calcBmis3 :: (RealFloat a) => [(a, a)] -> [a]
calcBmis3 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

-- let句の利用
cylinder :: (RealFloat a) => a -> a -> a
cylinder h r =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in sideArea + topArea * 2
