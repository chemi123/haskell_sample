-- 再帰の練習的な

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- おそらくここでのiはNumかつOrdの型クラスだと定義されている
-- replicates' :: (Num i, Ord i) => i -> a -> [a]
-- replicates' n x
--     | n <= 0 = []
--     | otherwise = x:replicates' (n - 1) x

replicates' :: (Num i, Ord i) => i -> a -> [a]
replicates' n x
    | n <= 0 = []
    | otherwise = x : replicates' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: (Ord a) => [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
