-- >>> (\x -> x + 1) 10
-- 11

-- >>> (\x -> (\y -> x + y)) 10 20
-- 30

-- >>> (\x y -> x + y) 10 20
-- 30

-- myMap (* 10) [] = []
-- myMap (* 10) [-4] = [-40]
-- myMap (* 10) [3, -4] = [30, -40]
-- myMap (* 10) [-2, 3, -4] = [-20, 30, -40]
-- myMap (* 10) [1, -2, 3, -4] = [10, -20, 30, -40]
--                             = 10 : [-20, 30, -40]
-- myMap (* 10) [1, -2, 3, -4] = 10 : myMap (* 10) [-2, 3, 4]
-- myMap f      (x : xs)       = f x : myMap f xs
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x : xs) = f x : myMap f xs

-- >>> myMap (* 10) []
-- []

-- >>> myMap (* 10) [-4]
-- [-40]

-- >>> myMap (* 10) [3, -4]
-- [30,-40]

-- >>> myMap (* 10) [-2, 3, -4]
-- [-20,30,-40]

-- >>> myMap (* 10) [1, -2, 3, -4]
-- [10,-20,30,-40]

-- myFilter (> 0) [] = []
-- myFilter (> 0) [-4] = []
-- myFilter (> 0) [3, -4] = [3]
-- myFilter (> 0) [-2, 3, -4] = [3]
--                            = myFilter (> 0) [3, -4]
-- myFilter (> 0) [1, -2, 3, -4] = [1, 3]
--                               = 1 : [3]
--                               = 1 : myFilter (> 0) [-2, 3, -4]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x : xs)
  | p x = x : myFilter p xs
  | otherwise = myFilter p xs -- not (p x) では non-exhaustive。p xが停止する (値を返す) 保証がない

-- >>> myFilter (> 0) []
-- []

-- >>> myFilter (> 0) [-4]
-- []

-- >>> myFilter (> 0) [3, -4]
-- [3]

-- >>> myFilter (> 0) [-2, 3, -4]
-- [3]

-- >>> myFilter (> 0) [1, -2, 3, -4]
-- [1,3]

-- partition (> 0) [] = ([], [])
-- partition (> 0) [-4] = ([], [-4])
-- partition (> 0) [-2, 3, -4] = ([3], [-2, -4])
--                             = ([3], -2 : [-4])
--                               ((ys, zs) = partition (> 0) [3, -4] = ([3], [-4]))
-- partition (> 0) [1, -2, 3, -4] = ([1, 3], [-2, -4])
--                                = (1 : [3], [-2, -4])
--                                  ((ys, zs) = partition (> 0) [-2, 3, -4] = ([3], [-2, -4]))

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition _ [] = ([], [])
partition p (x : xs)
  | p x = (x : ys, zs)
  | otherwise = (ys, x : zs)
  where
    (ys, zs) = partition p xs

-- >>> partition (> 0) []
-- ([],[])

-- >>> partition (> 0) [-4]
-- ([],[-4])

-- >>> partition (> 0) [-2, 3, -4]
-- ([3],[-2,-4])

-- >>> partition (> 0) [1, -2, 3, -4]
-- ([1,3],[-2,-4])

-- foldl (-) 10 [1, 2, 3] = ((10 - 1) - 2) - 3 = 4
--                        = (((-) 10 1) - 2) - 3
--                        = foldl (10 - 1) [2, 3]
-- foldr (-) 10 [1, 2, 3] = 1 - (2 - (3 - 10)) = -8
--                        = (-) 1 (2 - (3 - 10))
--                        = (-) 1 (foldr 10 [2, 3])

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ e [] = e
myFoldl f e (x : xs) = myFoldl f (f e x) xs

myFoldr :: (b -> a -> a) -> a -> [b] -> a
myFoldr _ e [] = e
myFoldr f e (x : xs) = f x (myFoldr f e xs)

-- >>> foldl (-) 10 [1, 2, 3]
-- 4

-- >>> foldr (-) 10 [1, 2, 3]
-- -8

sumList :: [Int] -> Int
sumList xs = myFoldl (+) 0 xs

-- >>> sumList [1, 2, 3, 4, 5]
-- 15

-- oddplus1 xs = map (+ 1) (filter (\x -> mod x 2 == 1) xs)

oddplus1 :: [Int] -> [Int]
oddplus1 xs = [x + 1 | x <- xs, mod x 2 == 1]

-- >>> oddplus1 [1, 2, 3, 4, 5]
-- [2,4,6]

-- merge [] [] = []
-- merge [1] [] = [1]
-- merge [] [2] = [2]
-- merge [1] [2] = [1, 2]
-- merge [2] [1] = [1, 2]
-- merge [1, 3] [2, 4] = [1, 2, 3, 4]
--                     = 1 : 2 : merge [3] [4]
-- merge [3, 5] [2, 2] = [2, 2, 3, 4]
--                     = 2 : merge [3, 5] [2]
-- merge [1, 3, 5] [2, 4, 6] = [1, 2, 3, 4, 5, 6]

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x <= y = x : y : merge xs ys
  | otherwise = y : merge (x : xs) ys

-- >>> merge [] []
-- []

-- >>> merge [1] []
-- [1]

-- >>> merge [] [2]
-- [2]

-- >>> merge [1] [2]
-- [1,2]

-- >>> merge [2] [1]
-- [1,2]

-- >>> merge [1, 3] [2, 4]
-- [1,2,3,4]

-- >>> merge [3, 5] [2, 2]
-- [2,2,3,5]

-- >>> merge [1, 3, 5] [2, 4, 6]
-- [1,2,3,4,5,6]

-- split [] = ([], [])
-- split [1] = ([1], [])
-- split [1, 2] = ([1], [2])
--              = (1 : [], [2])
-- split [1, 2, 3, 4, 5, 6] = ([1, 3, 5], [2, 4, 6])
-- 2個ずつ取って分ける

split :: [Int] -> ([Int], [Int])
split [] = ([], [])
split [x] = ([x], [])
split (x : y : zs) = (x : z1s, y : z2s)
  where
    (z1s, z2s) = split zs

-- >>> split []
-- ([],[])

-- >>> split [1]
-- ([1],[])

-- >>> split [1, 2]
-- ([1],[2])

-- >>> split [1, 2, 3, 4, 5, 6]
-- ([1,3,5],[2,4,6])

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
  where
    (ys, zs) = split xs

-- >>> msort [5, 2, 3, 2]
-- [2,2,3,5]
