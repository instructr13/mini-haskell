squareSum :: Int -> Int -> Int
squareSum x y = x * x + y * y

identity :: a -> a
identity x = x

sum1 :: Int -> Int
sum1 n =
  if n == 0 then 0 else n + sum1 (n - 1)

sum2 :: Int -> Int
sum2 n
  | n == 0 = 0
  | otherwise = n + sum2 (n - 1)

sum3 :: Int -> Int
sum3 0 = 0
sum3 n = n + sum3 (n - 1)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

myLength :: [a] -> Int
myLength [] = 0
myLength (x : xs) = 1 + myLength (xs)

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x : xs) ys = x : append xs ys

sumList :: [Int] -> Int
sumList [] = 0
sumList (x : xs) = x + sumList xs

-- evens [] = []
-- evens [10] = [10]
-- evens [10, 20] = [10]
-- evens [10, 20, 30] = 10 : evens [30] = [10, 30]

evens :: [a] -> [a]
evens [] = []
evens [x] = [x]
evens (x : y : zs) = x : evens zs

-- >>> evens []
-- []

-- >>> evens [10]
-- [10]

-- >>> evens [10, 20]
-- [10]

-- >>> evens [10, 20, 30]
-- [10,30]

-- x `mod` y -> y `mod` ans -> ans % (y `mod` ans) -> ...
myGcd :: Int -> Int -> Int
myGcd x 0 = x
myGcd x y
  | y > x = myGcd y x
  | otherwise = myGcd (x - y) y

-- range 10 15 = [10, 11, 12, 13, 14, 15]
-- range 10 9 = []

range :: Int -> Int -> [Int]
range x y | y < x = []
range x y | otherwise = x : range (x + 1) y

-- >>> range 10 15
-- [10,11,12,13,14,15]

-- >>> range 10 9
-- []

-- insert 5 [2, 2, 4, 6] = [2, 2, 4, 5, 6]
-- insert 7 [2, 2, 4, 6] = [2, 2, 4, 6, 7]
-- insert 5 [2, 2, 4, 6] -> 2 : insert 5 [2, 4, 6]
-- 2 : insert 5 [2, 4, 6] -> 2 : 2 : insert 5 [4, 6]
-- 2 : 2 : insert 5 [4, 6] -> 2 : 2 : 4 : insert 5 [6]
-- 2 : 2 : 4 : insert 5 [6] -> 2 : 2 : 4 : 5 : 6 : []

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y : ys)
  | x > y = y : insert x ys
  | otherwise = x : y : ys

-- >>> insert 5 [2, 2, 4, 6]
-- [2,2,4,5,6]

-- >>> insert 7 [2, 2, 4, 6]
-- [2,2,4,6,7]

-- >>> insert 5 [2, 2, 4, 6]
-- [2,2,4,5,6]

-- isort [5, 2, 3, 2]
-- insert 5 (insert 2 (insert 3 (insert 2 [])))
-- insert 5 (insert 2 (insert 3 [2]))
-- insert 5 (insert 2 [2, 3])
-- insert 5 [2, 2, 3]
-- [2, 2, 3, 5]

isort :: [Int] -> [Int]
isort [] = []
isort (x : xs) = insert x (isort xs)

-- >>> isort [5, 2, 3, 2]
-- [2,2,3,5]
