-- myZip [1, 2, 3] [4, 5, 6] = [(1, 4), (2, 5), (3, 6)]

myZip :: [a] -> [b] -> [(a, b)]
myZip [] [] = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

-- >>> myZip [1, 2, 3] [4, 5, 6]
-- [(1,4),(2,5),(3,6)]

-- myZipWith (+) [1, 2, 3] [4, 5, 6] = [5, 7, 9]

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] [] = []
myZipWith f (x : xs) (y : ys) = (f x y) : myZipWith f xs ys

-- >>> myZipWith (+) [1, 2, 3] [4, 5, 6]
-- [5,7,9]

-- prefixes [3] = [[], [3]]
-- prefixes [2, 3] = [[], [2], [2, 3]]
--                 = [] : [[2], [2, 3]]
--                 = [] : [(2 : xs) | xs <- [[], [3]]]
--                 = [] : [(2 : xs) | xs <- prefixes [3]]

prefixes :: [a] -> [[a]]
prefixes [] = [[]]
prefixes (x : xs) = [] : [(x : xs') | xs' <- prefixes xs]

-- >>> prefixes [3]
-- [[],[3]]

-- >>> prefixes [2, 3]
-- [[],[2],[2,3]]

-- suffixes [2] = [2, []]
-- suffixes [2, 3] = [[2, 3], [2], []]
--                 = [2, 3] : [[2], []]
--                 = [2, 3] : suffixes [2]

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs = xs : suffixes xs'
  where
    (x : xs') = xs

-- >>> suffixes [2]
-- [[2],[]]

-- >>> suffixes [2, 3]
-- [[2,3],[3],[]]

-- interleave 1 [] = [[1]]
-- interleave 1 [3] = [[1, 3], [3, 1]]
--                  = [1, 3] : [(3 : xs) | xs <- [[1]]]
--                  = [1, 3] : [(3 : xs) | xs <- interleave 1 []]
-- interleave 1 [2, 3] = [[1, 2, 3], [2, 1, 3], [2, 3, 1]]
--                     = [1, 2, 3] : [[2, 1, 3], [2, 3, 1]]
--                     = [1, 2, 3] : [(2 : xs) | xs <- [[1, 3], [3, 1]]]
--                     = [1, 2, 3] : [(2 : xs) | xs <- interleave 1 [3]]

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : [(y : ys') | ys' <- interleave x ys]

-- >>> interleave 1 []
-- [[1]]

-- >>> interleave 1 [3]
-- [[1,3],[3,1]]

-- >>> interleave 1 [2, 3]
-- [[1,2,3],[2,1,3],[2,3,1]]

-- permutations [3] = [[3]]
-- permutations [2, 3] = [[2, 3], [3, 2]]
--                     = interleave 2 [3]
-- permutations [1, 2, 3] = [[1, 2, 3], [2, 1, 3], [2, 3, 1],
--                           [1, 3, 2], [3, 1, 2], [3, 2, 1]]
--                        = interleave 1 [2, 3] ++ interleave 1 [3, 2]
--                        = concat [(interleave 1 [2, 3]), (interleave 1 [3, 2])]
--                        = concat [interleave 1 xs | xs <- permutations [2, 3]]

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x : xs) = concat [interleave x xs' | xs' <- permutations xs]

-- safe 1 1 1 1 = True
-- safe 3 2 3 4 = False (Horizontal conflict)
-- safe 0 3 4 3 = False (Vertical conflict)
-- safe 0 0 1 1 = False (Diagonal conflict 1)
-- safe 2 1 4 3 = False (Diagonal conflict 2)
-- safe 0 3 1 1 = True

safe :: Int -> Int -> Int -> Int -> Bool
safe i j i' j' = (i == i' && j == j') || (i /= i' && j /= j' && (i + j') /= (j + i'))

-- >>> safe 1 1 1 1
-- True

-- >>> safe 3 2 3 4
-- False

-- >>> safe 0 3 4 3
-- False

-- >>> safe 0 0 1 1
-- False

-- >>> safe 2 1 4 3
-- False

-- >>> safe 0 3 1 1
-- True

nqueenOk :: [Int] -> Bool
nqueenOk xs = and [safe i (xs !! i) j (xs !! j) | i <- [0 .. length xs - 1], j <- [0 .. length xs - 1]]

nqueen :: Int -> [[Int]]
nqueen n = [xs | xs <- permutations [0 .. n - 1], nqueenOk xs]

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

t1 :: Tree Int
t1 = Node Leaf 1 Leaf

t3 :: Tree Int
t3 = Node Leaf 3 Leaf

t5 :: Tree Int
t5 = Node Leaf 5 Leaf

t4 :: Tree Int
t4 = Node t3 4 t5

t2 :: Tree Int
t2 = Node t1 2 t4

memberEq :: (Eq a) => a -> Tree a -> Bool
memberEq _ Leaf = False
memberEq x (Node l y r) = x == y || memberEq x l || memberEq x r

-- >>> memberEq 2 t2
-- True

-- >>> memberEq 2 t4
-- False

-- depth (Node Leaf () Leaf) = 1
-- depth (Node (Node Leaf () Leaf) () Leaf) = 2

depth :: Tree a -> Int
depth Leaf = 0
depth (Node l x r)
  | ld >= rd = 1 + ld
  | otherwise = 1 + rd
  where
    ld = depth l
    rd = depth r

-- >>> depth t2
-- 3

-- >>> depth t4
-- 2

-- >>> depth t1
-- 1

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node l x r) = inorder l ++ [x] ++ inorder r

-- >>> inorder t2
-- [1,2,3,4,5]
-- ==> t2 is a binary search tree

member :: (Ord a) => a -> Tree a -> Bool
member x Leaf = False
member x (Node l y r)
  | x == y = True
  | x < y = member x l
  | otherwise = member x r

-- >>> member 2 t2
-- True

-- >>> member 2 t4
-- False

-- myUnzip [(1, 2), (3, 4)] = ([1, 3], [2, 4])
-- myUnzip [(1, 2), (3, 4), (5, 6)] = ([1, 3, 5], [2, 4, 6])

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((a, b) : xs) = (a : as, b : bs)
  where
    (as, bs) = myUnzip xs

-- >>> myUnzip [(1, 2), (3, 4)]
-- ([1,3],[2,4])

-- >>> myUnzip [(1, 2), (3, 4), (5, 6)]
-- ([1,3,5],[2,4,6])

-- power set; 冪集合
-- power [2, 3] = [[], [3], [2], [2, 3]] を反転して解釈
-- power [] = [[]]
-- power [3] = [[3], []]
--           = [[3]] ++ [[]]
--           = [3 : xs | xs <- [[]]] ++ [[]]
--           = [3 : xs | xs <- power []] ++ power []
-- power [2, 3] = [[2, 3], [2], [3], []]
--              = [[2, 3], [2]] ++ [[3], []]
--              = [[2, 3], [2]] ++ [[3], []]
--              = [2 : xs | xs <- [[3], []]] ++ [[3], []]
--              = [2 : xs | xs <- power [3]] ++ power [3]
-- power [1, 2, 3] = [[1, 2, 3], [1, 2], [1, 3], [1], [2, 3], [2], [3], []]
--                 = [[1, 2, 3], [1, 2], [1, 3], [1]] ++ [[2, 3], [2], [3], []]
--                 = [1 : xs | xs <- [[2, 3], [2], [3], []]] ++ [[2, 3], [2], [3], []]
--                 = [1 : xs | xs <- power [2, 3]] ++ power [2, 3]

power :: [a] -> [[a]]
power [] = [[]]
power (x : xs) = [x : ys | ys <- zs] ++ zs
  where
    zs = power xs

magicSquareOk :: [Int] -> Bool
magicSquareOk (a : b : c : d : _ : f : g : h : i : []) =
  -- E = 5
  -- A + B + C = 15
  a + b + c == 15 -- Horizontal #1 (base)
    && d + f == 10 -- Horizontal #2
    && g + h + i == 15 -- Horizontal #3
    && a + d + g == 15 -- Vertical #1
    && b + h == 10 -- Vertical #2
    && c + f + i == 15 -- Vertical #3
    && a + i == 10 -- Diagonal #1
    && c + g == 10 -- Diagonal #2
magicSquareOk _ = False

magicSquare :: [[Int]]
magicSquare = [xs | xs <- permutations [1 .. 9], magicSquareOk xs]

add :: (Ord a) => a -> Tree a -> Tree a
add x Leaf = Node Leaf x Leaf
add x t@(Node l y r)
  | x < y = Node (add x l) y r
  | x > y = Node l y (add x r)
  | otherwise = t

searchMax :: Tree a -> Maybe a
searchMax Leaf = Nothing
searchMax (Node _ x Leaf) = Just x
searchMax (Node _ _ r) = searchMax r

deleteMax :: Tree a -> Tree a
deleteMax Leaf = Leaf
deleteMax (Node l _ Leaf) = l
deleteMax (Node l x r) = Node l x (deleteMax r)

delete :: (Ord a) => a -> Tree a -> Tree a
delete _ Leaf = Leaf
delete x (Node l y r)
  | x < y = Node (delete x l) y r
  | x > y = Node l y (delete x r)
  | Leaf <- r = l
  | Leaf <- l = r
  | otherwise = Node (deleteMax l) x' r -- 左から最大値を取得し、それと置き換え; 左は最大値部分を削除する
  where
    Just x' = searchMax l

-- >>> delete 4 t2
-- Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 (Node Leaf 5 Leaf))

-- >>> delete 2 t2
-- Node Leaf 1 (Node (Node Leaf 3 Leaf) 4 (Node Leaf 5 Leaf))
