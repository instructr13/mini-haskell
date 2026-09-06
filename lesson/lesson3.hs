data Tree = Leaf | Node Tree Int Tree deriving (Show, Eq)

t1 :: Tree
t1 = Node Leaf 1 Leaf

t3 :: Tree
t3 = Node Leaf 3 Leaf

t5 :: Tree
t5 = Node Leaf 5 Leaf

t4 :: Tree
t4 = Node t3 4 t5

t2 :: Tree
t2 = Node t1 2 t4

-- >>> t2
-- Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 3 Leaf) 4 (Node Leaf 5 Leaf))

member :: Int -> Tree -> Bool
member _ Leaf = False
member x (Node l y r) =
  x == y || member x l || member x r

-- >>> member 3 t2
-- True

-- >>> member 6 t2
-- False

-- >>> lookup 1 [(1, "Jan"), (2, "Feb")]
-- Just "Jan"

-- >>> lookup 3 [(1, "Jan"), (2, "Feb")]
-- Nothing

myLookup :: (Eq a) => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup x ((x', y) : xys)
  | x == x' = Just y
  | otherwise = myLookup x xys

-- >>> myLookup 1 [(1, "Jan"), (2, "Feb")]
-- Just "Jan"

-- >>> myLookup 3 [(1, "Jan"), (2, "Feb")]
-- Nothing

preorder :: Tree -> [Int]
preorder Leaf = []
preorder (Node l x r) = [x] ++ preorder l ++ preorder r

-- >>> preorder t2
-- [2,1,4,3,5]

postorder :: Tree -> [Int]
postorder Leaf = []
postorder (Node l x r) = postorder l ++ postorder r ++ [x]

-- >>> postorder t2
-- [1,3,5,4,2]

-- 1と等しい要素を[2, 3, 3, 3, 2, 4, 1]から除外、2と等しい要素を[3, 3, 3, 2, 4]から除外、…

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x : xs) = x : nub [xi | xi <- xs, xi /= x]

-- >>> nub [1,2,3,3,3,2,4,1]
-- [1,2,3,4]

-- >>> nub "apple"
-- "aple"
