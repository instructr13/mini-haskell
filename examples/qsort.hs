data Bool = False | True
data Nat  = Z | S Nat
data List = Nil | Cons Nat List

otherwise = True

append []       ys = ys
append (x : xs) ys = x : append xs ys

leq 0     y     = True
leq (S x) 0     = False
leq (S x) (S y) = leq x y

qsort []       = []
qsort (x : xs) = split x xs [] []

split w []       ys zs = append (qsort ys) (w : qsort zs)
split w (x : xs) ys zs
  | leq w x   = split w xs ys (x : zs)
  | otherwise = split w xs (x : ys) zs

main = qsort [2, 0, 4, 1, 3]
