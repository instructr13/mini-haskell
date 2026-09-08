data Bool = False | True
data Nat  = Z | S Nat
data List = Nil | Cons Nat List

otherwise = True

append Nil         ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

leq Z     y     = True
leq (S x) Z     = False
leq (S x) (S y) = leq x y

qsort Nil         = Nil
qsort (Cons x xs) = split x xs Nil Nil

split w Nil         ys zs = append (qsort ys) (Cons w (qsort zs))
split w (Cons x xs) ys zs
  | leq w x   = split w xs ys (Cons x zs)
  | otherwise = split w xs (Cons x ys) zs

main = qsort (Cons (S (S Z)) (Cons Z (Cons (S (S (S (S Z))))
             (Cons (S Z) (Cons (S (S (S Z))) Nil)))))
