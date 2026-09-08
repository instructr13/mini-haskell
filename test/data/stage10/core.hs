data Nat = Z | S Nat
data List = Nil | Cons Nat List

add Z y = y
add (S x) y = S (add x y)

len Nil = Z
len (Cons x xs) = S (len xs)

main = len (Cons Z (Cons (S Z) Nil))
