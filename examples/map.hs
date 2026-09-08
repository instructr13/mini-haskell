data Nat = Z | S Nat
data List = Nil | Cons Nat List

add 0     y = y
add (S x) y = S (add x y)

map f []       = []
map f (x : xs) = f x : map f xs

main = map (add 1) [0, 1]
