data Nat = Z | S Nat
data List = Nil | Cons Nat List

add 0     y = y
add (S x) y = S (add x y)

add3 x y z = add x (add y z)

map f []       = []
map f (x : xs) = f x : map f xs

addTo1 = add3 1

main = map (addTo1 1) [0, 1]
