data Nat = Z | S Nat
data List = Nil | Cons Nat List

map f []       = []
map f (x : xs) = f x : map f xs

main = map (\x -> S (S x)) [0]
