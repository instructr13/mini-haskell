data Bool = False | True
data Nat = Z | S Nat
data List = Nil | Cons Nat List

add 0     y = y
add (S x) y = S (add x y)

map f []       = []
map f (x : xs) = f x : map f xs

foldr f z []       = z
foldr f z (x : xs) = f x (foldr f z xs)

filter p []       = []
filter p (x : xs) = filterWith (p x) x (filter p xs)

filterWith True  x xs = x : xs
filterWith False x xs = xs

compose f g x = f (g x)

main = foldr (\x acc -> x : acc) [] [0, 1]
