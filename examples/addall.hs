data Nat = Z | S Nat
data List = Nil | Cons Nat List

add 0     y = y
add (S x) y = S (add x y)

addAll n xs = go xs
  where
    go []       = []
    go (y : ys) = add n y : go ys

main = addAll 1 [0, 1]
