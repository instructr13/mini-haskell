data Nat = Z | S Nat

add 0     y = y
add (S x) y = S (add x y)

main = add 2 1
