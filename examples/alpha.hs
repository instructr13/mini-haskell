data Nat = Z | S Nat

f x = g (S x)
  where
    g x = x

main = f 0
