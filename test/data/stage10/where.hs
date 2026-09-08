data Nat = Z | S Nat

f x = g x
  where
    g y = S y

main = f Z
