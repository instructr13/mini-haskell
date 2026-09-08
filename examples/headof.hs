data Nat = Z | S Nat
data List = Nil | Cons Nat List
data Fail = Fail

headOf (x : xs) = x

main = headOf []
