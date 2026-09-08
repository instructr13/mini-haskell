data Nat = Z | S Nat
data List = Nil | Cons Nat List

pred 0     = 0
pred (S n) = n

f 0 = []
f n = n : f (pred n)

main = f 3
