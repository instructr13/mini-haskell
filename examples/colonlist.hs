data Nat = Z | S Nat
data List = Nil | Cons Nat List

length' []       = 0
length' (x : xs) = S (length' xs)

append []       ys = ys
append (x : xs) ys = x : append xs ys

reverse' []       = []
reverse' (x : xs) = append (reverse' xs) [x]

main = reverse' (append [0, 1] (2 : 3 : []))
