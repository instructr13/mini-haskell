data Nat = Z | S Nat
data List = Nil | Cons Nat List

firstTwo (x : y : ys) = y : x : ys
firstTwo xs           = xs

main = firstTwo [0, 1]
