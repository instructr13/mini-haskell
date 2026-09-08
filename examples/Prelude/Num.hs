{-
Binary numerals: One = 1, O b = 2*b, I b = 2*b+1, so the outermost
constructor is the low bit and a numeral's size is logarithmic in its value.

There is no representation for zero -- One is 1 -- so `--numeric binary`
rejects the literal 0. The Peano Prelude is the one to use when zero is
needed; these functions deliberately do not share its names, because
without type classes one name cannot serve both types.
-}

data Bool = False | True
data Bin = One | O Bin | I Bin

sucB One   = O One
sucB (O b) = I b
sucB (I b) = O (sucB b)

addB One   b     = sucB b
addB (O a) One   = I a
addB (O a) (O b) = O (addB a b)
addB (O a) (I b) = I (addB a b)
addB (I a) One   = O (sucB a)
addB (I a) (O b) = I (addB a b)
addB (I a) (I b) = O (sucB (addB a b))

leqB One   b     = True
leqB (O a) One   = False
leqB (I a) One   = False
leqB (O a) (O b) = leqB a b
leqB (O a) (I b) = leqB a b
leqB (I a) (O b) = leqB (sucB a) b
leqB (I a) (I b) = leqB a b

main = sucB (sucB (sucB One))
