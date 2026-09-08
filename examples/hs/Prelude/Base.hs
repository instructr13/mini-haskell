{-
Core Haskell EBNF:

module   ::= decl*

decl     ::= 'data' Con tyvar* '=' condecl ('|' condecl)*
           | var pat* '=' expr
           | ('infixl' | 'infixr' | 'infix') digit op+

condecl  ::= Con atype*

pat      ::= var | Con | '(' Con pat* ')'
expr     ::= var | Con | expr expr | '(' expr ')'
-}

{-
Operator Precedence (the defaults HS.Fixity assumes; a fixity declaration
in the source overrides them):

HIGHEST
10. f x - Function application (Left)
9. . - Function composition (Right)
8. ^ ^^ ** - Power (Right)
7. * / `quot` `rem` `div` `mod` (Left)
6. + - (Left)
5. : ++ - Append to list (Right)
4. == /= < <= >= > - Comparisons (Infix)
3. && - Logical AND (Right)
2. || - Logical OR (Right)
(Omit)
LOWEST
-}

data Bool = True | False ;
data Nat = Zero | Succ Nat ;
data List a = Nil | Cons a (List a) ;
data Tuple2 a b = Tuple2 a b ;
data Fail = Fail ;

infixr 2 || ;
infixr 3 && ;
infix 4 ==, /=, <, <=, >, >= ;
infixr 5 ++ ;
infixl 6 +, - ;
infixl 7 * ;

-- Bool

not :: Bool -> Bool ;
not True = False ;
not False = True ;

(&&) :: Bool -> Bool -> Bool ;
True && y = y ;
False && y = False ;

(||) :: Bool -> Bool -> Bool ;
True || y = True ;
False || y = y ;

-- Nat

(+) :: Nat -> Nat -> Nat ;
Zero + y = y ;
Succ x + y = Succ (x + y) ;

(*) :: Nat -> Nat -> Nat ;
Zero * y = Zero ;
Succ x * y = y + (x * y) ;

-- Truncated subtraction: Nat has no negatives, so 1 - 2 is Zero.
(-) :: Nat -> Nat -> Nat ;
Zero - y = Zero ;
Succ x - Zero = Succ x ;
Succ x - Succ y = x - y ;

(==) :: Nat -> Nat -> Bool ;
Zero == Zero = True ;
Zero == Succ y = False ;
Succ x == Zero = False ;
Succ x == Succ y = x == y ;

(<=) :: Nat -> Nat -> Bool ;
Zero <= y = True ;
Succ x <= Zero = False ;
Succ x <= Succ y = x <= y ;

(/=) :: Nat -> Nat -> Bool ;
x /= y = not (x == y) ;

(<) :: Nat -> Nat -> Bool ;
x < y = Succ x <= y ;

(>) :: Nat -> Nat -> Bool ;
x > y = y < x ;

(>=) :: Nat -> Nat -> Bool ;
x >= y = y <= x ;

max :: Nat -> Nat -> Nat ;
max Zero y = y ;
max (Succ x) Zero = Succ x ;
max (Succ x) (Succ y) = Succ (max x y) ;

min :: Nat -> Nat -> Nat ;
min Zero y = Zero ;
min (Succ x) Zero = Zero ;
min (Succ x) (Succ y) = Succ (min x y) ;

-- List

(++) :: List a -> List a -> List a ;
Nil ++ ys = ys ;
Cons x xs ++ ys = Cons x (xs ++ ys) ;

null :: List a -> Bool ;
null Nil = True ;
null (Cons x xs) = False ;

length :: List a -> Nat ;
length Nil = Zero ;
length (Cons x xs) = Succ (length xs) ;

head :: List a -> a ;
head Nil = Fail ;
head (Cons x xs) = x ;

tail :: List a -> List a ;
tail Nil = Fail ;
tail (Cons x xs) = xs ;

reverse :: List a -> List a ;
reverse Nil = Nil ;
reverse (Cons x xs) = reverse xs ++ Cons x Nil ;

elem :: Nat -> List Nat -> Bool ;
elem x Nil = False ;
elem x (Cons y ys) = (x == y) || elem x ys ;

sum :: List Nat -> Nat ;
sum Nil = Zero ;
sum (Cons x xs) = x + sum xs ;

product :: List Nat -> Nat ;
product Nil = 1 ;
product (Cons x xs) = x * product xs ;

sort :: List Nat -> List Nat ;
sort Nil = Nil ;
sort (Cons x xs) = joinParts x (partition x xs) ;

joinParts :: Nat -> Tuple2 (List Nat) (List Nat) -> List Nat ;
joinParts x (ys, zs) = sort ys ++ Cons x (sort zs) ;

-- partition p xs = (the elements of xs below p, the elements from p up)
partition :: Nat -> List Nat -> Tuple2 (List Nat) (List Nat) ;
partition p Nil = (Nil, Nil) ;
partition p (Cons x xs) = partitionWith (p <= x) x (partition p xs) ;

partitionWith ::
  Bool -> Nat -> Tuple2 (List Nat) (List Nat) -> Tuple2 (List Nat) (List Nat) ;
partitionWith True x (ys, zs) = (ys, Cons x zs) ;
partitionWith False x (ys, zs) = (Cons x ys, zs) ;

-- Tuple2

fst :: Tuple2 a b -> a ;
fst (Tuple2 a b) = a ;

snd :: Tuple2 a b -> b ;
snd (Tuple2 a b) = b ;

swap :: Tuple2 a b -> Tuple2 b a ;
swap (Tuple2 a b) = Tuple2 b a ;

-- A smoke test for :run. Sorts a list and pairs it with its sum.
main :: Tuple2 (List Nat) Nat ;
main = (sort [3, 1, 4, 1, 5], sum [1, 2, 3]) ;
