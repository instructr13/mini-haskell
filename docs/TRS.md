# Term Rewriting System

## TRS Format in EBNF

```bnf
sectionSet ::= varSec? rulesSec

varSec ::= "(" "VAR" x* ")"

rulesSec ::= "(" "RULES" rule* ")" ";"?

# Rule
rule ::= t "->" t

# Term List
ts ::= t ("," t)+

# Term (var or signature. if x ∉ V, assume x as constant function)
t ::= x | f ("(" ts ")")?
```

## Applicative TRS Format in EBNF

Applies functions with the [Juxtaposition Notation](https://en.wikipedia.org/wiki/Function_composition).

```bnf
sectionSet ::= dataSec* rulesSec?

# Data declaration (C is an upper case identifier, x a lower case one)
dataSec ::= "(" "DATA" C x* "=" conDecl ("|" conDecl)* ")"

conDecl ::= C typeAtom*

typeAtom ::= C | x | "(" C typeAtom* ")"

rulesSec ::= "(" "RULES" rule* ")"

# Rule
rule ::= t "->" t ";"

# Applicative term
t ::= app | t binOp t

# Function application
app = s s*

# Simple expression
s ::= "_" | ident | numLiteral | listLiteral | "(" t ")"

numLiteral  ::= digit+                # desugars to Zero / Succ
listLiteral ::= "[" (t ("," t)*)? "]" # desugars to Nil / Cons

tuple ::= "(" (t ("," t)?)? ")"       # desugars to Unit / Tuple2
```

## (Simplified) Haskell in EBNF

Simplified Haskell without any type information or special expressions.

```bnf
module ::= decl*

# Any declaration
# - Data declaration (C is an upper case ident, x a lower case one)
# - Rule declaration
decl ::= "data" C x* "=" conDecl ("|" conDecl)*
         | x apat* "=" t

# Constructors
conDecl  ::= C typeAtom*
typeAtom ::= x | C | "(" typeAtom* ")"

# Patterns
pat  ::= pat' (":" pat)?
pat' ::= C apat* | apat
apat ::= "_"
      | x
      | C
      | numLiteral
      | patListLiteral
      | "(" pat ")"
      | patTuple

# Terms
t    ::= app | t binOp t

# Function application
app  ::= s s*

# Simple expression
s    ::= x
       | C
       | numLiteral
       | termListLiteral
       | "(" t ")"
       | termTuple

# desugars to Zero / Succ
numLiteral  ::= digit+

# desugars to Nil / Cons
# pat doesn't allow terms in a list, so we define them separately
termListLiteral ::= "[" (t ("," t)*)? "]"
patListLiteral  ::= "[" (pat ("," pat)*)? "]"

# desugars to Unit / Tuple2
termTuple ::= "(" (t ("," t)?)? ")"
patTuple  ::= "(" (pat ("," pat)?)? ")"
```
