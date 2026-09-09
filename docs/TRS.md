# Term Rewriting System

## TRS Format in EBNF

```bnf
sectionSet = varSec? rulesSec

varSec = "(" "VAR" x* ")"

rulesSec = "(" "RULES" rule* ")" ";"?

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
sectionSet = dataSec* rulesSec?

# Data declaration (C is an upper case identifier, x a lower case one)
dataSec = "(" "DATA" C x* "=" conDecl ("|" conDecl)* ")"

conDecl = C typeAtom*

typeAtom = C | x | "(" C typeAtom* ")"

rulesSec = "(" "RULES" rule* ")"

# Rule
rule ::= t "->" t ";"

# Applicative term
t ::= s+ | t binOp t

# Simple expression
s ::= ident | numLiteral | listLiteral | "(" t ")"

numLiteral  ::= digit+                # desugars to Zero / Succ
listLiteral ::= "[" (t ("," t)*)? "]" # desugars to Nil / Cons
```
