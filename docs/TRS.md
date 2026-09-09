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
sectionSet = varSec? rulesSec

varSec = "(" "VAR" x* ")"

rulesSec = "(" "RULES" rule* ")"

# Rule
rule ::= t "->" t ";"?

# Applicative term
t ::= s+

# Simple expression (var or constant. let v as some ident, x if x ∈ V, c else)
s ::= x | c | "(" t ")"
```
