# Term Rewriting System

## TRS Format in EBNF

```bnf
sectionSet = varSec? rulesSec

varSec = "(" "VAR" x* ")"

rulesSec = "(" "RULES" rule* ")"

# Rule
rule ::= t "->" t

# Term List
ts ::= t ("," t)+

# Term (var or signature. if x ∉ V, assume x as constant function)
t ::= x | f ("(" ts ")")?
```
