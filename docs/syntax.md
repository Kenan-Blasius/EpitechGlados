# Syntaxe de notre langage

## Types

```bnf
<type> ::= "int" | "float" | "string" | "char"
```

## Variables

```bnf
<variable> ::= <type> <indent> <variable-name> [ "=" <expression> ] ";"

<variable-name> ::= <letter> | <variable-name> <letter> | <variable-name> <digit>
```

## Lettres

```bnf
<letter> ::= <utf16-char> - <excluded-letter>

<excluded-letter> ::= <special> | <space> | <tabulation> | <line-break>

<space> ::= " "

<tabulation> ::= "\t"

<line-break> ::= "\n"

<special> ::= "!" | "^" | "&" | "(" | ")" | "=" | "[" | "]" | "{" | "}" | "|" | "\" | ":" | ";" | "'" | '"' | "<" | ">" | "," | "." | "?" | "/" | "`" | "~" | "+" | "-" | "*" | "/" | "%"
```

## Chiffres

```bnf
<number> ::= <int> | <float>

<int> ::= <digit> | <int> <digit>

<float> ::= <int> "." <int>

<digit> ::= "0"..."9"
```

## Indent

```bnf
<indent> ::= <space> | <tabulation> | <indent> <indent>
```

## Expressions

```bnf
<expression> ::= <expression> <op> <expression>
                | "(" <expression> ")"
                | <indent>
                | <number>
                | <string>
                | <char>

<op> ::= "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "&&" | "||"

<string> ::= '"' <char>* '"'

<char> ::= <letter> | <digit> | <special> | <space> | <tabulation> | <line-break>
```
