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

## Statements

```bnf
<statement> ::= ( <variable> | <expression> | <if> | <while> | <for> | <return> | <function-call> ) ";"
```

## Retours

```bnf
<return> ::= "return" <indent> ( <expression> | <variable> | <function-call> ) ";"
```

## Appels de fonctions

```bnf
<function-call> ::= <function-name> "(" <function-call-args> ")"
```

## Conditions

```bnf
<if> ::= "if" <indent> "(" <expression> ")" <indent> "{" <statement> "}" [ <else-if> ] [ <else> ]

<else-if> ::= "else" <indent> "if" <indent> "(" <expression> ")" <indent> "{" <statement> "}" [ <else-if> ] [ <else> ]

<else> ::= "else" <indent> "{" <statement> "}"
```

## Boucles

```bnf
<while> ::= "while" <indent> "(" <expression> ")" <indent> "{" <statement> "}"

<for> ::= "for" <indent> "(" <variable> ";" <expression> ";" <expression> ")" <indent> "{" <statement> "}"
```

## Fonctions

```bnf
<function> ::= <type> <indent> <function-name> "(" <function-args> ")" ":" <return-type> <function-body>

<function-name> ::= <letter> | <function-name> <letter> | <function-name> <digit>

<function-args> ::= <function-arg> | <function-arg> "," <function-args>

<function-arg> ::= <type> <indent> <variable-name>

<return-type> ::= <type>

<function-body> ::= <indent> <statement> | <indent> <statement> <function-body>
```

## Commentaires

```bnf
<comment> ::= "//" <utf16-char>* "\n"

<multiline-comment> ::= "/*" <utf16-char>* "*/"
```

## Fichiers

```bnf
<file> ::= <function> | <statement> | <comment> | <multiline-comment> | <file> <file>
```
