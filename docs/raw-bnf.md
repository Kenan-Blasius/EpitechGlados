# Syntaxe BNF (Backus-Naur Form) brute du langage C--

```bnf
<type> ::= "int" | "float" | "string" | "char"

<variable> ::= <type> <indent> <variable-name> [ "=" <expression> ] ";"

<variable-name> ::= <letter> | <variable-name> <letter> | <variable-name> <digit>

<letter> ::= <utf16-char> - <excluded-letter>

<excluded-letter> ::= <special> | <space> | <tabulation> | <line-break>

<space> ::= " "

<tabulation> ::= "\t"

<line-break> ::= "\n"

<special> ::= "!" | "^" | "&" | "(" | ")" | "=" | "[" | "]" | "{" | "}" | "|" | "\" | ":" | ";" | "'" | '"' | "<" | ">" | "," | "." | "?" | "/" | "`" | "~" | "+" | "-" | "*" | "/" | "%"

<number> ::= <int> | <float>

<int> ::= <digit> | <int> <digit>

<float> ::= <int> "." <int>

<digit> ::= "0"..."9"

<indent> ::= <space> | <tabulation> | <indent> <indent>

<expression> ::= <expression> <op> <expression>
                | "(" <expression> ")"
                | <indent>
                | <number>
                | <string>
                | <char>

<op> ::= "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "&&" | "||"

<string> ::= '"' <char>* '"'

<char> ::= <utf16-char>

<statement> ::= ( <variable> | <expression> | <if> | <while> | <for> | <return> | <function-call> ) ";"

<return> ::= "return" <indent> ( <expression> | <variable> | <function-call> ) ";"

<function-call> ::= <function-name> "(" <function-call-args> ")"

<if> ::= "if" <indent> "(" <expression> ")" <indent> "{" <statement> "}" [ <else-if> ] [ <else> ]

<else-if> ::= "else" <indent> "if" <indent> "(" <expression> ")" <indent> "{" <statement> "}" [ <else-if> ] [ <else> ]

<else> ::= "else" <indent> "{" <statement> "}"

<while> ::= "while" <indent> "(" <expression> ")" <indent> "{" <statement> "}"

<for> ::= "for" <indent> "(" <variable> ";" <expression> ";" <expression> ")" <indent> "{" <statement> "}"

<function> ::= "fun" <indent> <function-name> "(" <function-args> ")" ":" <return-type> <function-body>

<function-name> ::= <letter> | <function-name> <letter> | <function-name> <digit>

<function-args> ::= <function-arg> | <function-arg> "," <function-args>

<function-arg> ::= <type> <indent> <variable-name>

<return-type> ::= <type>

<function-body> ::= <indent> <statement> | <indent> <statement> <function-body>

<comment> ::= "//" <utf16-char>* "\n"

<multiline-comment> ::= "/*" <utf16-char>* "*/"

<file> ::= <function> | <statement> | <comment> | <multiline-comment> | <file> <file>

<define> ::= "#define" <indent> <variable-name> <indent> <expression> <line-break>

<include> ::= "#include" <indent> <string> <line-break>
```
