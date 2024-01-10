# Syntaxe du langage C--

## BNF (Backus-Naur Form) brute

Si vous voulez voir la syntax BNF brute, sans les explications et les exemples, c'est par ici : [BNF brute](raw-bnf.md)

## Types

Notre langage supporte les types suivants :

| Type | Description |
| ---- | ----------- |
| `int` | Entier signé sur 32 bits |
| `float` | Nombre à virgule flottante sur 32 bits |
| `string` | Chaîne de caractères |
| `char` | Caractère |

Suivant la syntaxe de notre langage, les types sont définis comme suit :

```bnf
<type> ::= "int" | "float" | "string" | "char"
```

## Variables

Pour déclarer une variable, il faut préciser son type, son nom et éventuellement sa valeur, suivant la syntaxe suivante :

```bnf
<variable> ::= <type> <indent> <variable-name> [ "=" <expression> ] ";"

<variable-name> ::= <letter> | <variable-name> <letter> | <variable-name> <digit>
```

Voici quelques exemples de déclarations de variables :

```c
int a;
int b = 2;
float c = 3.14;
string d = "Hello world!";
char e = 'a';
```

## Lettres

Les lettres sont des caractères Unicode, à l'exception de certains caractères spéciaux, suivant la syntaxe suivante :

```bnf
<letter> ::= <utf16-char> - <excluded-letter>

<excluded-letter> ::= <special> | <space> | <tabulation> | <line-break>

<space> ::= " "

<tabulation> ::= "\t"

<line-break> ::= "\n"

<special> ::= "!" | "^" | "&" | "(" | ")" | "=" | "[" | "]" | "{" | "}" | "|" | "\" | ":" | ";" | "'" | '"' | "<" | ">" | "," | "." | "?" | "/" | "`" | "~" | "+" | "-" | "*" | "/" | "%"
```

Voici quelques exemples de lettres :

```c
int Hello_world;
float こんにちは世界;
char 你好世界;
string 안녕하세요 세계;
char 😄🤖👍;
```

## Chiffres

Les chiffres sont des caractères Unicode, suivant la syntaxe suivante :

```bnf
<number> ::= <int> | <float>

<int> ::= <digit> | <int> <digit>

<float> ::= <int> "." <int>

<digit> ::= "0"..."9"
```

Voici quelques exemples de chiffres :

```c
int a = 42;
float b = 3.14;
int c = 0;
float d = 0.0;
int e = -123456789;
float f = -123.456789;
```

## Indent

L'indentation est utilisée pour séparer les blocs de code. Elle est composée d'espaces et de tabulations, suivant la syntaxe suivante :

```bnf
<indent> ::= <space> | <tabulation> | <indent> <indent>
```

Voici quelques exemples d'indentation :

```c
int a = 42;
if (a == 42) {
    int b = 3.14;
    if (b == 3.14) {
        int c = 0;
    }
}
```

## Expressions

Une expression est une suite de caractères qui peut être évaluée en une valeur. Les expressions peuvent être utilisées dans les variables, les conditions, les boucles, les fonctions, etc.

Les expressions sont composées de variables, de chiffres, de chaînes de caractères, de caractères et d'opérateurs, suivant la syntaxe suivante :

```bnf
<expression> ::= <expression> <op> <expression>
                | "(" <expression> ")"
                | <indent>
                | <number>
                | <string>
                | <char>

<op> ::= "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "&&" | "||"

<string> ::= '"' <char>* '"'

<char> ::= <utf16-char>
```

Voici quelques exemples d'expressions :

```c
int a = 42;     // 42
int b = 3.14;   // 3.14
int c = a + b;  // 45
int d = a * b;  // 126
int e = a / b;  // 13
int f = a % b;  // 0
char g = 'a';   // a
string h = "Hello world!"; // Hello world!
char i = '😄';  // 😄
string j = "some \"string\" with \"quotes\" and chinese characters 你好世界"; // some "string" with "quotes" and chinese characters 你好世界
int k = 1 + 2 * 3;   // 7
int l = (1 + 2) * 3; // 9
int m = k == l;      // 0
```

## Statements

Un statement peut être une variable, une expression, une condition, une boucle, un retour ou un appel de fonction, suivant la syntaxe suivante :

```bnf
<statement> ::= ( <variable> | <expression> | <if> | <while> | <for> | <return> | <function-call> ) ";"
```

Voici quelques exemples de statements :

```c
int a = 42;
int b = 3.14;
int c = add(a, b);
if (a == 42) {
    c = 0;
}
while (c < 10) {
    c = c + 1;
}
for (int i = 0; i < 10; i = i + 1) {
    c = c + 1;
}
return c;
```

## Retours

Les retours sont utilisés pour retourner une valeur depuis une fonction, suivant la syntaxe suivante :

```bnf
<return> ::= "return" <indent> ( <expression> | <variable> | <function-call> ) ";"
```

Voici quelques exemples de retours :

```c
int a = 42;
return a;
```

```c
int a = 42;
int b = 3.14;
return add(a, b);
```

```c
int a = 42;
int b = 3.14;
return a + b;
```

## Appels de fonctions

Les appels de fonctions sont utilisés pour appeler une fonction avec des arguments, si elle en a, suivant la syntaxe suivante :

```bnf
<function-call> ::= <function-name> "(" <function-call-args> ")"
```

Voici quelques exemples d'appels de fonctions :

```c
int a = 42;
int b = 3.14;
int c = add(a, b);
```

```c
int a = 42;
int b = 3.14;
int c = addThree(a, b, 1);
```

```c
int a = getNumber();
```

## Conditions

Les conditions sont utilisées pour exécuter un bloc de code si une expression est vraie, suivant la syntaxe suivante :

```bnf
<if> ::= "if" <indent> "(" <expression> ")" <indent> "{" <statement> "}" [ <else-if> ] [ <else> ]

<else-if> ::= "else" <indent> "if" <indent> "(" <expression> ")" <indent> "{" <statement> "}" [ <else-if> ] [ <else> ]

<else> ::= "else" <indent> "{" <statement> "}"
```

Voici quelques exemples de conditions :

```c
int a = 42;
int b = 0;
if (a == 42) {
    b = 3.14;
}
```

```c
int a = 42;
int b = 0;
if (a != 42) {
    b = -1;
} else {
    b = 3.14;
}
```

```c
int a = 42;
int b = 0;
if (a > 42) {
    b = 1;
} else if (a < 0) {
    b = -1;
} else {
    b = 3.14;
}
```

## Boucles

Les boucles sont utilisées pour exécuter un bloc de code plusieurs fois, suivant la syntaxe suivante :

```bnf
<while> ::= "while" <indent> "(" <expression> ")" <indent> "{" <statement> "}"

<for> ::= "for" <indent> "(" <variable> ";" <expression> ";" <expression> ")" <indent> "{" <statement> "}"
```

Voici quelques exemples de boucles :

```c
int a = 0;
while (a < 10) {
    a = a + 1;
}
```

```c
int a = 0;
for (int i = 0; i < 10; i = i + 1) {
    a = a + 1;
}
```

## Fonctions

Les fonctions sont utilisées pour définir un bloc de code qui peut être appelé depuis d'autres blocs de code, en lui passant des arguments, si elle en a, suivant la syntaxe suivante :

```bnf
<function> ::= "fun" <indent> <function-name> "(" <function-args> ")" ":" <return-type> <function-body>

<function-name> ::= <letter> | <function-name> <letter> | <function-name> <digit>

<function-args> ::= <function-arg> | <function-arg> "," <function-args>

<function-arg> ::= <type> <indent> <variable-name>

<return-type> ::= <type>

<function-body> ::= <indent> <statement> | <indent> <statement> <function-body>
```

Voici quelques exemples de fonctions :

```c
fun add(int a, int b) : int {
    return a + b;
}
```

```c
fun addThree(int a, int b, int c) : int {
    return a + b + c;
}
```

```c
fun getNumber() : int {
    return 42;
}
```

## Commentaires

Les commentaires sont utilisés pour commenter le code, permettant de donner des informations sur le code, sans que cela ne modifie le comportement du programme, suivant la syntaxe suivante :

```bnf
<comment> ::= "//" <utf16-char>* "\n"

<multiline-comment> ::= "/*" <utf16-char>* "*/"
```

Voici quelques exemples de commentaires :

```c
// Voici un commentaire en fin de ligne
int a = 42; // Voici un commentaire en fin de ligne
int b = 3.14; // Voici un commentaire en fin de ligne
int c = add(a, b); // Voici un commentaire en fin de ligne
// Tout ce qui est après // est ignoré
```

```c
/*
Voici un commentaire
sur plusieurs lignes
qui peut être utilisé
sur une ou plusieurs lignes
*/
int a = 42;
int b = 3.14;
int c = /* Ce type de commentaire peut meme être intégrer en plein milieu du code */ add(a, b);
// Tout ce qui est entre /* et */ est ignoré
```

## Fichiers

Un fichier est composé de fonctions, de statements et de commentaires, un fichier doit contenir une fonction `main` ou être inclus dans un autre fichier, suivant la syntaxe suivante :

```bnf
<file> ::= <function> | <statement> | <comment> | <multiline-comment> | <file> <file>
```

Voici un exemple de fichier :

```c
fun add(int a, int b) : int {
    return a + b;
}

fun addThree(int a, int b, int c) : int {
    return a + b + c;
}

fun getNumber() : int {
    return 42;
}

fun main() : int {
    int a = 42;
    int b = 3.14;
    int c = add(a, b);
    if (a == 42) {
        c = 0;
    }
    while (c < 10) {
        c = c + 1;
    }
    for (int i = 0; i < 10; i = i + 1) {
        c = c + 1;
    }
    int d = getNumber();
    int e = addThree(a, b, d);
    return e;
}
```

## Define

Les define sont utilisés pour définir des constantes, suivant la syntaxe suivante :

```bnf
<define> ::= "#define" <indent> <variable-name> <indent> <expression> <line-break>
```

Voici quelques exemples de define :

```c
#define PI 3.14
#define HELLO_WORLD "Hello world!"
#define TRUE 1
#define FALSE 0

float a = PI;           // float a = 3.14;
string b = HELLO_WORLD; // string b = "Hello world!";
int c = TRUE;           // int c = 1;
int d = FALSE;          // int d = 0;
```

## Include

Les include sont utilisés pour inclure le contenu d'un fichier dans un autre fichier.
Les include récursifs sont bloqués pour éviter les boucles infinies.
La syntaxe est la suivante :

```bnf
<include> ::= "#include" <indent> <string> <line-break>
```

Voici quelques exemples d'include :

`getNumber.cmm` :

```c
fun getNumber() : int {
    return 42;
}
```

`main.cmm` :

```c
#include "getNumber.cmm"

fun main() : int {
    return getNumber();
}
```

Voici un exemple d'include récursif qui est bloqué :

`add.cmm` :

```c
fun add(int a, int b) : int {
    return a + b;
}

#include "main.cmm"
```

`main.cmm` :

```c
#include "add.cmm"

fun main() : int {
    return add(21, 21);
}
```

donne :

```c
fun add(int a, int b) : int {
    return a + b;
}

fun main() : int {
    return add(21, 21);
}
```

## Exemples

Voici quelques exemples de programmes en C--

### Factorielle et Fibonacci

`factorial.cmm` :

```c
fun factorial(int n) : int {
    if (n == 0) {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}
```

`fibonacci.cmm` :

```c
fun fibonacci(int n) : int {
    if (n == 0) {
        return 0;
    } else if (n == 1) {
        return 1;
    } else {
        return fibonacci(n - 1) + fibonacci(n - 2);
    }
}
```

`main.cmm` :

```c
#include "factorial.cmm"
#include "fibonacci.cmm"

fun main() : int {
    int a = factorial(5);
    int b = fibonacci(10);
    int c = 0;

    if (a < b) {
        c = a;
    } else {
        c = b;
    }

    return c;
}
```
