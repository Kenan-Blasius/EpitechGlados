# Fonctionnalités - Le type string

Le type string est un type de base de notre langage.

Il permet de manipuler des chaînes de caractères.

## Déclaration

Pour déclarer une variable de type string, il suffit de faire :

```c
string a = "Hello world!";
```

## Opérateurs

### Opérateur `+`

L'opérateur `+` permet de concaténer deux chaînes de caractères.

```c
string a = "Hello";
string b = " world!";
string c = a + b; // c = "Hello world!"
```

### Opérateur `==`

L'opérateur `==` permet de comparer deux chaînes de caractères.

```c
string a = "Hello";
string b = "World";
int c = a == b; // c = 0
```

### Opérateur `!=`

L'opérateur `!=` permet de comparer deux chaînes de caractères.

```c
string a = "Hello";
string b = "World";
int c = a != b; // c = 1
```

### Opérateur `<`

L'opérateur `<` permet de comparer la longueur de deux chaînes de caractères.

```c
string a = "Hello";
string b = "World";
int c = a < b; // c = 0
```

### Opérateur `<=`

L'opérateur `<=` permet de comparer la longueur de deux chaînes de caractères.

```c
string a = "Hello";
string b = "World";
int c = a <= b; // c = 1
```

### Opérateur `>`

L'opérateur `>` permet de comparer la longueur de deux chaînes de caractères.

```c
string a = "Hello";
string b = "World";
int c = a > b; // c = 0
```

### Opérateur `>=`

L'opérateur `>=` permet de comparer la longueur de deux chaînes de caractères.

```c
string a = "Hello";
string b = "World";
int c = a >= b; // c = 1
```

### Opérateur `[]`

L'opérateur `[]` permet d'accéder à un caractère d'une chaîne de caractères.

```c
string a = "Hello";
char b = a[0]; // b = 'H'
```

Un string se termine toujours par un caractère nul `\0`.

Si vous essayez d'accéder à un caractère en dehors de la chaîne de caractères, vous recevrez le dernier caractère (le caractère nul `\0`).

```c
string a = "Hello";
char b = a[10]; // b = '\0'
```

Si vous donnez un index négatif, vous recevrez le n-ième caractère en partant de la fin.

```c
string a = "Hello";
char b = a[-1]; // b = 'o'
char c = a[-4]; // c = 'e'
```

### Opérateur `+=`

L'opérateur `+=` permet de concaténer deux chaînes de caractères.

```c
string a = "Hello";
string b = " world!";
a += b; // a = "Hello world!"
```
