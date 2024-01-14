# Fonctionnalités - Les variables

## [Tutoriel sur les variables](tutorials/variables.md)

## Fonctionnalités

### Types

Les types disponibles sont les suivants :

| Type     | Description |
| -------- | ----------- |
| `int`    | Entier |
| `float`  | Flottant |
| `char`   | Caractère |
| `string` | Chaîne de caractères |

### Déclaration

Le nom de la variable peut contenir n'importe quel caractère présent dans l'UTF-16.

Par exemple :

```c
// int en français
int mon_entier;
// float en chinois
float 浮点数;
// char en japonais
char 文字;
// string en russe
string Строка;
// int en emoji
int 🤖👍;
```

### Affectation

Les char et string peuvent contenir des caractères UTF-16.

Par exemple :

```c
mon_entier = 5;
浮点数 = 3.14;
文字 = 'a';
Строка = "Hello World!😄";
🤖👍 = 42;
```
