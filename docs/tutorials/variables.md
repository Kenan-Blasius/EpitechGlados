# Tutoriel sur les variables

## Définition

Une variable est un espace mémoire qui permet de stocker une valeur.

Cette valeur peut être de différents types.

## Informations

Pour plus d'informations sur les types et les noms des varibles, voir la page [Fonctionnalités - Les variables](../features/variables.md).

## Déclaration

En C--, une variable doit être déclarée avant d'être utilisée. La déclaration d'une variable se fait de la manière suivante :

```c
type nom_variable;
```

Par exemple :

```c
int a;
float b;
char c;
string d;
```

## Affectation

L'affectation d'une valeur à une variable se fait de la manière suivante :

```c
nom_variable = valeur;
```

Par exemple :

```c
a = 5;
b = 3.14;
c = 'a';
d = "Hello World!";
```

## Déclaration et affectation

Il est possible de déclarer et d'affecter une valeur à une variable en même temps :

```c
type nom_variable = valeur;
```

Par exemple :

```c
int a = 5;
float b = 3.14;
char c = 'a';
string d = "Hello World!";
```

## Utilisation

Une variable peut être utilisée dans une expression :

```c
int a = 5;
int b = 3;
int c = a + b; // c vaut 8
```

## Portée

Une variable a une portée, c'est-à-dire une zone du code dans laquelle elle est accessible.

En C--, une variable déclarée dans une fonction n'est accessible que dans cette fonction.

Par exemple :

```c
void fonction1() {
    int b = 3;
    int c = 5 + b; // c vaut 8
}

void fonction2() {
    int d = 5 + b; // erreur : b n'est pas accessible
}
```

<!-- ## Constantes

Une constante est une variable dont la valeur ne peut pas être modifiée.

En C--, une constante est déclarée de la manière suivante :

```c
const type nom_variable = valeur;
```

Par exemple :

```c
const int a = 5;
const float b = 3.14;
const char c = 'a';
const string d = "Hello World!";
``` -->

## Exercices

### Exercice 1

Écrire un programme qui déclare une variable de chaque type et qui leur affecte une valeur.

### Exercice 2

Maintenant, le programme doit afficher la valeur de chaque variable.

!!! note "Indice"
    Pour afficher une valeur, il faut utiliser la fonction `print`

### Exercice 3

Écrire un programme qui déclare deux variables de type entier et qui leur affecte une valeur.

Le programme doit ensuite afficher la somme de ces deux variables.
