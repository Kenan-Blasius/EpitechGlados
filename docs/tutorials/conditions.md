# Tutoriel sur les conditions

## Définition

Une condition est une expression qui peut être vraie ou fausse.

Une condition permet de contrôler l'exécution d'un programme.

Si la condition est vraie, le programme exécute une partie du code, sinon il exécute une autre partie du code.

## Structure

### If

La structure d'une condition est la suivante :

```c
if (condition) {
    // Instructions
}
```

Par exemple :

```c
if (a == 5) {
    print("a est égal à 5");
}
```

### Else

Il est possible d'ajouter une alternative à une condition :

```c
if (condition) {
    // Instructions
} else {
    // Instructions
}
```

Par exemple :

```c
if (a == 5) {
    print("a est égal à 5");
} else {
    print("a n'est pas égal à 5");
}
```

### Else if

Il est possible d'ajouter plusieurs alternatives à une condition :

```c
if (condition1) {
    // Instructions
} else if (condition2) {
    // Instructions
} else {
    // Instructions
}
```

Par exemple :

```c
if (a == 5) {
    print("a est égal à 5");
} else if (a == 6) {
    print("a est égal à 6");
} else {
    print("a n'est ni égal à 5 ni égal à 6");
}
```

## Exercices

### Exercice 1

Écrire un programme qui compare deux nombres et qui affiche le plus grand.

### Exercice 2

Écrire un programme qui compare deux nombres et qui affiche le plus petit.

### Exercice 3

Écrire un programme qui compare deux nombres et qui affiche si ils sont égaux ou non.

### Exercice 4

Écrire un programme qui compare deux nombres et qui affiche si ils sont différents ou non.
