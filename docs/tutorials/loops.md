# Tutoriel sur les boucles

## Définition

Une boucle est une structure qui permet de répéter plusieurs fois une partie du code.

## Structure

### While

La structure d'une boucle while est la suivante :

```c
while (condition) {
    // Instructions
}
```

Par exemple :

```c
int i = 0;
while (i < 10) {
    print(i);
    i = i + 1;
}
```

### For

La structure d'une boucle for est la suivante :

```c
for (initialisation; condition; incrémentation) {
    // Instructions
}
```

Par exemple :

```c
for (int i = 0; i < 10; i = i + 1) {
    print(i);
}
```

## Exercices

### Exercice 1

Écrire un programme qui affiche les nombres de 1 à 10.

### Exercice 2

Écrire un programme qui affiche les nombres de 10 à 1.

### Exercice 3

Écrire un programme qui affiche les nombres pairs de 1 à 10.

### Exercice 4

Écrire un programme qui affiche les nombres impairs de 1 à 10 en sautant les nombres 3 et 7.
