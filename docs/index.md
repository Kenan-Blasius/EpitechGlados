# GLaDOS

## Description

GLaDOS est un projet de création de langage de programmation.
Ce projet est divisé en 2 parties :

- Un compilateur
  - Un Parser qui transforme un fichier de code en AST
  - Un compilateur qui transforme l'AST en bytecode puis en fichier binaire
- Une machine virtuelle
  - Un programme qui lit un fichier binaire et l'execute

## Installation

Clonez le repo et lancez `make re`.

## Syntaxe

La syntaxe du langage est décrite dans ici : [Syntaxe](syntax.md)

## Utilisation

Lancez `./glados <fichier>` pour compiler un fichier `.bin` puis `./eval <fichier.bin>` pour l'executer.

## Auteurs

- [**Kenan Blasius**](https://github.com/Kenan-Blasius)
- [**Niels Ouvrard**](https://github.com/NielsOuvrard)
- [**Clément Montoya**](https://github.com/ClementMNT)
