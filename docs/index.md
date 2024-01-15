# GLaDOS

## Description

GLaDOS est un projet de création de langage de programmation.

Ce projet est divisé en 2 parties :

1. Un compilateur
    1. Un Parser qui transforme un fichier de code en AST
        1. Un lexer qui transforme un fichier de code en tokens
        2. Un parser qui transforme les tokens en Sexpr
        3. Un parser qui transforme les Sexpr en AST
    2. Un compilateur qui transforme l'AST en bytecode puis en fichier binaire
        1. Un compilateur qui transforme l'AST en bytecode
        2. Un compilateur qui transforme le bytecode en fichier binaire
2. Une machine virtuelle
    1. Un programme qui lit un fichier binaire et l'execute

## Installation

### A partir d'un binaire

Téléchargez les binaires dans la section realease du repo.

### A partir du code source

Clonez le repo et lancez `make`.

## Syntaxe

La syntaxe du langage est décrite ici : [Syntaxe](syntax.md)

## Utilisation

Lancez `./glados <fichier>` pour compiler un fichier `.bin` puis `./eval <fichier.bin>` pour l'executer.

## Auteurs

- [**Kenan Blasius**](https://github.com/Kenan-Blasius)
- [**Niels Ouvrard**](https://github.com/NielsOuvrard)
- [**Clément Montoya**](https://github.com/ClementMNT)
