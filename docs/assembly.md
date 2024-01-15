
# Syntaxe de l'Assembly

## Opérations

```py

; Définitions des types
- LOAD_CONST      0x01
- LOAD_VAR        0x02
- STORE_VAR       0x03
- BINARY_OP       0x04
- UNARY_OP        0x05
- COMPARE_OP      0x06
- JUMP_IF_TRUE    0x07
- JUMP_IF_FALSE   0x08
- JUMP            0x09
- JUMP_NEW_SCOPE  0x0A
- POP             0x0B
- DUP             0x0C
- CALL            0x0D
- RETURN          0x0E
- LOAD_PC         0x0F
- INDEX           0x10
- SAVE_AT         0x11

```

1. **LOAD_CONST(index):** Charge une valeur constante sur la pile. L'`index` pointe vers la position de la constante dans un pool de constantes.

2. **LOAD_VAR(name):** Charge la valeur d'une variable sur la pile. Le `name` est l'identifiant de la variable.

3. **STORE_VAR(name):** Stocke la valeur en haut de la pile dans la variable avec le `name` donné.

4. **BINARY_OP(operator):** Effectue une opération binaire sur les deux valeurs supérieures de la pile. L'`operator` indique l'opération (addition, soustraction, multiplication, etc.).

5. **UNARY_OP(operator):** Effectue une opération unaire sur la valeur supérieure de la pile. L'`operator` indique l'opération (négation, NOT bit à bit, etc.).

6. **COMPARE_OP(operator):** Compare les deux valeurs supérieures de la pile en utilisant l'`operator` spécifié (égal, non égal, inférieur, supérieur, etc.).

7. **JUMP_IF_TRUE(target):** Saute à l'instruction `target` spécifiée si la valeur supérieure de la pile est vraie.

8. **JUMP_IF_FALSE(target):** Saute à l'instruction `target` spécifiée si la valeur supérieure de la pile est fausse.

9. **JUMP(target):** Saut inconditionnel vers l'instruction `target` spécifiée.

10. **JUMP_NEW_SCOPE(target):** Saut inconditionnel vers l'instruction `target` spécifiée et crée une nouvelle portée vide de variables.

11. **POP:** Dépile la valeur supérieure de la pile.

12. **DUP:** Duplique la valeur supérieure de la pile.

13. **CALL(func_name, num_args):** Appelle une fonction avec le `func_name` et `num_args` spécifiés comme arguments.

14. **RETURN:** Retourne de la fonction en cours.

15. **LOAD_PC:** Charge le compteur de programme sur la pile.

16. **INDEX:** Place dans la stack la valeur du string à l'index donné. L'index est le premier élément de la stack, la string est le deuxième.

17. **SAVE_AT:** Sauvegarde dans la string à l'index donné la valeur donnée. Le string est le premier élément de la stack, l'index le deuxième, la valeur le troisième.

## Exemples

### Exemple 1

```c
fun main (int spain🇪🇸) : int
{
    return 0;
}
```

Est converti en AST:

```haskell
-- AST
FunAST "main" (AST [IntTypeAST,SymbolAST "spain🇪🇸"]) (FunTypeAST (AST [IntTypeAST])) (ReturnAST (AST [IntAST 0]))


-- AST (human readable)
FunAST main
|   AST
|   |   IntTypeAST
|   |   SymbolAST "spain🇪🇸"
|   FunTypeAST
|   |   AST
|   |   |   IntTypeAST
|   ReturnAST
|   |   AST
|   |   |   IntAST 0
```

Qui est converti en bytecode:

```py
32 FunEntryPoint "main" IntType
37 StoreVarBefore spain🇪🇸 IntType  # sauvagarde de la variable spain🇪🇸
43 LoadConst 0 IntType
49 Return
50 Return                          # return deus fois si l'utilisateur ne fait pas de return depuis le main
```

Devient:

```py
32 Jump 37                          # saute dans la fonction main, les 32 premiers octets sont le header
37 StoreVar 0 IntType
43 LoadConst 0 IntType
49 Return
50 Return
```

Le bytecode final est:

```py
# le magic number
122,105,122,105,
# Le header: "This is the comment section\0"
84,104,105,115,32,105,115,32,116,104,101,32,99,111,109,109,101,110,116,32,115,101,99,116,105,111,110,0,

9,37,0,0,0,  # Jump 37
3,0,0,0,0,1, # StoreVar 0 IntType
1,0,0,0,0,1, # LoadConst 0 IntType
14,          # Return
14           # Return
```

### Exemple 2

```c
fun add (int a, int b) : (int)
{
    return a + b;
}

fun main () : int
{
    int south_corea🇰🇷 = 7;
    int north_corea🇰🇵 = 3;

    int corea = add(south_corea🇰🇷, north_corea🇰🇵);

    return corea;
}
```

Est converti en AST:

```haskell

AST
|   FunAST add
|   |   AST
|   |   |   AST
|   |   |   |   IntTypeAST
|   |   |   |   SymbolAST a
|   |   |   AST
|   |   |   |   IntTypeAST
|   |   |   |   SymbolAST b
|   |   FunTypeAST
|   |   |   AST
|   |   |   |   AST
|   |   |   |   |   IntTypeAST
|   |   ReturnAST
|   |   |   PlusAST
|   |   |   |   AST
|   |   |   |   |   SymbolAST a
|   |   |   |   AST
|   |   |   |   |   SymbolAST b
|   FunAST main
|   |   DeadLeafAST
|   |   FunTypeAST
|   |   |   AST
|   |   |   |   IntTypeAST
|   |   AST
|   |   |   AssignAST
|   |   |   |   AST
|   |   |   |   |   IntTypeAST
|   |   |   |   |   SymbolAST south_corea🇰🇷
|   |   |   |   AST
|   |   |   |   |   IntAST 7
|   |   |   AssignAST
|   |   |   |   AST
|   |   |   |   |   IntTypeAST
|   |   |   |   |   SymbolAST north_corea🇰🇵
|   |   |   |   AST
|   |   |   |   |   IntAST 3
|   |   |   AssignAST
|   |   |   |   AST
|   |   |   |   |   IntTypeAST
|   |   |   |   |   SymbolAST corea
|   |   |   |   AST
|   |   |   |   |   SymbolAST add
|   |   |   |   |   AST
|   |   |   |   |   |   AST
|   |   |   |   |   |   |   SymbolAST south_corea🇰🇷
|   |   |   |   |   |   AST
|   |   |   |   |   |   |   SymbolAST north_corea🇰🇵
|   |   |   ReturnAST
|   |   |   |   AST
|   |   |   |   |   SymbolAST corea
```

Qui est converti en bytecode:

```py
32 Jump 65
# add
37 StoreVar 0 IntType
43 StoreVar 1 IntType
49 LoadVar 0 IntType
55 LoadVar 1 IntType
61 BinaryOp +
63 Return
64 Return
# main
65 LoadConst 7 IntType
71 StoreVar 0 IntType
77 LoadConst 3 IntType
83 StoreVar 1 IntType
89 LoadVar 0 IntType
95 LoadVar 1 IntType
101 LoadPC
102 JumpNewScope 37
107 StoreVar 2 IntType
113 LoadVar 2 IntType
119 Return
120 Return
```

Comme on peut le voir, les variables sont stockées sous forme d'id, et les fonctions aussi.

Devient:

```py
# Le magic number
122,105,122,105,
# Le header: "This is the comment section\0"
84,104,105,115,32,105,115,32,116,104,101,32,99,111,109,109,101,110,116,32,115,101,99,116,105,111,110,0,

9,65,0,0,0,  # Jump 65
3,0,0,0,0,1, # StoreVar 0 IntType
3,1,0,0,0,1, # StoreVar 1 IntType
2,0,0,0,0,1, # LoadVar 0 IntType
2,1,0,0,0,1, # LoadVar 1 IntType
4,43,        # BinaryOp +
14,          # Return
14,          # Return
1,7,0,0,0,1, # LoadConst 7 IntType
3,0,0,0,0,1, # StoreVar 0 IntType
1,3,0,0,0,1, # LoadConst 3 IntType
3,1,0,0,0,1, # StoreVar 1 IntType
2,0,0,0,0,1, # LoadVar 0 IntType
2,1,0,0,0,1, # LoadVar 1 IntType
15,          # LoadPC
10,37,0,0,0, # JumpNewScope 37
3,2,0,0,0,1, # StoreVar 2 IntType
2,2,0,0,0,1, # LoadVar 2 IntType
14,          # Return
14           # Return
```

-- LOAD_CONST 0x01
-- LOAD_VAR 0x02
-- STORE_VAR 0x03

Comme on peut le voir, les instructions `LoadConst`, `LoadVar` et `StoreVar` fonctionnent comme ceci:

```py

First byte:
0x01 # LOAD_CONST
0x02 # LOAD_VAR
0x03 # STORE_VAR

4 octets suivants sont l id de la variable ou, la valeur de la constante

1 octet suivant est le type de la variable

```

<!-- ## TODO
// faire header au bytecode
// organisation

// appel malloc.c ffi
// appel system (malloc de haskell en gros)
// refaire malloc

// faire le static
// ensuite les str
// pas de concat

// include



pour les fonctions, on met le PC dans la stack, et on fait un jump au debut de la fonction
pour les return, on fait un jump au PC dans la stack

lancer le programme :
./glados simple.c
./eval file.bin

read (Syscall Number: 0): Read data from a file descriptor (e.g., reading from stdin).

open (Syscall Number: 2): Open a file or device for reading, writing, or both.

close (Syscall Number: 3): Close a file descriptor.

fork (Syscall Number: 57): Create a new process (child process).

execve (Syscall Number: 59): Execute a program.

waitpid (Syscall Number: 61): Wait for a specific child process to exit.

kill (Syscall Number: 62): Send a signal to a process.

stat (Syscall Number: 4): Get file status.

chdir (Syscall Number: 12): Change the current working directory.

getpid (Syscall Number: 39): Get the process ID.

getuid (Syscall Number: 102): Get the user ID.

getgid (Syscall Number: 104): Get the group ID.

socket (Syscall Number: -1): Create an endpoint for communication (varies between operating systems).
-->

```c
Les syscalls

exit (Syscall Number: 60): Termine le processus et retourne le status de sortie au parent.

write (Syscall Number: 1): Ecrire dans le stdout

```
