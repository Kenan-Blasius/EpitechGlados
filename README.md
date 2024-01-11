# GLaDOS

## [Documentation](https://kenan-blasius.github.io/glados-doc/)

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

## Syntaxe of Assembly

```py

; Opcode Definitions
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
```

1. **LOAD_CONST(index):** Load a constant value onto the stack. The `index` points to the position of the constant in a constant pool.

2. **LOAD_VAR(name):** Load the value of a variable onto the stack. The `name` is the identifier of the variable.

3. **STORE_VAR(name):** Store the value on top of the stack into the variable with the given `name`.

4. **BINARY_OP(operator):** Perform a binary operation on the top two values of the stack. The `operator` indicates the operation (addition, subtraction, multiplication, etc.).

5. **UNARY_OP(operator):** Perform a unary operation on the top value of the stack. The `operator` indicates the operation (negation, bitwise NOT, etc.).

6. **COMPARE_OP(operator):** Compare the top two values on the stack using the specified `operator` (equal, not equal, less than, greater than, etc.).

7. **JUMP_IF_TRUE(target):** Jump to the specified `target` instruction if the top value on the stack is true.

8. **JUMP_IF_FALSE(target):** Jump to the specified `target` instruction if the top value on the stack is false.

9. **JUMP(target):** Unconditional jump to the specified `target` instruction.

10. **JUMP_NEW_SCOPE(target):** Unconditional jump to the specified `target` instruction, and create a new empty scope of variables.

11. **POP:** Pop the top value from the stack.

12. **DUP:** Duplicate the top value on the stack.

13. **CALL(func_name, num_args):** Call a function with the specified `func_name` and `num_args` arguments.

14. **RETURN:** Return from the current function.

### Exemple

```c
fun main (int spain🇪🇸) : int
{
    return 0;
}
```

Is converted to this AST:

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

Which is converted to this bytecode:

```py
32 FunEntryPoint "main" IntType
37 StoreVarBefore spain🇪🇸 IntType  # save the variable spain🇪🇸 in the scope
43 LoadConst 0 IntType
49 Return
50 Return                          # double return in case of no main return
```

Become:

```py
32 Jump 37                          # jump to the main function, the 32 first bytes are for the header
37 StoreVar 0 IntType
43 LoadConst 0 IntType
49 Return
50 Return
```

The final bytecode is:

```py
# the magic number
122,105,122,105,
# the header: "This is the comment section\0"
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

Is converted to this AST:

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

Which is converted to this bytecode:

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

As we can see, the variable are stored as id, and the function are stored as id too.

Become:

```py
# the magic number
122,105,122,105,
# the header: "This is the comment section\0"
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

As we can see, the instructions `LoadConst`, `LoadVar` and `StoreVar` work like this:

```py

First byte:
0x01 # LOAD_CONST
0x02 # LOAD_VAR
0x03 # STORE_VAR

4 next bytes are the id of the variable or, the value of the constant

1 next byte is the type of the variable
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
all the syscalls

// done
exit (Syscall Number: 60): Terminate the process and return the exit status to the parent.

// done only for stdout
write (Syscall Number: 1): Write data to a file descriptor (e.g., printing to stdout).

```

## Auteurs

- [**Kenan Blasius**](https://github.com/Kenan-Blasius)
- [**Niels Ouvrard**](https://github.com/NielsOuvrard)
- [**Clément Montoya**](https://github.com/ClementMNT)
