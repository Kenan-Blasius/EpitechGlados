# GLaDOS

## [Documentation](https://kenan-blasius.github.io/glados-doc/)

## Description

GLaDOS est un projet de création de langage de programmation.
Actuelement, il s'agit d'un interpréteur [Chez Scheme Lisp](https://github.com/cisco/ChezScheme/releases/tag/v9.5.8).

## Installation

Clonez le repo et lancez `make re`.

## Utilisation

Lancez `./glados <fichier>` pour exécuter un fichier (Chez Scheme lisp) tout comme vous executeriez un code python avec `python <fichier>`.

## Syntaxe of Assembly

```py

; Opcode Definitions
LOAD_CONST      0x01
LOAD_VAR        0x02
STORE_VAR       0x03
BINARY_OP       0x04
UNARY_OP        0x05
COMPARE_OP      0x06
JUMP_IF_TRUE    0x07
JUMP_IF_FALSE   0x08
JUMP            0x09
JUMP_REF        (only used internally)
POP             0x0A
DUP             0x0B
CALL            0x0C
RETURN          0x0D
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

10. **JUMP_REF(target):** Internal instruction used to reference a jump target.

11. **POP:** Pop the top value from the stack.

12. **DUP:** Duplicate the top value on the stack.

13. **CALL(func_name, num_args):** Call a function with the specified `func_name` and `num_args` arguments.

14. **RETURN:** Return from the current function.

### Exemple

```c
if (2 == 0) {
    return 1;
} else {
    return 2;
}
```

Is converted to this AST:

```haskell
-- AST
AST [IfAST (EqualAST (AST [IntAST 2]) (AST [IntAST 0])) (AST [ReturnAST (AST [IntAST 1])]) (AST [ElseAST (AST [ReturnAST (AST [IntAST 2])])])]


-- AST (human readable)
AST
|   IfAST
|   |   EqualAST
|   |   |   AST
|   |   |   |   IntAST 2
|   |   |   AST
|   |   |   |   IntAST 0
|   |   AST
|   |   |   ReturnAST
|   |   |   |   AST
|   |   |   |   |   IntAST 1
|   |   AST
|   |   |   ElseAST
|   |   |   |   AST
|   |   |   |   |   ReturnAST
|   |   |   |   |   |   AST
|   |   |   |   |   |   |   IntAST 2
```

Which is converted to this bytecode:

```python
LOAD_CONST(2)
LOAD_CONST(0)      # Push the constant 0 onto the stack
COMPARE_OP("==")   # Compare the top two values for equality
JUMP_IF_FALSE_BEFORE(1)   # Jump to JUMP_REF 1 if the comparison is false

LOAD_CONST(1)      # Push the constant 1 onto the stack
RETURN             # Return with the value 1
JUMP_BEFORE(2)            # Jump to JUMP_REF 2 (useless here, but needed if not returning)

JUMP_REF(1)        # reference for JUMP_IF_FALSE
LOAD_CONST(2)      # Push the constant 2 onto the stack
RETURN             # Return with the value 2
JUMP_REF(2)        # reference for JUMP
```

The jump references are calculated

```python
LOAD_CONST(2)
LOAD_CONST(0)
COMPARE_OP("==")
JUMP_IF_FALSE(13)  # Jump to JUMP_REF 1, which is at index 13, if the comparison is false

LOAD_CONST(1)
RETURN
JUMP(16)           # Jump to JUMP_REF 2, which is at index 16 (useless here, but needed if not returning)

LOAD_CONST(2)
RETURN
```

The final bytecode is:

```py

; Bytecode
1,2    ; LOAD_CONST 2
1,0    ; LOAD_CONST 0
6,61   ; COMPARE_OP =
8,13   ; JUMP_IF_FALSE 13
1,1    ; LOAD_CONST 1
13     ; RETURN
9,16   ; JUMP 16
1,2    ; LOAD_CONST 2
13     ; RETURN
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
-->

### Exemple 3

```c
int a = 0;

while (a < 10) {
    b = b + 1;
    if (b == 5) {
        a = 5;
    }
    a = a + 1;
}

```

```py

# here 8
| JUMP_REF 3
LOAD_VAR a
LOAD_CONST 10
COMPARE_OP <
| JUMP_IF_FALSE 2
    LOAD_VAR b
    LOAD_CONST 1
    BINARY_OP +
    STORE_VAR b
    LOAD_VAR b
    LOAD_CONST 5
    COMPARE_OP ==
    | JUMP_IF_FALSE 1
        LOAD_CONST 5
        STORE_VAR a
    | JUMP_REF 1
    LOAD_VAR a
    LOAD_CONST 1
    BINARY_OP +
    STORE_VAR a
    | JUMP 3
| JUMP_REF 2

## become

# here 8
LOAD_VAR a
LOAD_CONST 10
COMPARE_OP <
| JUMP_IF_FALSE 46
    LOAD_VAR b
    LOAD_CONST 1
    BINARY_OP +
    STORE_VAR b
    LOAD_VAR b
    LOAD_CONST 5
    COMPARE_OP == # 30
    | JUMP_IF_FALSE 36
        LOAD_CONST 5
        STORE_VAR a # 36
    LOAD_VAR a
    LOAD_CONST 1
    BINARY_OP +
    STORE_VAR a
| JUMP 8 # 46

1,0,3,97, # int a = 0;
1,0,3,98, # int b = 0;
2,97,1,10,6,60,8,46, # while (a < 10), jmp_if_false 46
    2,98,1,1,4,43,3,98, # b = b + 1;
    2,98,1,5,6,61,8,36, # if (b == 5), jmp_if_false 36
        1,5,3,97,2,97, # a = 5;
    1,1,4,43,3,97,9, # a = a + 1;

```

```c
all the syscalls

// done
exit (Syscall Number: 60): Terminate the process and return the exit status to the parent.

read (Syscall Number: 0): Read data from a file descriptor (e.g., reading from stdin).

// done only for stdout
write (Syscall Number: 1): Write data to a file descriptor (e.g., printing to stdout).

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
```

## Auteurs

- [**Kenan Blasius**](https://github.com/Kenan-Blasius)
- [**Niels Ouvrard**](https://github.com/NielsOuvrard)
- [**Clément Montoya**](https://github.com/ClementMNT)
