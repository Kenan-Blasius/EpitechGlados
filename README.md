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

1. **LOAD_CONST(index):** Load a constant value onto the stack. The `index` points to the position of the constant in a constant pool.

2. **LOAD_VAR(name):** Load the value of a variable onto the stack. The `name` is the identifier of the variable.

3. **STORE_VAR(name):** Store the value on top of the stack into the variable with the given `name`.

4. **BINARY_OP(operator):** Perform a binary operation on the top two values of the stack. The `operator` indicates the operation (addition, subtraction, multiplication, etc.).

5. **UNARY_OP(operator):** Perform a unary operation on the top value of the stack. The `operator` indicates the operation (negation, bitwise NOT, etc.).

6. **COMPARE_OP(operator):** Compare the top two values on the stack using the specified `operator` (equal, not equal, less than, greater than, etc.).

7. **JUMP_IF_TRUE(target):** Jump to the specified `target` instruction if the top value on the stack is true.

8. **JUMP_IF_FALSE(target):** Jump to the specified `target` instruction if the top value on the stack is false.

9. **JUMP(target):** Unconditional jump to the specified `target` instruction.

10. **POP:** Pop the top value from the stack.

11. **DUP:** Duplicate the top value on the stack.

12. **CALL(func_name, num_args):** Call a function with the specified `func_name` and `num_args` arguments.

13. **RETURN:** Return from the current function.

14. **BUILD_LIST(size):** Build a list on the stack with the specified `size`.

15. **INDEX:** Pop an index and a list from the stack, then push the corresponding element onto the stack.

16. **ATTRIBUTE(name):** Pop an object from the stack and push the value of the attribute with the given `name`.

17. **CREATE_OBJECT(num_attributes):** Create an object on the stack with the specified number of attributes.

### Exemple

```c
if (2 == 0) {
    return 1;
} else {
    return 2;
}
```

```haskell
-- AST
AST [IfAST (AST [IntAST 2,SymbolAST "==",IntAST 0]) (AST [AST [SymbolAST "return",IntAST 1]]) (AST [ElseAST (AST [AST [SymbolAST "return",IntAST 2]])])]


-- AST (human readable)
AST
|   IfAST
|   |   EqualAST
|   |   |   AST
|   |   |   |   IntAST 2
|   |   |   AST
|   |   |   |   IntAST 0
|   |   AST
|   |   |   AST
|   |   |   |   SymbolAST return
|   |   |   |   IntAST 1
|   |   AST
|   |   |   ElseAST
|   |   |   |   AST
|   |   |   |   |   AST
|   |   |   |   |   |   SymbolAST return
|   |   |   |   |   |   IntAST 2
```

```python
1. LOAD_CONST(2)
2. LOAD_CONST(0)      # Push the constant 0 onto the stack
3. COMPARE_OP("==")   # Compare the top two values for equality
4. JUMP_IF_FALSE(10)  # Jump to instruction 10 if the comparison is false

5. LOAD_CONST(1)      # Push the constant 1 onto the stack
6. RETURN             # Return with the value 1

10:                   # Instruction 10: Else clause
7. LOAD_CONST(2)      # Push the constant 2 onto the stack
8. RETURN             # Return with the value 2
```

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
POP             0x0A
DUP             0x0B
CALL            0x0C
RETURN          0x0D
BUILD_LIST      0x0E
INDEX           0x0F
ATTRIBUTE       0x10
CREATE_OBJECT   0x11

; Bytecode
01 0002         ; LOAD_CONST 2
01 0000         ; LOAD_CONST 0
06 0000         ; COMPARE_OP "=="
07 000A         ; JUMP_IF_FALSE 10

01 0001         ; LOAD_CONST 1
0D              ; RETURN

0A              ; Instruction 10: POP
01 0002         ; LOAD_CONST 2
0D              ; RETURN
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

### Exemple 2

```c
var a = 6 * 2;
```

```py
LOAD_CONST 6
LOAD_CONST 2
BINARY_OP MULT  ; Multiply 6 and 2
STORE_VAR a
```

### Exemple 3

```c
print(a);
```

```py
LOAD_VAR a
CALL 1         ; Assuming print is a function that prints the top of the stack
POP            ; Pop the result of print
```

```c
int a = 0;

while (a < 10) {
    a = a + 1;
}

return 0;
```

```py
# Bytecode
LOAD_CONST 0   ; Initialize a to 0
STORE_VAR a

LOOP_START:    ; Label for the beginning of the loop
LOAD_VAR a
LOAD_CONST 10
COMPARE_OP LESS_THAN   ; Compare a < 10
JUMP_IF_FALSE LOOP_END  ; Jump to LOOP_END if the comparison is false

# Body of the loop
LOAD_VAR a
LOAD_CONST 1
BINARY_OP ADD    ; Increment a by 1
STORE_VAR a
JUMP LOOP_START   ; Jump back to LOOP_START to repeat the loop

LOOP_END:        ; Label for the end of the loop
LOAD_CONST 0     ; Return 0
RETURN
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
