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

//





pour les fonctions, on met le PC dans la stack, et on fait un jump au debut de la fonction
pour les return, on fait un jump au PC dans la stack

lancer le programme :
./glados simple.c
./eval file.bin
-->

## Auteurs

- [**Kenan Blasius**](https://github.com/Kenan-Blasius)
- [**Niels Ouvrard**](https://github.com/NielsOuvrard)
- [**Clément Montoya**](https://github.com/ClementMNT)
