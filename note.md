# Tests

## First test

```c
int a = 0;
int b = 0;

while (a < 10) {
    b = b + 1;
    if (b == 5) {
        a = 5;
        while (a < 20) {
            a = a + 1;
        }
    }
    a = a + 1;
}
```

```py

...
```

## Second test

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

## Third test

```c
int a = 1;

if (a == 0) {
    a = a + 1;
} else if (a == 1) {
    a = a + 2;
} else {
    a = a + 4;
}
```

```py



# Bytecode
LOAD_CONST 1   ; Initialize a to 1
STORE_VAR a

LOAD_VAR a
LOAD_CONST 0
COMPARE_OP EQUAL   ; Compare a == 0
JUMP_IF_FALSE 1

# IF branch 1
LOAD_VAR a
LOAD_CONST 1
BINARY_OP ADD    ; Increment a by 1
STORE_VAR a
JUMP 2

JUMP_REF 1
# LOAD_VAR a
# LOAD_CONST 1
# COMPARE_OP EQUAL   ; Compare a == 1
# JUMP_IF_FALSE 3

# # ELSE branch 1
# LOAD_VAR a
# LOAD_CONST 2
# BINARY_OP ADD    ; Increment a by 2
# STORE_VAR a
JUMP 2

JUMP_REF 3
LOAD_VAR a
LOAD_CONST 4
BINARY_OP ADD    ; Increment a by 4
STORE_VAR a

JUMP_REF 2
# Continue with the rest of the program or return a
```

fun main() : int
{
int a = -1;

    while (a > -10)
    {
        a--;
    }

    return a;

}

Optimisation:
Remarque de grande lanteur sur la fonction substringSearch.
Après recherches, cela vient de l'utilisation à chaque boucles de str_last_n_char.

Pensée: Aouter la possibilité de faire (string + int) pour decaler le string de int caractères

Avant:

```c
fun substringSearch(string mainString, string subString) : (int)
{
    int mainLength = strlen(mainString);
    int subLength = strlen(subString);

    for (int i = 0; i <= (mainLength - subLength); i++) {
        string last = str_last_n_char(mainString, mainLength - i);
        if (strncmp(mainString, subString, subLength)) {
            return i; // Substring found
        }
    }
    return -1; // Substring not found
}
```

```sh
➜  B-FUN-500-MAR-5-2-glados-niels.ouvrard git:(develop) ✗ time ./eval file.bin 2>> stderr.txt
Found at index: 36
./eval file.bin 2>> stderr.txt  3.27s user 1.89s system 119% cpu 4.305 total
```

Après l'optimisation:

```c
fun substringSearch(string mainString, string subString) : (int)
{
    int mainLength = strlen(mainString);
    int subLength = strlen(subString);

    for (int i = 0; i <= (mainLength - subLength); i++) {
        if (strncmp(mainString + i, subString, subLength)) {
            return i; // Substring found
        }
    }
    return -1; // Substring not found
}

```

```sh
➜  B-FUN-500-MAR-5-2-glados-niels.ouvrard git:(develop) ✗ time ./eval file.bin 2>> stderr.txt
Found at index: 36
./eval file.bin 2>> stderr.txt  0.06s user 0.05s system 50% cpu 0.222 total
```

On obtient un gain de temps de 4.305s à 0.222s
Ce gain est exponentiellement plus grand lorsque la taille des strings augmente.
