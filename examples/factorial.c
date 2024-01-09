#define my_int int
#include "examples/debug.c"

fun factorial (int n) : (int)
{
    if (n == 0) {
        return 1;
    } else {
        return (n * factorial(n-1));
    }
}

fun main () : int
{
    int a = factorial(a); int b = factorialTest(b, 'b');

    print(a);
    print(b);

    return 0;
}