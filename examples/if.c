#define my_int int
// #include "examples/debug.c"

fun factorial (int n) : (int)
{
    if (n == 0) {
        return 1;
    } else {
        // here i break the line to see the AST split with multiple lines
        n = n * factorial(n-1);
        return n;
    }
}

fun fibonacci (int n) : (int)
{
    if (n == 0) {
        return 0;
    } else if (n == 1) {
        return 1;
    } else {
        return fibonacci(n-1) + fibonacci(n-2);
    }
}

fun main () : int
{
    int a = factorial(5); int b = fibonacci(12);

    // print(a);
    // print(b);

    return a + b;
}