fun fibonacci (int n) : (int)
{
    if (n == 0) {
        return 0;
    } else if (n == 1) {
        return 1;
    }else {
        return (fibonacci(n-1) + fibonacci(n-2));
    }
}

fun main () : int
{
    int a = 5;

    a = fibonacci(a);

    return a;
}
