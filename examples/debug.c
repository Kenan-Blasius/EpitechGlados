#include "factorial.c"

fun debugTest (int a, char c, string s, float f) : (const my_int)
{
    print(a);
    print(c);
    print(s);
    print(f);

    string my_UTF16_string = "This is a string with UTF16 characters: éèàçù and some chinese characters: 你好吗？ and some emojis: 😂😄🤣🌟";
    char my_chinese_char = '你';
    char my_emoji_char = '😂';
    // Variable name with UTF16 characters
    int 你好吗 = 42;
    int 42var = 42;
    int forme = 42;

    float myFloat = -84.42;

    for (int i = 0; i < 10; i++) {
        a = a + 1;
    }

    int count = 0;
    while (count < 10) {
        count = count + 1;
    }
}

/*
* This is a multiline comment
* It should be ignored
* by the compiler more precisely by the parser
*/
fun factorialTest (int n, char c) /* lol this is a comment to try to break something */: (const my_int) // and this is an inline comment
{
    print(n);
    if (n < 0) {
        print("n should be positive");
        return 0;
    } else {
        print("n is positive. Let's compute the factorial!");
        if (n == 0) {
            return 1;
        } else if (n == 1) {
            return 1;
        }else {
            // here i break the line to see the AST split with multiple lines
            n = n * factorial(n-1);
            return n;
        }
    }
    print("This should not be printed");
}