#include "examples/factorial.c"

fun debugTest (int a, char c, string s, float f) : (const my_int)
{
    print(a);
    print(c);
    print(s);
    print(f);

    // // PEMDAS check
    // int pemdas = (1 + -2) * 3 / 4 - 5;
    // int pemdasHardcore = (595 / (1 - 8) * 5) + 2 - 18 % (2 + 1);
    // int pemdasHardcore2 = (595 / (1 - 8) * 5) + 2 - 18 % (2 + 1) * 2;
    // int pemdasHardcore3 = ((20 + 5 * 3) - (7 % 3)) / 2 + (15 - 3 * 2) % 4;
    // int pemdasHardcore4 = (((20 + 5 * 3) - (7 % 3)) / 2 + (15 - 3 * 2) % 4) * ((12 / 6 + 3) - (2 * 4) % 5) + (((9 * 3) - (7 + 2)) / (4 % 3)) - ((18 / 2) + (5 * 2) % 3) + ((10 - 3) * (6 + 2 % 4)) / ((16 - 3 * 2) + (5 / 2)) % 7;
    // int pemdasHardcore5 = 20 + 5 * 3 - 7 % 3 / 2 + 15 - 3 * 2 % 4 * 12 / 6 + 3 - 2 * 4 % 5 + 9 * 3 - 7 + 2 / 4 % 3 / 18 / 2 + 5 * 2 % 3 + 10 - 3 * 6 + 2 % 4 / 16 - 3 * 2 + 5 / 2 % 7;

    string my_UTF16_string = "This is a string with UTF16 characters: éèàçù and some chinese characters: 你好吗？ and some emojis: 😂😄🤣🌟";
    string my_string = "This is a string with some \"escaped\" characters: \128514 and \r and \" and \\ and \n and \t and \'";
    char my_chinese_char = '你';
    char my_emoji_char = '😂';
    // Variable name with UTF16 characters
    int 你好吗 = 42;
    int 42var = 42;
    int forme = 42;
    forme = -forme;

    float myFloat = -84.42;

    for (int i = 0; i < 10; i++) {
        a = a + 1;
    }

    int count = 0;
    while (count < 10) {
        count = count + 1;
    }
}

// it's a comment
// it"s a comment
/*
* This is a multiline comment
* It should be ignored
* by the compiler more precisely by the parser
* it's having some issue for now with the " and the ' characters inside the comment (it's fixed now)
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