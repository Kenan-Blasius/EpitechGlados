fun funk (int a) : int
{
    return a + 1;
}


fun main (int a) : int
{
    char o = '😂';

    print(o);
    o = funk(o);

    return 0;
}



// load this as ["funk" : [int a]]
37 FunEntryPoint "funk"
37 StoreVar a IntType
40 LoadVar a StringType
43 LoadConst 1 IntType
49 BinaryOp +
51 Return
52 Return

// load this as ["main" : [int a, char o]]
53 FunEntryPoint "main"
53 StoreVar a IntType
56 LoadConst 128514 CharType
62 StoreVar o CharType
65 LoadVar o StringType
68 Call 1
70 LoadVar o StringType
73 LoadPC
74 CallUserFun "funk"
79 StoreVar o StringType
82 LoadConst 0 IntType
88 Return
89 Return