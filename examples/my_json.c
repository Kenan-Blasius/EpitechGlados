// #include "examples/lib.cmm"


fun main () : int
{

    print("\tUser 1:\n");
    print("Enter a name:\n");
    string name = getline();

    print("Enter an age:\n");
    string age = getline();

    print("Enter a car:\n");
    string car = getline();


    writeInFile("file.json", "[\n\t{\n\t\t\"name\": \"" + name + "\",\n\t\t\"age\": \"" + age + "\",\n\t\t\"car\": \"" + car + "\"\n\t}");


    print("\tUser 2:\n");
    print("Enter a name:\n");
    string name = getline();

    print("Enter an age:\n");
    string age = getline();

    print("Enter a car:\n");
    string car = getline();

    appendInFile("file.json", ",\n\t{\n\t\t\"name\": \"" + name + "\",\n\t\t\"age\": \"" + age + "\",\n\t\t\"car\": \"" + car + "\"\n\t}\n]\n");

    return 0;
}
