import ballerina/io;

type R record {|
     int a;
     string foo;
|}

public function main() {
    io:println("Hello, World!");
}

function foo(int a, int b) returns int {
    int c = a * b;
    return c + a + b;
}

function bar(int a, string foo) returns R|error {
    return { a, foo };
}
