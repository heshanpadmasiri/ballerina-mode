import ballerina/io;

type R record {|
     int a; // inline comment
     string foo;
|}

public function main() {
    string a = string `dlae {4}`
    io:println("Hello, World!");
}

# block comment
function foo(int a, int b) returns int {
    int c = a * b; // ldalta
    int d = a / b;
    R a = {};
    return c + a + b;
}

function bar(int a, string foo) returns R|error {
    return { a, foo };
}

function baz(R a, string foo) returns R? {
    return { ...a, foo };
}
