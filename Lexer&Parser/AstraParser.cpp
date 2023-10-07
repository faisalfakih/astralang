#include <iostream>
#include <string>
#include <vector>
#include "AstraLexer.h"

int main() {
    std::string input = "fn main() { print(\"Hello, World!\"); }";
    std::vector<Token> lexer = Lexer(input);
    for (Token i : lexer) {
        std::cout << i.lexeme << ":" << i.line << "\n";
    }


    return 0;
}
