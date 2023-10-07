#include <iostream>
#include "AstraLexer.h"

int main() {
    std::string input;
    std::cout << "Enter the input string: ";
    std::getline(std::cin, input);
    std::vector<Token> tokens = Lexer(input);
    for (const Token& token : tokens) {
        std::cout << token.lexeme << " ";
    }
    std::cout << std::endl;
    return 0; // fn f() -> int { return 0; }
}