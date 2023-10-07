#include <iostream>
#include <string>
#include <vector>
#include <sstream>
#include "AstraLexer.h"

std::vector<std::string> splitByNewline(const std::string& s) {
    std::vector<std::string> result;
    std::istringstream iss(s);
    std::string line;

    while (std::getline(iss, line, '\n')) {
        result.push_back(line);
    }

    return result;
}

int main() {
    std::string input;
    std::cout << "Enter the input string: ";
    std::getline(std::cin, input);
    std::vector<std::string> lines = splitByNewline(input);

    for (const auto& line : lines) {
        std::vector<Token> tokens = Lexer(line);
        for (const Token& token : tokens) {
            std::cout << token.lexeme << " ";
        }
        std::cout << std::endl;
    }

    return 0;
}
