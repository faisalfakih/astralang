#include <iostream>
#include <string>
#include <vector>

enum TokenType {
    TOKEN_NUMBER, // Number
    TOKEN_PLUS, // '+'
    TOKEN_MINUS, // '-'
    TOKEN_ASTERISK, // '*'
    TOKEN_SLASH, // '/'
    TOKEN_LPAREN, // '('
    TOKEN_RPAREN, // ')'
    TOKEN_EOF, // End of File
    TOKEN_INVALID  // To indicate an unrecognized character
};

struct Token {
    TokenType type;
    std::string lexeme;
};

std::vector<Token> Lexer(const std::string& input) {
    std::vector<Token> tokens;
    for (int i = 0; i < input.size(); i++) {
        switch (input[i]) {
            case '+':
                tokens.push_back({TOKEN_PLUS, "+"});
                break;
            case '-':
                tokens.push_back({TOKEN_MINUS, "-"});
                break;
            case '*':
                tokens.push_back({TOKEN_ASTERISK, "*"});
                break;
            case '/':
                tokens.push_back({TOKEN_SLASH, "/"});
                break;
            case '(':
                tokens.push_back({TOKEN_LPAREN, "("});
                break;
            case ')':
                tokens.push_back({TOKEN_RPAREN, ")"});
                break;
            default:
                if (isdigit(input[i])) {
                    size_t start = i;
                    while (i < input.size() && isdigit(input[i])) {
                        i++;
                    }
                    tokens.push_back({TOKEN_NUMBER, input.substr(start, i - start)});
                    i--;
                } else if(isspace(input[i])) {
                    // Skip white spaces, do nothing
                } else {
                    tokens.push_back({TOKEN_INVALID, std::string(1, input[i])});
                }
                break;
        }
    }
    tokens.push_back({TOKEN_EOF, ""});
    return tokens;
}



int main() {
    std::string input = "1 + 2 * (3 - 4)";
    std::vector<Token> tokens = Lexer(input);
    for (auto token : tokens) {
        std::cout << token.lexeme;
    }
    return 0;
}
