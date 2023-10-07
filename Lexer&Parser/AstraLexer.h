//
// Created by Faisal Fakih on 07/10/2023.
//

#ifndef ASTRALANG_ASTRALEXER_H
#define ASTRALANG_ASTRALEXER_H
#include <string>
#include <vector>
#include <cctype>

// Define the different types of tokens that our lexer can recognize
enum TokenType {
    TOKEN_EOF,      // End of File or end of input string
    TOKEN_INVALID,  // To indicate an unrecognized character
    TOKEN_NUMBER,   // Number
    TOKEN_EQUAL,    // '='
    TOKEN_PLUS,     // '+'
    TOKEN_MINUS,    // '-'
    TOKEN_ASTERISK, // '*'
    TOKEN_SLASH,    // '/'
    TOKEN_LPAREN,   // '('
    TOKEN_RPAREN,   // ')'
    TOKEN_INT_TYPE, // 'int'
    TOKEN_SHORT_TYPE, // 'short'
    TOKEN_LONG_TYPE, // 'long'
    TOKEN_FLOAT_TYPE, // 'float'
    TOKEN_DOUBLE_TYPE, // 'double'
    TOKEN_CHAR_TYPE, // 'char'
    TOKEN_STRING_TYPE, // 'string'
    TOKEN_BOOL_TYPE, // 'bool'
    TOKEN_VOID_TYPE, // 'void'
    TOKEN_IDENTIFIER, // Identifier
    TOKEN_SEMI_COLON, // ';'
    TOKEN_FN, // 'fn'
    TOKEN_ARROW, // '->'
    TOKEN_LBRACE, // '{'
    TOKEN_RBRACE, // '}'
    TOKEN_COMMA, // ','
    TOKEN_RETURN // 'return'
};

// Struct to represent a single token with its type and actual string representation (lexeme)
struct Token {
    TokenType type;
    std::string lexeme;
};

// Lexer function that takes an input string and returns a list of tokens
std::vector<Token> Lexer(const std::string& input) {
    std::vector<Token> tokens;  // List to store the recognized tokens
    for (int i = 0; i < input.size(); i++) {
        switch (input[i]) {
            case '=':
                tokens.push_back({TOKEN_EQUAL, "="});
                break;
            case '+':
                tokens.push_back({TOKEN_PLUS, "+"});
                break;
            case '-':
                if (i + 1 < input.size() && input[i + 1] == '>') {
                    tokens.push_back({TOKEN_ARROW, "->"});
                    i++;  // Move past '>'
                    continue;
                }
                tokens.push_back({TOKEN_MINUS, "-"});
                break;
            case '*':
                tokens.push_back({TOKEN_ASTERISK, "*"});
                break;
            case '/':
                // Check if the next character also is '/', indicating a single-line comment
                if (i + 1 < input.size() && input[i + 1] == '/') {
                    while (i < input.size() && input[i] != '\n') {
                        i++;
                    }
                    continue;
                }
                    // Check for start of multi-line comment '/*'
                else if (i + 1 < input.size() && input[i + 1] == '*') {
                    i += 2;  // Move past '/*'
                    while (i < input.size() && (input[i] != '*' || (i + 1 < input.size() && input[i + 1] != '/'))) {
                        i++;
                    }
                    if (i < input.size()) {
                        i++;  // Move past '*'
                    }
                    continue;
                } else {
                    tokens.push_back({TOKEN_SLASH, "/"});
                }
                break;
            case '(':
                tokens.push_back({TOKEN_LPAREN, "("});
                break;
            case ')':
                tokens.push_back({TOKEN_RPAREN, ")"});
                break;
            case ';':
                tokens.push_back({TOKEN_SEMI_COLON, ";"});
                break;
            case '{':
                tokens.push_back({TOKEN_LBRACE, "{"});
                break;
            case '}':
                tokens.push_back({TOKEN_RBRACE, "}"});
                break;
            case ',':
                tokens.push_back({TOKEN_COMMA, ","});
                break;
            default:
                if (isdigit(input[i])) {
                    size_t start = i;
                    while (i < input.size() && isdigit(input[i])) {
                        i++;
                    }
                    tokens.push_back({TOKEN_NUMBER, input.substr(start, i - start)});
                    i--;  // Decrement the index since the outer for-loop will increment it
                    continue;  // Continue to the next iteration of the for loop
                } else if (isspace(input[i])) {
                    continue;  // Ignore whitespaces and continue to the next iteration
                }
                // Recognize multi-character datatypes
                std::string potentialLexeme = input.substr(i, 6);  // maximum datatype length ('double')
                if (potentialLexeme.substr(0, 3) == "int") {
                    tokens.push_back({TOKEN_INT_TYPE, "int"});
                    i += 2;  // Adjust index since we've read 3 characters
                    continue;
                } else if (isalpha(input[i]) || input[i] == '_') {
                    size_t start = i;
                    while (i < input.size() && (isalnum(input[i]) || input[i] == '_')) {
                        i++;
                    }
                    tokens.push_back({TOKEN_IDENTIFIER, input.substr(start, i - start)});
                    i--;
                    continue;
                } else if (potentialLexeme.substr(0, 5) == "short") {
                    tokens.push_back({TOKEN_SHORT_TYPE, "short"});
                    i += 5;
                } else if (potentialLexeme.substr(0, 4) == "long") {
                    tokens.push_back({TOKEN_LONG_TYPE, "long"});
                    i += 4;
                } else if (potentialLexeme.substr(0, 5) == "float") {
                    tokens.push_back({TOKEN_FLOAT_TYPE, "float"});
                    i += 5;
                } else if (potentialLexeme.substr(0, 6) == "double") {
                    tokens.push_back({TOKEN_DOUBLE_TYPE, "double"});
                    i += 6;
                } else if (potentialLexeme.substr(0, 4) == "char") {
                    tokens.push_back({TOKEN_CHAR_TYPE, "char"});
                    i += 4;
                } else if (potentialLexeme.substr(0, 6) == "string") {
                    tokens.push_back({TOKEN_STRING_TYPE, "string"});
                    i += 6;
                } else if (potentialLexeme.substr(0, 4) == "bool") {
                    tokens.push_back({TOKEN_BOOL_TYPE, "bool"});
                    i += 4;
                } else if (potentialLexeme.substr(0, 4) == "void") {
                    tokens.push_back({TOKEN_VOID_TYPE, "void"});
                    i += 4;
                } else if (potentialLexeme.substr(0, 2) == "fn") {
                    tokens.push_back({TOKEN_FN, "fn"});
                    i += 2;
                } else if (potentialLexeme.substr(0, 6) == "return") {
                    tokens.push_back({TOKEN_RETURN, "return"});
                    i += 6;
                } else {
                    // If current character doesn't match any known token, mark it as invalid
                    tokens.push_back({TOKEN_INVALID, std::string(1, input[i])});
                }
                break;
        }
    }
    tokens.push_back({TOKEN_EOF, ""});  // Add an EOF token at the end to signify the end of the token list
    return tokens;
}

#endif //ASTRALANG_ASTRALEXER_H
