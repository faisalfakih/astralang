//
// Created by Faisal Fakih on 07/10/2023.
//

#ifndef ASTRALANG_ASTRALEXER_H
#define ASTRALANG_ASTRALEXER_H
#include <string>
#include <vector>
#include <cctype>
#include <unordered_map>

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
    TOKEN_LET, // 'let'
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
    TOKEN_RETURN, // 'return'
    TOKEN_IF, // 'if'
    TOKEN_ELSE, // 'else'
    TOKEN_WHILE, // 'while'
    TOKEN_FOR, // 'for'
    TOKEN_BREAK, // 'break'
    TOKEN_CONTINUE, // 'continue'
    TOKEN_TRUE, // 'true'
    TOKEN_FALSE, // 'false'
    TOKEN_TRY, // 'try'
    TOKEN_CATCH, // 'catch'
    TOKEN_FINALLY, // 'finally'
    TOKEN_THROW, // 'throw'
    TOKEN_PRINT, // 'print'
    TOKEN_READ, // 'read'
    TOKEN_EXCLAMATION_MARK, // '!'
    TOKEN_AMPERSAND, // '&'
    TOKEN_EQUAL_EQUAL,   // '=='
    TOKEN_NOT_EQUAL,     // '!='
    TOKEN_GREATER,       // '>'
    TOKEN_LESS,          // '<'
    TOKEN_GREATER_EQUAL, // '>='
    TOKEN_LESS_EQUAL,    // '<='
    TOKEN_AND_AND,       // '&&'
    TOKEN_OR_OR,          // '||'
    TOKEN_CHAR, // 'a'
    TOKEN_STRING, // "hello world"
    TOKEN_VECTOR, // 'vector';
    TOKEN_MAP, // 'map';
    TOKEN_UNORDERED_MAP, // 'unordered_map';
    TOKEN_STRUCT, // 'struct';
    TOKEN_CLASS, // 'class'
    TOKEN_COLON, // ':'
    TOKEN_THIS // 'this'
};

// Keyword List
const std::unordered_map<std::string, TokenType> keywordMap = {
        {"let", TOKEN_LET},
        {"int", TOKEN_INT_TYPE},
        {"short", TOKEN_SHORT_TYPE},
        {"long", TOKEN_LONG_TYPE},
        {"float", TOKEN_FLOAT_TYPE},
        {"double", TOKEN_DOUBLE_TYPE},
        {"char", TOKEN_CHAR_TYPE},
        {"string", TOKEN_STRING_TYPE},
        {"bool", TOKEN_BOOL_TYPE},
        {"void", TOKEN_VOID_TYPE},
        {"fn", TOKEN_FN},
        {"return", TOKEN_RETURN},
        {"if", TOKEN_IF},
        {"else", TOKEN_ELSE},
        {"while", TOKEN_WHILE},
        {"for", TOKEN_FOR},
        {"break", TOKEN_BREAK},
        {"continue", TOKEN_CONTINUE},
        {"true", TOKEN_TRUE},
        {"false", TOKEN_FALSE},
        {"try", TOKEN_TRY},
        {"catch", TOKEN_CATCH},
        {"finally", TOKEN_FINALLY},
        {"throw", TOKEN_THROW},
        {"print", TOKEN_PRINT},
        {"read", TOKEN_READ},
        {"vector", TOKEN_VECTOR},
        {"map", TOKEN_MAP},
        {"unordered_map", TOKEN_UNORDERED_MAP},
        {"struct", TOKEN_STRUCT},
        {"class", TOKEN_CLASS},
        {"this", TOKEN_THIS}
};


// Struct to represent a single token with its type and actual string representation (lexeme)
struct Token {
    TokenType type;
    std::string lexeme;
};

// Lexer function that takes an input string and returns a list of tokens
std::vector<Token> Lexer(const std::string& input) {
    std::vector<Token> tokens;  // List to store the recognized tokens
    for (size_t i = 0; i < input.size(); i++) {
        switch (input[i]) {
            case ' ':
            case '\t':
            case '\n':
            case '\r':
                break;
            case '\'': // Start of a character literal
            {
                size_t start = i + 1;
                i++;

                if (i >= input.size() || input[i] == '\'') {
                    // Handle error: Empty character literal
                    tokens.push_back({TOKEN_INVALID, "EMPTY_CHAR_LITERAL"});
                } else {
                    char charValue = input[i];
                    i++; // Move to the next character (should be the closing quote)

                    if (i >= input.size() || input[i] != '\'') {
                        // Handle error: Unterminated character literal
                        tokens.push_back({TOKEN_INVALID, "UNTERMINATED_CHAR"});
                    } else {
                        tokens.push_back({TOKEN_CHAR, std::string(1, charValue)});
                    }
                }
            }
                break;
            case '"': // Start of a string literal
            {
                size_t start = i + 1; // +1 to skip the opening quote
                do {
                    i++;
                } while (i < input.size() && input[i] != '"');

                if (i >= input.size()) {
                    // Handle error: Unterminated string literal
                    tokens.push_back({TOKEN_INVALID, "UNTERMINATED_STRING"});
                } else {
                    std::string strValue = input.substr(start, i - start);
                    tokens.push_back({TOKEN_STRING, strValue});
                }
            }
                break;
            case '=':
                if (i + 1 < input.size() && input[i + 1] == '=') {
                    tokens.push_back({TOKEN_EQUAL_EQUAL, "=="});
                    i++;
                } else {
                    tokens.push_back({TOKEN_EQUAL, "="});
                }
                break;
            case '+':
                tokens.push_back({TOKEN_PLUS, "+"});
                break;
            case '-':
                if (i + 1 < input.size() && input[i + 1] == '>') {
                    tokens.push_back({TOKEN_ARROW, "->"});
                    i++;
                } else {
                    tokens.push_back({TOKEN_MINUS, "-"});
                }
                break;
            case '*':
                tokens.push_back({TOKEN_ASTERISK, "*"});
                break;
            case '/':
                // Check for single-line comments
                if (i + 1 < input.size() && input[i + 1] == '/') {
                    while (i < input.size() && input[i] != '\n') i++;
                }
                    // Check for multi-line comments
                else if (i + 1 < input.size() && input[i + 1] == '*') {
                    i += 2;  // Move past '/*'
                    while (i < input.size() && !(input[i] == '*' && i + 1 < input.size() && input[i + 1] == '/')) i++;
                    if (i < input.size()) i++;  // Move past '*/'
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
            case ':':
                tokens.push_back({TOKEN_COLON, ":"});
                break;
            case '!':
                if (i + 1 < input.size() && input[i + 1] == '=') {
                    tokens.push_back({TOKEN_NOT_EQUAL, "!="});
                    i++;
                } else {
                    tokens.push_back({TOKEN_EXCLAMATION_MARK, "!"});
                }
                break;
            case '>':
                if (i + 1 < input.size() && input[i + 1] == '=') {
                    tokens.push_back({TOKEN_GREATER_EQUAL, ">="});
                    i++;
                } else {
                    tokens.push_back({TOKEN_GREATER, ">"});
                }
                break;
            case '<':
                if (i + 1 < input.size() && input[i + 1] == '=') {
                    tokens.push_back({TOKEN_LESS_EQUAL, "<="});
                    i++;
                } else {
                    tokens.push_back({TOKEN_LESS, "<"});
                }
                break;
            case '&':
                if (i + 1 < input.size() && input[i + 1] == '&') {
                    tokens.push_back({TOKEN_AND_AND, "&&"});
                    i++;
                } else {
                    tokens.push_back({TOKEN_AMPERSAND, "&"});
                }
                break;
            case '|':
                if (i + 1 < input.size() && input[i + 1] == '|') {
                    tokens.push_back({TOKEN_OR_OR, "||"});
                    i++;
                }
                break;
            default:
                // Handle identifiers and keywords
                if (isalpha(input[i]) || input[i] == '_') {
                    size_t start = i;
                    while (i < input.size() && (isalnum(input[i]) || input[i] == '_')) i++;
                    std::string lexeme = input.substr(start, i - start);

                    // Check for "else if"
                    if (lexeme == "else" && i + 2 < input.size() && input.substr(i, 3) == " if") {
                        lexeme += input.substr(i, 3);
                        i += 2; // Adjust for " if"
                    }

                    auto it = keywordMap.find(lexeme);
                    if (it != keywordMap.end()) {
                        tokens.push_back({it->second, lexeme});
                    } else {
                        tokens.push_back({TOKEN_IDENTIFIER, lexeme});
                    }
                    i--;  // Adjust for the loop's increment
                }
                    // Handle numbers
                else if (isdigit(input[i])) {
                    size_t start = i;
                    while (i < input.size() && isdigit(input[i])) i++;
                    // Handle floating point numbers
                    if (i < input.size() && input[i] == '.' && isdigit(input[i+1])) {
                        i++;
                        while (i < input.size() && isdigit(input[i])) i++;
                    }
                    std::string num = input.substr(start, i - start);
                    tokens.push_back({TOKEN_NUMBER, num});
                    i--;  // Adjust for the loop's increment
                }
                    // Invalid tokens
                else {
                    tokens.push_back({TOKEN_INVALID, std::string(1, input[i])});
                }
                break;
        }
    }
    tokens.push_back({TOKEN_EOF, ""});
    return tokens;
}

#endif //ASTRALANG_ASTRALEXER_H
