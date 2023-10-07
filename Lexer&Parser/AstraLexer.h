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
    TOKEN_CHAR_LITERAL, // 'a'
    TOKEN_STRING, // "hello world"
    TOKEN_VECTOR, // 'vector';
    TOKEN_MAP, // 'map';
    TOKEN_UNORDERED_MAP, // 'unordered_map';
    TOKEN_STRUCT, // 'struct';
    TOKEN_CLASS, // 'class'
    TOKEN_COLON, // ':'
    TOKEN_THIS, // 'this'
    TOKEN_LSQUARE, // '['
    TOKEN_RSQUARE // ']'
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
    size_t line;
    size_t column;
};

std::vector<Token> Lexer(const std::string& input) {
    std::vector<Token> tokens;
    size_t line = 1;
    size_t column = 0;

    for (size_t i = 0; i < input.size(); i++) {
        column++;
        switch (input[i]) {
            case ' ':
            case '\t':
                break;
            case '\n':
                line++;
                column = 0;
                break;
            case '\r':
                break;
            case '"':
            {
                size_t start = i + 1;
                i++;
                while (i < input.size() && input[i] != '"') {
                    column++;
                    if (input[i] == '\n') {
                        line++;
                        column = 0;
                    }
                    i++;
                }
                if (i == input.size()) {
                    tokens.push_back({TOKEN_INVALID, "UNTERMINATED_STRING", line, column});
                } else {
                    std::string strValue = input.substr(start, i - start);
                    tokens.push_back({TOKEN_STRING, strValue, line, column});
                }
            }
                break;
            case '\'':
            {
                size_t start = i + 1;
                i++;
                while (i < input.size() && input[i] != '\'') {
                    column++;
                    if (input[i] == '\n') {
                        line++;
                        column = 0;
                    }
                    i++;
                }
                if (i == input.size() || i - start > 1) { // character literals should only be one character long
                    tokens.push_back({TOKEN_INVALID, "INVALID_CHARACTER_LITERAL", line, column});
                } else {
                    std::string charValue = input.substr(start, i - start);
                    tokens.push_back({TOKEN_CHAR_LITERAL, charValue, line, column});
                }
            }
                break;

            case '=':
                if (i + 1 < input.size() && input[i + 1] == '=') {
                    tokens.push_back({TOKEN_EQUAL_EQUAL, "==", line, column});
                    i++;
                } else {
                    tokens.push_back({TOKEN_EQUAL, "=", line, column});
                }
                break;
            case '+':
                tokens.push_back({TOKEN_PLUS, "+", line, column});
                break;
            case '-':
                if (i + 1 < input.size() && input[i + 1] == '>') {
                    tokens.push_back({TOKEN_ARROW, "->", line, column});
                    i++;
                } else {
                    tokens.push_back({TOKEN_MINUS, "-", line, column});
                }
                break;
            case '*':
                tokens.push_back({TOKEN_ASTERISK, "*", line, column});
                break;
            case '/':
                if (i + 1 < input.size() && input[i + 1] == '/') {
                    i++; // Move to the next character after '//'
                    column++;
                    while (i < input.size() && input[i] != '\n') {
                        i++;
                        column++;
                    }
                } else if (i + 1 < input.size() && input[i + 1] == '*') {
                    i += 2;
                    column += 2;
                    while (i < input.size() && !(input[i] == '*' && i + 1 < input.size() && input[i + 1] == '/')) {
                        if (input[i] == '\n') {
                            line++;
                            column = 0;
                        }
                        i++;
                        column++;
                    }
                    if (i < input.size()) {
                        i++;  // skip the closing '/'
                    }
                } else {
                    tokens.push_back({TOKEN_SLASH, "/", line, column});
                }
                break;
            case '(':
                tokens.push_back({TOKEN_LPAREN, "(", line, column});
                break;
            case ')':
                tokens.push_back({TOKEN_RPAREN, ")", line, column});
                break;
            case ';':
                tokens.push_back({TOKEN_SEMI_COLON, ";", line, column});
                break;
            case '{':
                tokens.push_back({TOKEN_LBRACE, "{", line, column});
                break;
            case '}':
                tokens.push_back({TOKEN_RBRACE, "}", line, column});
                break;
            case ',':
                tokens.push_back({TOKEN_COMMA, ",", line, column});
                break;
            case ':':
                tokens.push_back({TOKEN_COLON, ":", line, column});
                break;
            case '!':
                if (i + 1 < input.size() && input[i + 1] == '=') {
                    tokens.push_back({TOKEN_NOT_EQUAL, "!=", line, column});
                    i++;
                } else {
                    tokens.push_back({TOKEN_EXCLAMATION_MARK, "!", line, column});
                }
                break;
            case '>':
                if (i + 1 < input.size() && input[i + 1] == '=') {
                    tokens.push_back({TOKEN_GREATER_EQUAL, ">=", line, column});
                    i++;
                } else {
                    tokens.push_back({TOKEN_GREATER, ">", line, column});
                }
                break;
            case '<':
                if (i + 1 < input.size() && input[i + 1] == '=') {
                    tokens.push_back({TOKEN_LESS_EQUAL, "<=", line, column});
                    i++;
                } else {
                    tokens.push_back({TOKEN_LESS, "<", line, column});
                }
                break;
            case '&':
                if (i + 1 < input.size() && input[i + 1] == '&') {
                    tokens.push_back({TOKEN_AND_AND, "&&", line, column});
                    i++;
                } else {
                    tokens.push_back({TOKEN_AMPERSAND, "&", line, column});
                }
                break;
            case '|':
                if (i + 1 < input.size() && input[i + 1] == '|') {
                    tokens.push_back({TOKEN_OR_OR, "||", line, column});
                    i++;
                }
                break;
            case '[':
                tokens.push_back({TOKEN_LSQUARE, "[", line, column});
                break;
            case ']':
                tokens.push_back({TOKEN_RSQUARE, "]", line, column});
                break;
            default:
                if (isalpha(input[i]) || input[i] == '_') {
                    size_t start = i;
                    while (i < input.size() && (isalnum(input[i]) || input[i] == '_')) {
                        i++;
                        column++;
                    }
                    std::string lexeme = input.substr(start, i - start);
                    auto it = keywordMap.find(lexeme);
                    if (it != keywordMap.end()) {
                        tokens.push_back({it->second, lexeme, line, column});
                    } else {
                        tokens.push_back({TOKEN_IDENTIFIER, lexeme, line, column});
                    }
                    i--;  // Adjust for the loop's increment
                } else if (isdigit(input[i])) {
                    size_t start = i;
                    while (i < input.size() && isdigit(input[i])) {
                        i++;
                        column++;
                    }
                    if (i < input.size() && input[i] == '.' && isdigit(input[i+1])) {
                        i++;
                        column++;
                        while (i < input.size() && isdigit(input[i])) {
                            i++;
                            column++;
                        }
                    }
                    std::string num = input.substr(start, i - start);
                    tokens.push_back({TOKEN_NUMBER, num, line, column});
                    i--;  // Adjust for the loop's increment
                } else {
                    tokens.push_back({TOKEN_INVALID, std::string(1, input[i]), line, column});
                }
                break;
        }
    }
    tokens.push_back({TOKEN_EOF, "", line, column});
    return tokens;
}

#endif //ASTRALANG_ASTRALEXER_H