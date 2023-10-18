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
    TOKEN_PERCENT,   // '%'
    TOKEN_SLASH,    // '/'
    TOKEN_LPAREN,   // '('
    TOKEN_RPAREN,   // ')'
    TOKEN_CONST, // 'const'
    TOKEN_INT_TYPE, // 'int'
    TOKEN_SHORT_TYPE, // 'short'
    TOKEN_LONG_TYPE, // 'long'
    TOKEN_FLOAT_TYPE, // 'float'
    TOKEN_DOUBLE_TYPE, // 'double'
    TOKEN_CHAR_TYPE, // 'char'
    TOKEN_STRING_TYPE, // 'string'
    TOKEN_BOOL_TYPE, // 'bool'
    TOKEN_VOID_TYPE, // 'void'
    TOKEN_VAR, // 'var'
    TOKEN_IDENTIFIER, // Identifier
    TOKEN_SEMI_COLON, // ';'
    TOKEN_FUNC, // 'func'
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
    TOKEN_EXCLAMATION, // '!'
    TOKEN_AMPERSAND, // '&'
    TOKEN_EQUAL_EQUAL,   // '=='
    TOKEN_STRICT_EQUAL,   // '==='
    TOKEN_NOT_EQUAL,     // '!='
    TOKEN_STRICT_NOT_EQUAL, // '!=='
    TOKEN_GREATER,       // '>'
    TOKEN_LESS,          // '<'
    TOKEN_GREATER_EQUAL, // '>='
    TOKEN_LESS_EQUAL,    // '<='
    TOKEN_AND_AND,       // '&&'
    TOKEN_OR_OR,          // '||'
    TOKEN_CHAR_LITERAL, // 'a'
    TOKEN_STRING_LITERAL, // "hello world"
    TOKEN_VECTOR, // 'vector';
    TOKEN_MAP, // 'map';
    TOKEN_UNORDERED_MAP, // 'unordered_map';
    TOKEN_CLASS, // 'class'
    TOKEN_COLON, // ':'
    TOKEN_THIS, // 'this'
    TOKEN_LSQUARE, // '['
    TOKEN_RSQUARE, // ']'
    TOKEN_NEW, // 'new'
    TOKEN_DELETE, // 'delete'
    TOKEN_NULL, // 'null'
    TOKEN_DOT, // '.'
    TOKEN_PUBLIC, // 'public'
    TOKEN_PRIVATE, // 'private'
    TOKEN_PROTECTED, // 'protected'
    TOKEN_IMPORT, // 'import'
    TOKEN_AS, // 'as'
    TOKEN_EXTENDS, // 'extends'
    TOKEN_CONSTRUCTOR, // 'constructor'
    TOKEN_DESTRUCTOR, // 'destructor'
    TOKEN_MATH, // 'math'
    TOKEN_MATH_PI, // 'pi'
    TOKEN_MATH_SQRT, // 'sqrt'
    TOKEN_MATH_SIN, // 'sin'
    TOKEN_MATH_COS, // 'cos'
    TOKEN_MATH_TAN, // 'tan'
    TOKEN_MATH_POWER, // 'pow'
    TOKEN_ENUM, // 'enum'
    TOKEN_STATIC, // 'static'
    TOKEN_VIRTUAL, // 'virtual'
    TOKEN_OVERRIDE, // 'override'
    TOKEN_ABSTRACT, // 'abstract'
    TOKEN_NEWLINE, // '\n'
    TOKEN_IN, // 'in'
    TOKEN_PLUS_EQUAL, // '+='
    TOKEN_MINUS_EQUAL, // '-='
    TOKEN_ASTERISK_EQUAL, // '*='
    TOKEN_SLASH_EQUAL, // '/='
    TOKEN_PERCENT_EQUAL, // '%='
    TOKEN_PLUS_PLUS, // '++'
    TOKEN_MINUS_MINUS // '--'
};

// Keyword List
const std::unordered_map<std::string, TokenType> keywordMap = {
        {"const",    TOKEN_CONST},
        {"int",      TOKEN_INT_TYPE},
        {"short",    TOKEN_SHORT_TYPE},
        {"long",     TOKEN_LONG_TYPE},
        {"float",    TOKEN_FLOAT_TYPE},
        {"double",   TOKEN_DOUBLE_TYPE},
        {"char",     TOKEN_CHAR_TYPE},
        {"string",   TOKEN_STRING_TYPE},
        {"bool",     TOKEN_BOOL_TYPE},
        {"void",     TOKEN_VOID_TYPE},
        {"var",      TOKEN_VAR},
        {"func",       TOKEN_FUNC},
        {"return",   TOKEN_RETURN},
        {"if",       TOKEN_IF},
        {"else",     TOKEN_ELSE},
        {"while",    TOKEN_WHILE},
        {"for",      TOKEN_FOR},
        {"break",    TOKEN_BREAK},
        {"continue", TOKEN_CONTINUE},
        {"true",     TOKEN_TRUE},
        {"false",    TOKEN_FALSE},
        {"try",      TOKEN_TRY},
        {"catch", TOKEN_CATCH},
        {"finally", TOKEN_FINALLY},
        {"throw", TOKEN_THROW},
        {"print", TOKEN_PRINT},
        {"read", TOKEN_READ},
        {"vector", TOKEN_VECTOR},
        {"map", TOKEN_MAP},
        {"unordered_map", TOKEN_UNORDERED_MAP},
        {"class", TOKEN_CLASS},
        {"this", TOKEN_THIS},
        {"new", TOKEN_NEW},
        {"delete", TOKEN_DELETE},
        {"null", TOKEN_NULL},
        {"public", TOKEN_PUBLIC},
        {"private", TOKEN_PRIVATE},
        {"protected", TOKEN_PROTECTED},
        {"import", TOKEN_IMPORT},
        {"as", TOKEN_AS},
        {"extends", TOKEN_EXTENDS},
        {"constructor", TOKEN_CONSTRUCTOR},
        {"destructor", TOKEN_DESTRUCTOR},
        {"Math", TOKEN_MATH},
        {"pi", TOKEN_MATH_PI},
        {"sqrt", TOKEN_MATH_SQRT},
        {"sin", TOKEN_MATH_SIN},
        {"cos", TOKEN_MATH_COS},
        {"tan", TOKEN_MATH_TAN},
        {"pow", TOKEN_MATH_POWER},
        {"enum", TOKEN_ENUM},
        {"static", TOKEN_STATIC},
        {"virtual", TOKEN_VIRTUAL},
        {"override", TOKEN_OVERRIDE},
        {"abstract", TOKEN_ABSTRACT},
        {"in", TOKEN_IN}
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
    size_t column = 1;
    for (size_t i = 0; i < input.size(); i++) {
        size_t startColumn = column;
        column++;
        char ch = input[i];
        switch (ch) {
            case ' ':
            case '\t':
                column++;
                break;
            case '\n':
                tokens.push_back({TOKEN_NEWLINE, "\n", line, column});
                line++;
                column = 1;
                break;
            case '\r':
                break;
            case '.':
                tokens.push_back({TOKEN_DOT, ".", line, column});
                column++;
                break;
            case '"': {
                size_t start = i + 1;
                i++;
                column++;
                while (i < input.size() && input[i] != '"') {
                    if (input[i] == '\n') {
                        line++;
                        column = 1;
                    }
                    i++;
                }
                if (i == input.size()) {
                    tokens.push_back({TOKEN_INVALID, "UNTERMINATED_STRING", line, startColumn});
                } else {
                    std::string strValue = input.substr(start, i - start);
                    tokens.push_back({TOKEN_STRING_LITERAL, strValue, line, startColumn});
                    column += strValue.length() + 1; // +1 accounts for the ending quote
                }
                break;
            }
            case '\'': {
                size_t start = i + 1;
                i++;
                column++;
                while (i < input.size() && input[i] != '\'') {
                    column++;
                    if (input[i] == '\n') {
                        line++;
                        column = 1;
                    }
                    i++;
                }
                if (i == input.size() || i - start > 1) {
                    tokens.push_back({TOKEN_INVALID, "INVALID_CHARACTER_LITERAL", line, column});
                } else {
                    std::string charValue = input.substr(start, i - start);
                    tokens.push_back({TOKEN_CHAR_LITERAL, charValue, line, column});
                    column++;
                }
                break;
            }
            case '=':
                if (i + 2 < input.size() && input[i + 1] == '=' && input[i + 2] == '=') {
                    tokens.push_back({TOKEN_STRICT_EQUAL, "===", line, column});
                    i += 2;
                    column += 3;
                } else if (i + 1 < input.size() && input[i + 1] == '=') {
                    tokens.push_back({TOKEN_EQUAL_EQUAL, "==", line, column});
                    i++;
                    column += 2;
                } else {
                    tokens.push_back({TOKEN_EQUAL, "=", line, column});
                    column++;
                }
                break;
            case '+':
                if (i + 1 < input.size() && input[i + 1] == '=') {
                    tokens.push_back({TOKEN_PLUS_EQUAL, "+", line, column});
                    column += 2;
                    i++;
                    break;
                } else if (i + 1 < input.size() && input[i + 1] == '+') {
                    tokens.push_back({TOKEN_PLUS_PLUS, "++", line, column});
                    column += 2;
                    i++;
                    break;
                }
                tokens.push_back({TOKEN_PLUS, "+", line, column});
                column++;
                break;
            case '-':
                if (i + 1 < input.size() && input[i + 1] == '>') {
                    tokens.push_back({TOKEN_ARROW, "->", line, column});
                    i++;
                    column += 2;
                } else {
                    if (i + 1 < input.size() && input[i + 1] == '=') {
                        tokens.push_back({TOKEN_MINUS_EQUAL, "+", line, column});
                        column += 2;
                        i++;
                        break;
                    } else if (i + 1 < input.size() && input[i + 1] == '+') {
                        tokens.push_back({TOKEN_MINUS_MINUS, "++", line, column});
                        column += 2;
                        i++;
                        break;
                    }
                    tokens.push_back({TOKEN_MINUS, "-", line, column});
                    column++;
                }
                break;
            case '%':
                if (i + 1 < input.size() && input[i + 1] == '=') {
                    tokens.push_back({TOKEN_PERCENT_EQUAL, "+", line, column});
                    column += 2;
                    i++;
                    break;
                }
                tokens.push_back({TOKEN_PERCENT, "%", line, column});
                column++;
                break;
            case '*':
                if (i + 1 < input.size() && input[i + 1] == '=') {
                    tokens.push_back({TOKEN_ASTERISK_EQUAL, "+", line, column});
                    column += 2;
                    i++;
                    break;
                }
                tokens.push_back({TOKEN_ASTERISK, "*", line, column});
                column++;
                break;
            case '/':
                if (i + 1 < input.size() && input[i + 1] == '/') {
                    i++;
                    column += 2;
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
                            column = 1;
                        }
                        i++;
                        column++;
                    }
                    if (i < input.size()) {
                        i++;
                        column++;
                    }
                } else {
                    if (i + 1 < input.size() && input[i + 1] == '=') {
                        tokens.push_back({TOKEN_SLASH_EQUAL, "+", line, column});
                        column += 2;
                        i++;
                        break;
                    }
                    tokens.push_back({TOKEN_SLASH, "/", line, column});
                    column++;
                }
                break;
            case '(':
                tokens.push_back({TOKEN_LPAREN, "(", line, column});
                column++;
                break;
            case ')':
                tokens.push_back({TOKEN_RPAREN, ")", line, column});
                column++;
                break;
            case ';':
                tokens.push_back({TOKEN_SEMI_COLON, ";", line, column});
                column++;
                break;
            case '{':
                tokens.push_back({TOKEN_LBRACE, "{", line, column});
                column++;
                break;
            case '}':
                tokens.push_back({TOKEN_RBRACE, "}", line, column});
                column++;
                break;
            case ',':
                tokens.push_back({TOKEN_COMMA, ",", line, column});
                column++;
                break;
            case ':':
                tokens.push_back({TOKEN_COLON, ":", line, column});
                column++;
                break;
            case '!':
                if (i + 2 < input.size() && input[i + 1] == '=' && input[i + 2] == '=') {
                    tokens.push_back({TOKEN_STRICT_NOT_EQUAL, "!==", line, column});
                    i += 2;
                    column += 3;
                } else if (i + 1 < input.size() && input[i + 1] == '=') {
                    tokens.push_back({TOKEN_NOT_EQUAL, "!=", line, column});
                    i++;
                    column += 2;
                } else {
                    tokens.push_back({TOKEN_EXCLAMATION, "!", line, column});
                    column++;
                }
                break;
            case '>':
                if (i + 1 < input.size() && input[i + 1] == '=') {
                    tokens.push_back({TOKEN_GREATER_EQUAL, ">=", line, column});
                    i++;
                    column += 2;
                } else {
                    tokens.push_back({TOKEN_GREATER, ">", line, column});
                    column++;
                }
                break;
            case '<':
                if (i + 1 < input.size() && input[i + 1] == '=') {
                    tokens.push_back({TOKEN_LESS_EQUAL, "<=", line, column});
                    i++;
                    column += 2;
                } else {
                    tokens.push_back({TOKEN_LESS, "<", line, column});
                    column++;
                }
                break;
            case '&':
                if (i + 1 < input.size() && input[i + 1] == '&') {
                    tokens.push_back({TOKEN_AND_AND, "&&", line, column});
                    i++;
                    column += 2;
                } else {
                    tokens.push_back({TOKEN_AMPERSAND, "&", line, column});
                    column++;
                }
                break;
            case '|':
                if (i + 1 < input.size() && input[i + 1] == '|') {
                    tokens.push_back({TOKEN_OR_OR, "||", line, column});
                    i++;
                    column += 2;
                } else {
                    tokens.push_back({TOKEN_INVALID, "|", line, column});
                    column++;
                }
                break;
            case '[':
                tokens.push_back({TOKEN_LSQUARE, "[", line, column});
                column++;
                break;
            case ']':
                tokens.push_back({TOKEN_RSQUARE, "]", line, column});
                column++;
                break;
            default:
                if (isalpha(input[i]) || input[i] == '_') {
                    size_t start = i;
                    while (i < input.size() && (isalnum(input[i]) || input[i] == '_')) {
                        i++;
                    }
                    std::string lexeme = input.substr(start, i - start);
                    auto it = keywordMap.find(lexeme);
                    if (it != keywordMap.end()) {
                        tokens.push_back({it->second, lexeme, line, startColumn});
                    } else {
                        tokens.push_back({TOKEN_IDENTIFIER, lexeme, line, startColumn});
                    }
                    column += lexeme.size() - 1;
                    i--;
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
                    tokens.push_back({TOKEN_NUMBER, num, line, startColumn});
                    column += num.size() - 1;
                    i--;
                } else {
                    tokens.push_back({TOKEN_INVALID, std::string(1, input[i]), line, startColumn});
                }
                break;
        }
    }
    tokens.push_back({TOKEN_EOF, "", line, column});
    return tokens;
}


#endif //ASTRALANG_ASTRALEXER_H