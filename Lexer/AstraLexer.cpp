#include <string>
#include <vector>

// Define the different types of tokens that our lexer can recognize
enum TokenType {
    TOKEN_NUMBER,   // Number
    TOKEN_PLUS,     // '+'
    TOKEN_MINUS,    // '-'
    TOKEN_ASTERISK, // '*'
    TOKEN_SLASH,    // '/'
    TOKEN_LPAREN,   // '('
    TOKEN_RPAREN,   // ')'
    TOKEN_EOF,      // End of File or end of input string
    TOKEN_INVALID   // To indicate an unrecognized character
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
                // If current character is a digit, loop through the next characters until a non-digit is found to get the full number
                if (isdigit(input[i])) {
                    size_t start = i;
                    while (i < input.size() && isdigit(input[i])) {
                        i++;
                    }
                    tokens.push_back({TOKEN_NUMBER, input.substr(start, i - start)});
                    i--;  // Decrement the index since the outer for-loop will increment it
                } else if(isspace(input[i])) {
                    // If current character is a whitespace, just ignore and continue
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
