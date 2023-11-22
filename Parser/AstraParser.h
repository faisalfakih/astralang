//
// Created by Faisal Fakih on 18/10/2023.
//

#ifndef ASTRALANG_ASTRAPARSER_H
#define ASTRALANG_ASTRAPARSER_H

#include "../Lexer/AstraLexer.h"
#include "../AST/AstraAST.h"
#include <string>
#include <memory>
#include <variant>
#include <vector>
#include <map>
#include <fstream>
#include <iostream>
#include <sstream>

class Parser {
private:
    Scope* currentScope;

    std::vector<Token> tokens;
    int currentTokenIndex = 0;

    Token& currentToken() {
        return tokens[currentTokenIndex];
    }

    Token& nextToken() {
        return tokens[++currentTokenIndex];
    }

    Token& peek(int distance = 0) {
        return tokens[currentTokenIndex + distance];
    }

    void consumeToken() {
        if (!isAtEnd()) {
            currentTokenIndex++;
        }
    }

    bool isAtEnd() const {
        return currentTokenIndex >= tokens.size();
    }

    bool match(TokenType type) {
        if (currentToken().type == type) {
            nextToken();
            return true;
        }
        return false;
    }

    void expect(TokenType type) {
        if (currentToken().type != type) {
            throw std::runtime_error("Unexpected token: " + currentToken().lexeme + " at line: " + std::to_string(currentToken().line)  + ". Was expecting " + std::to_string(type));
        }
        nextToken();
    }

    bool checkTokenType(TokenType type) {
        return peek().type == type;
    }


    std::unique_ptr<TypeRepresentation> parseType() {
        std::map<TokenType, BasicType::Type> typeMap = {
                { TokenType::TOKEN_INT_TYPE,    BasicType::INT },
                { TokenType::TOKEN_SHORT_TYPE,  BasicType::SHORT },
                { TokenType::TOKEN_LONG_TYPE,   BasicType::LONG },
                { TokenType::TOKEN_FLOAT_TYPE,  BasicType::FLOAT },
                { TokenType::TOKEN_DOUBLE_TYPE, BasicType::DOUBLE },
                { TokenType::TOKEN_CHAR_TYPE,   BasicType::CHAR },
                { TokenType::TOKEN_STRING_TYPE, BasicType::STRING },
                { TokenType::TOKEN_BOOL_TYPE,   BasicType::BOOL },
                { TokenType::TOKEN_VOID_TYPE,   BasicType::VOID },
                { TokenType::TOKEN_LET,         BasicType::VAR },
        };

        for (const auto& [tokenType, basicType] : typeMap) {
            if (match(tokenType) || match(TokenType::TOKEN_VECTOR) || match(TokenType::TOKEN_UNORDERED_MAP) || match(TokenType::TOKEN_MAP)) {
                if (match(TokenType::TOKEN_ASTERISK)) {
                    return std::make_unique<PointerType>(std::make_unique<BasicType>(basicType));
                } else if (match(TokenType::TOKEN_AMPERSAND)) {
                    return std::make_unique<ReferenceType>(std::make_unique<BasicType>(basicType));
                } else if (match(TokenType::TOKEN_LSQUARE)) {
                    size_t size = std::stoul(currentToken().lexeme);
                    consumeToken();
                    expect(TokenType::TOKEN_RSQUARE);
                    return std::make_unique<ArrayType>(std::make_unique<BasicType>(basicType), size);
                } else if (match(TokenType::TOKEN_LESS)) {
                    if (peek(-2).type == TokenType::TOKEN_VECTOR) {
                        std::unique_ptr<TypeRepresentation> vectorType = parseType();
                        expect(TokenType::TOKEN_GREATER);
                        return std::make_unique<VectorType>(std::make_unique<BasicType>(basicType), 0);
                    } else if (peek(-2).type == TokenType::TOKEN_MAP || peek(-2).type == TokenType::TOKEN_UNORDERED_MAP) {
                        bool ordered = peek(-2).type == TokenType::TOKEN_MAP; // True if it's an ordered map
                        std::unique_ptr<TypeRepresentation> keyType = parseType();
                        expect(TokenType::TOKEN_COMMA);
                        std::unique_ptr<TypeRepresentation> valueType = parseType();
                        expect(TokenType::TOKEN_GREATER);

                        if (ordered) return std::make_unique<MapType>(std::make_unique<BasicType>(basicType), std::make_unique<BasicType>(basicType));
                        else return std::make_unique<UnorderedMapType>(std::make_unique<BasicType>(basicType), std::make_unique<BasicType>(basicType));
                    }
                }

                return std::make_unique<BasicType>(basicType);
            } else if (checkTokenType(TokenType::TOKEN_IDENTIFIER)) {
                consumeToken();
                if (match(TokenType::TOKEN_ASTERISK)) {
                    return std::make_unique<PointerType>(std::make_unique<BasicType>(basicType));
                } else if (match(TokenType::TOKEN_AMPERSAND)) {
                    return std::make_unique<ReferenceType>(std::make_unique<BasicType>(basicType));
                }
                return std::make_unique<BasicType>(basicType);
            }
        }

        return nullptr; // No type matched
    }

    // Parse Parameters
    std::unique_ptr<Parameter> parseParameter() {
        std::unique_ptr<TypeRepresentation> paramType = parseType();
        if (paramType == nullptr) {
            throw std::runtime_error("Expected a type for parameter. Line: " + std::to_string(currentToken().line) + ".");
        }

        expect(TokenType::TOKEN_IDENTIFIER);
        std::string paramName = currentToken().lexeme;

        return std::make_unique<Parameter>(paramName, std::move(paramType));
    }

    // Parse Parameters
    std::vector<std::unique_ptr<Parameter>> parseParameters() {
        expect(TokenType::TOKEN_LPAREN); // Expect an opening parenthesis

        std::vector<std::unique_ptr<Parameter>> params;

        if (!checkTokenType(TokenType::TOKEN_RPAREN)) {
            do {
                params.push_back(parseParameter()); // Parse the parameter
            } while (match(TokenType::TOKEN_COMMA));
        }

        expect(TokenType::TOKEN_RPAREN); // Expect a closing parenthesis

        return params;
    }

    // Parse Expressions
    std::unique_ptr<Expression> parseExpression() {
        return parseLogicalExpression();
    }

    std::unique_ptr<Expression> parseLogicalExpression() {
        std::unique_ptr<Expression> left = parseComparison();
        while (checkTokenType(TokenType::TOKEN_AND_AND) || checkTokenType(TokenType::TOKEN_OR_OR)) {
            TokenType op = currentToken().type;
            consumeToken();
            std::unique_ptr<Expression> right = parseComparison();
            left = std::make_unique<LogicalExpression>(LogicalExpression::Operator(op), std::move(left), std::move(right));
        }
        return left;
    }

    std::unique_ptr<Expression> parseComparison() {
        auto left = parseAdditionSubtraction();
        while (checkTokenType(TokenType::TOKEN_EQUAL_EQUAL) || checkTokenType(TokenType::TOKEN_NOT_EQUAL) ||
               checkTokenType(TokenType::TOKEN_LESS) || checkTokenType(TokenType::TOKEN_LESS_EQUAL) ||
               checkTokenType(TokenType::TOKEN_GREATER) || checkTokenType(TokenType::TOKEN_GREATER_EQUAL) ||
               checkTokenType(TokenType::TOKEN_STRICT_EQUAL) || checkTokenType(TokenType::TOKEN_STRICT_NOT_EQUAL)) {
            TokenType op = currentToken().type;
            consumeToken();
            auto right = parseAdditionSubtraction();
            left = std::make_unique<ComparisonExpression>(ComparisonExpression::Operator(op), std::move(left), std::move(right));
        }
        return left;
    }

    std::unique_ptr<Expression> parseAdditionSubtraction() {
        auto left = parseMultiplicationDivision();
        while (checkTokenType(TokenType::TOKEN_PLUS) || checkTokenType(TokenType::TOKEN_MINUS)) {
            TokenType op = currentToken().type;
            consumeToken();
            auto right = parseMultiplicationDivision();
            left = std::make_unique<BinaryExpression>(BinaryExpression::Operator(op), std::move(left), std::move(right));
        }
        return left;
    }

    std::unique_ptr<Expression> parseMultiplicationDivision() {
        auto left = parseUnaryExpression();
        while (checkTokenType(TokenType::TOKEN_ASTERISK) || checkTokenType(TokenType::TOKEN_SLASH) || match(TokenType::TOKEN_PERCENT)) {
            TokenType op = currentToken().type;
            consumeToken();
            auto right = parseUnaryExpression();
            left = std::make_unique<BinaryExpression>(BinaryExpression::Operator(op), std::move(left), std::move(right));
        }
        return left;
    }


    std::unique_ptr<Expression> parseUnaryExpression() {
        if (match(TokenType::TOKEN_EXCLAMATION)) {
            auto operand = parsePrimaryExpression();
            return std::make_unique<UnaryExpression>(UnaryExpression::Operator::NOT, std::move(operand));
        }
        if (match(TokenType::TOKEN_AMPERSAND)) {
            auto operand = parsePrimaryExpression();
            return std::make_unique<UnaryExpression>(UnaryExpression::Operator::REFERENCE, std::move(operand));
        }
        if (match(TokenType::TOKEN_ASTERISK)) {
            auto operand = parsePrimaryExpression();
            return std::make_unique<UnaryExpression>(UnaryExpression::Operator::DEREFERENCE, std::move(operand));
        }
        // If it's not a unary operator, parse as primary expression (e.g., literals, identifiers, etc.)
        return parsePrimaryExpression();
    }

    std::unique_ptr<Expression> parsePrimaryExpression() {
        if (peek().type == TokenType::TOKEN_READ) {
            return parseReadExpression();
        }
        if (peek().type == TokenType::TOKEN_NUMBER) {
            consumeToken();
            try {
                return std::make_unique<NumberLiteral>(std::stod(peek(-1).lexeme));
            } catch (const std::invalid_argument& e) {
                throw std::runtime_error("Invalid number format at line " + std::to_string(currentToken().line) + ", column " + std::to_string(currentToken().column) + " at token " + peek(-1).lexeme);
            }
        }
        if (peek().type == TokenType::TOKEN_CHAR_LITERAL) {
            consumeToken();
            if (currentToken().lexeme.length() == 1) {
                return std::make_unique<CharLiteral>(peek(-1).lexeme[0]);
            } else {
                throw std::runtime_error("Expected expression at line " + std::to_string(currentToken().line) + ", column " + std::to_string(currentToken().column));
            }
        }
        if (peek().type == TokenType::TOKEN_STRING_LITERAL) {
            consumeToken();
            return std::make_unique<StringLiteral>(peek(-1).lexeme);
        }
        if (peek().type == TokenType::TOKEN_TRUE || peek().type == TokenType::TOKEN_FALSE) {
            consumeToken();
            bool value = currentToken().type == TokenType::TOKEN_TRUE;
            return std::make_unique<BooleanLiteral>(value);
        }
        if (peek().type == TokenType::TOKEN_IDENTIFIER) {
            consumeToken();
            return std::make_unique<Identifier>(peek(-1).lexeme);
        }
        if (peek().type == TokenType::TOKEN_LPAREN) {
            consumeToken();
            auto expr = parseExpression();
            expect(TokenType::TOKEN_RPAREN);
            return expr;
        }
        throw std::runtime_error("Expected expression at line " + std::to_string(currentToken().line) + ", column " + std::to_string(currentToken().column) + " at token " + peek().lexeme);
    }

    // Parse Statement
    std::unique_ptr<Statement> parseStatement() {
        switch (currentToken().type) {
            case TokenType::TOKEN_IF:
                return parseIfStatement();
            case TokenType::TOKEN_WHILE:
                return parseWhileLoop();
            case TokenType::TOKEN_FOR:
                return parseForLoop();
            case TokenType::TOKEN_PRINT:
                return parsePrintStatement();
            case TokenType::TOKEN_IMPORT:
            {
                Statement* stmtPtr = dynamic_cast<Statement*>(parseImportStatement().release());
                if(!stmtPtr) {
                    throw std::runtime_error("Unexpected error at line " + std::to_string(currentToken().line) + " column " + std::to_string(currentToken().column));
                }
                std::unique_ptr<Statement> import(stmtPtr);
                break;
            }
            case TokenType::TOKEN_NEWLINE:
                consumeToken();  // consume newline if it's there
                break;
            case TokenType::TOKEN_INT_TYPE:
            case TokenType::TOKEN_SHORT_TYPE:
            case TokenType::TOKEN_LONG_TYPE:
            case TokenType::TOKEN_FLOAT_TYPE:
            case TokenType::TOKEN_DOUBLE_TYPE:
            case TokenType::TOKEN_CHAR_TYPE:
            case TokenType::TOKEN_STRING_TYPE:
            case TokenType::TOKEN_BOOL_TYPE:
            case TokenType::TOKEN_VOID_TYPE:
            case TokenType::TOKEN_LET:
            case TokenType::TOKEN_CONST:
                parseVariableDeclaration();
                break;
            case TokenType::TOKEN_IDENTIFIER:
                if (peek(1).type == TokenType::TOKEN_IDENTIFIER)
                    parseVariableDeclaration();
                parseVariableReassignment();
                break;
            case TokenType::TOKEN_RETURN:
                return parseReturnStatement();
            case TokenType::TOKEN_BREAK:
                return parseBreakStatement();
            case TokenType::TOKEN_CONTINUE:
                return parseContinueStatement();
            default:
                throw std::runtime_error("Unexpected token at line " + std::to_string(currentToken().line) + " column " + std::to_string(currentToken().column) + " at token " + peek().lexeme);
        }
        return nullptr;
    }


    std::unique_ptr<BlockStatement> parseBlockStatement() {
        consumeToken();  // Consume the opening brace TOKEN_LBRACE
        std::vector<std::unique_ptr<Statement>> statements;

        consumeOptionalNewlines();  // Consume newlines if they exist

        while (!checkTokenType(TokenType::TOKEN_RBRACE) && !checkTokenType(TokenType::TOKEN_EOF)) {
            statements.push_back(parseStatement());
            consumeOptionalNewlines();  // Consume newlines between statements
        }

        expect(TokenType::TOKEN_RBRACE);  // Ensure the closing brace TOKEN_RBRACE is present and consume it
        return std::make_unique<BlockStatement>(std::move(statements));
    }

    // Skip optional new line
    void consumeOptionalNewlines() {
        while (checkTokenType(TokenType::TOKEN_NEWLINE)) {
            consumeToken();
        }
    }

    // If Statements
    std::unique_ptr<BlockStatement> parseIfStatementBody()  {
        return parseBlockStatement();
    }

    std::unique_ptr<IfStatement> parseIfStatement() {
        consumeToken(); // consume 'if'
        std::unique_ptr<Expression> condition = parseExpression();

        consumeOptionalNewlines();  // Consume newlines if they exist before the block

        enterNewScope();  // Enter scope for true branch
        std::unique_ptr<Statement> trueBranch = parseIfStatementBody();  // Adjusted for either block or single statement
        exitCurrentScope();  // Exit scope for true branch

        consumeOptionalNewlines();  // Add this line to consume any newlines after the 'if' body

        std::unique_ptr<Statement> falseBranch = nullptr;
        if (checkTokenType(TokenType::TOKEN_ELSE)) {
            consumeToken();  // consume 'else'
            consumeOptionalNewlines();  // Consume newlines if they exist before the block

            // Check for "else if" condition
            if (checkTokenType(TokenType::TOKEN_IF)) {
                falseBranch = parseIfStatement();  // Recursively parse the else-if part
            } else {
                enterNewScope();  // Enter scope for false branch
                falseBranch = parseIfStatementBody();  // Adjusted for either block or single statement
                exitCurrentScope();  // Exit scope for false branch
            }
        }

        return std::make_unique<IfStatement>(std::move(condition), std::move(trueBranch), std::move(falseBranch));
    }



    // While Loop
    std::unique_ptr<Statement> parseWhileLoopBody() {
        if (checkTokenType(TokenType::TOKEN_LBRACE)) {
            return parseBlockStatement();
        }
        return parseStatement();
    }

    std::unique_ptr<WhileLoop> parseWhileLoop() {
        consumeToken();  // consume 'while'
        std::unique_ptr<Expression> condition = parseExpression();

        consumeOptionalNewlines();  // Consume newlines if they exist before the block

        enterNewScope();  // Enter scope for loop body
        std::unique_ptr<Statement> body = parseWhileLoopBody();  // Adjusted for either block or single statement
        exitCurrentScope();  // Exit scope for loop body

        return std::make_unique<WhileLoop>(std::move(condition), std::move(body));
    }

    // For Loop
    std::unique_ptr<Statement> parseForLoop() {
        expect(TokenType::TOKEN_FOR);
        std::unique_ptr<TypeRepresentation> type = parseType();
        if (!type) {
            throw std::runtime_error("Expected a type for the loop variable at line " + std::to_string(currentToken().line));
        }
        if (!match(TokenType::TOKEN_IDENTIFIER)) {
            throw std::runtime_error("Expected a variable name at line " + std::to_string(currentToken().line));
        }
        std::string varName;
        varName = currentToken().lexeme;

        if (match(TokenType::TOKEN_IN)) {
            if (checkTokenType(TokenType::TOKEN_NUMBER) || checkTokenType(TokenType::TOKEN_IDENTIFIER)) {
                std::unique_ptr<Expression> expr = parseExpression();

                // If it's a range-based loop
                if (match(TokenType::TOKEN_ARROW)) {
                    std::unique_ptr<Expression> endExpr = parseExpression();

                    consumeOptionalNewlines();  // Consume newlines if they exist before the block

                    enterNewScope();  // Enter scope for loop body
                    std::unique_ptr<Statement> body = parseBlockStatement();
                    exitCurrentScope();  // Exit the scope for loop body

                    return std::make_unique<ForLoop>(std::move(type), std::move(varName),
                                                     std::move(expr), std::move(endExpr),
                                                     std::move(body));
                }
                    // If it's an iterator-based loop
                else {
                    consumeOptionalNewlines();  // Consume newlines if they exist before the block

                    enterNewScope();  // Enter scope for loop body
                    std::unique_ptr<Statement> body = parseBlockStatement();
                    exitCurrentScope();  // Exit the scope for loop body

                    return std::make_unique<ForEachLoop>(std::move(type), std::move(varName),
                                                         std::move(expr), std::move(body));
                }
            } else {
                throw std::runtime_error("Expected a number or identifier after 'in' at line " + std::to_string(currentToken().line));
            }
        } else {
            throw std::runtime_error("Expected 'in' at line " + std::to_string(currentToken().line));
        }
    }

    std::unique_ptr<Statement> parsePrintStatement() {
        expect(TokenType::TOKEN_PRINT);
        expect(TokenType::TOKEN_LPAREN);

        std::unique_ptr<Expression> expression = parsePrimaryExpression();

        expect(TokenType::TOKEN_RPAREN);
        expect(TokenType::TOKEN_SEMI_COLON);

        return std::make_unique<PrintStatement>(std::move(expression));
    }


    // Parse Import Statement
    std::unique_ptr<Import> parseImportStatement() {
        expect(TokenType::TOKEN_IMPORT); // Checks for the import keyword

        if (currentToken().type != TokenType::TOKEN_IDENTIFIER) {
            throw std::runtime_error("Expected a module name after 'import' at line " + std::to_string(currentToken().line) + ".");
        }

        std::string moduleName = currentToken().lexeme;
        consumeToken(); // Move to the next token after capturing the moduleName

        std::string alias;
        if (match(TokenType::TOKEN_AS)) {
            if (currentToken().type != TokenType::TOKEN_IDENTIFIER) {
                throw std::runtime_error("Expected an alias after 'as' at line " + std::to_string(currentToken().line) + ".");
            }
            alias = currentToken().lexeme;
            consumeToken(); // Move to the next token after capturing the alias
        }

        expect(TokenType::TOKEN_SEMI_COLON);

        return std::make_unique<Import>(moduleName, alias.empty() ? moduleName : alias);
    }

    // Parse Class Methods
    std::unique_ptr<FunctionDeclaration> parseMethodDeclaration() {
        parseFunctionDeclaration();
    }


    // Parse Function Declaration
    std::unique_ptr<FunctionDeclaration> parseFunctionDeclaration() {
        expect(TokenType::TOKEN_FUNC);
        if (currentToken().type != TokenType::TOKEN_IDENTIFIER) {
            throw std::runtime_error("Expected a currentFunction name after 'func' at line " + std::to_string(currentToken().line) + ".");
        }
        FunctionDeclaration::Kind kind = FunctionDeclaration::Kind::REGULAR;

        std::string functionName = currentToken().lexeme;

        consumeToken();

        std::vector<std::unique_ptr<Parameter>> params = parseParameters();

        // Check if the currentFunction returns a type
        std::unique_ptr<TypeRepresentation> returnType = nullptr;
        if (checkTokenType(TokenType::TOKEN_ARROW)) {
            consumeToken();
            returnType = parseType();
            if (!returnType) {
                throw std::runtime_error("Expected return type after '->' at line " + std::to_string(currentToken().line));
            }
        }

        std::unique_ptr<BlockStatement> body;
        // Code block
        if (checkTokenType(TokenType::TOKEN_LBRACE)) {
            body = parseBlockStatement();
        } else {
            throw std::runtime_error("Expected open bracket at line " + std::to_string(currentToken().line));
        }

        return std::make_unique<FunctionDeclaration>(functionName, std::move(params), std::move(returnType), std::move(body), kind);
    }

    // Parse Variable Declarations
    bool isTypeName(TokenType tokenType) {
        switch (tokenType) {
            case TokenType::TOKEN_INT_TYPE:
            case TokenType::TOKEN_SHORT_TYPE:
            case TokenType::TOKEN_LONG_TYPE:
            case TokenType::TOKEN_FLOAT_TYPE:
            case TokenType::TOKEN_DOUBLE_TYPE:
            case TokenType::TOKEN_CHAR_TYPE:
            case TokenType::TOKEN_STRING_TYPE:
            case TokenType::TOKEN_BOOL_TYPE:
            case TokenType::TOKEN_VOID_TYPE:
            case TokenType::TOKEN_LET:
            case TokenType::TOKEN_VECTOR:
            case TokenType::TOKEN_UNORDERED_MAP:
            case TokenType::TOKEN_MAP:
                return true;
            default:
                return false;
        }
    }


    std::unique_ptr<Expression> parseReadExpression() {
        expect(TokenType::TOKEN_READ);
        expect(TokenType::TOKEN_LPAREN);
        expect(TokenType::TOKEN_RPAREN);

        // Create a new ReadExpression instance.
        return std::make_unique<ReadStatement>();
    }

    std::unique_ptr<VariableCall> parseVariableCall() {
        std::string varName = currentToken().lexeme;
        consumeToken();  // Consume the identifier token
        expect(TokenType::TOKEN_SEMI_COLON);
        return std::make_unique<VariableCall>(varName);
    }

    std::unique_ptr<VariableCall> parseVariableCallWithinParameter() {
        std::string varName = currentToken().lexeme;
        consumeToken();  // Consume the identifier token
        return std::make_unique<VariableCall>(varName);
    }

    std::unique_ptr<VariableDeclaration> parseVariableDeclaration(bool withinParameter = false) {
        std::unique_ptr<TypeRepresentation> type;
        bool isConst = false;
        size_t lifetime = 1;

        if (checkTokenType(TokenType::TOKEN_CONST)) {
            isConst = true;
            consumeToken();
        }

        // Check if there is a lifetime (1 by default meaning that if the variable goes out of scope it will be deleted)
        if (checkTokenType(TokenType::TOKEN_NUMBER)) {
            lifetime = std::stoll(currentToken().lexeme);
            expect(TokenType::TOKEN_DOLLAR_SIGN);
        }

        // First, try to parse known types.
        if (isTypeName(currentToken().type)) {
            type = parseType();
        } else if (checkTokenType(TokenType::TOKEN_IDENTIFIER)) {
            // The identifier itself is the custom type name.
            type = std::make_unique<CustomType>(currentToken().lexeme);
            consumeToken();  // Move past the custom type name.

            // Now, check the next token for the variable name.
            if (!checkTokenType(TokenType::TOKEN_IDENTIFIER)) {
                throw std::runtime_error("Expected variable name after type. Line: " + std::to_string(currentToken().line) + ", Column: " + std::to_string(currentToken().column));
            }
        } else {
            throw std::runtime_error("Expected a type for variable declaration. Line: " + std::to_string(currentToken().line) + ", Column: " + std::to_string(currentToken().column));
        }

        size_t size = 0;
        std::string varName;

        varName = currentToken().lexeme;  // This is now the variable name.
        consumeToken();  // Move past the variable name.


        std::unique_ptr<Expression> initValue = nullptr;
        if (match(TokenType::TOKEN_EQUAL)) {
            initValue = parseExpression();
        }

        if (!withinParameter) {
            expect(TokenType::TOKEN_SEMI_COLON); // Expect a semicolon only if it's not a parameter
        }


        return std::make_unique<VariableDeclaration>(std::move(type), varName, std::move(initValue), isConst, lifetime, size);
    }

    std::unique_ptr<VariableDeclaration> parseVariableDeclarationWithinParameter() {
        parseVariableDeclaration(true);
    }

    // Parse Variable Declaration or Reassignment
    std::unique_ptr<Statement> parseVariableDeclarationOrCall() {
        if (checkTokenType(TokenType::TOKEN_IDENTIFIER) || isTypeName(currentToken().type)) {
            // Peek ahead to check if the next token type suggests a variable declaration.
            if (peek(1).type == TokenType::TOKEN_IDENTIFIER) {
                // Parse it as a variable declaration.
                return parseVariableDeclarationWithinParameter();
            } else {
                // Assume it's a variable call.
                return parseVariableCallWithinParameter();
            }
        } else {
            throw std::runtime_error("Expected identifier at line " + std::to_string(currentToken().line));
        }
    }


    std::unique_ptr<FunctionDeclaration> parseConstructorOrDestructor() {
        TokenType tokenType = currentToken().type;
        if (tokenType != TokenType::TOKEN_CONSTRUCTOR && tokenType != TokenType::TOKEN_DESTRUCTOR) {
            throw std::runtime_error("Expected 'constructor' or 'destructor' at line " + std::to_string(currentToken().line) + ".");
        }
        consumeToken();  // consume 'constructor' or 'destructor'

        FunctionDeclaration::Kind kind = (tokenType == TokenType::TOKEN_CONSTRUCTOR) ?
                                         FunctionDeclaration::Kind::CONSTRUCTOR :
                                         FunctionDeclaration::Kind::DESTRUCTOR;

        std::vector<std::unique_ptr<Parameter>> params;
        if (kind == FunctionDeclaration::Kind::CONSTRUCTOR) {
            params = parseParameters();
        } else if (currentToken().type != TokenType::TOKEN_LBRACE) {
            throw std::runtime_error("Expected '{' at line " + std::to_string(currentToken().line));
        }
        // For constructors and destructors, no return type is expected
        std::unique_ptr<TypeRepresentation> returnType = nullptr;

        std::unique_ptr<BlockStatement> body;
        if (checkTokenType(TokenType::TOKEN_LBRACE)) {
            body = parseBlockStatement();
        } else {
            throw std::runtime_error("Expected '{' at line " + std::to_string(currentToken().line));
        }

        return std::make_unique<FunctionDeclaration>("", std::move(params), std::move(returnType), std::move(body), kind);
    } // TODO: ADD FUNCTIONALLITY FOR KEYWORD `this`


    // Parse Class Declarations
    std::unique_ptr<ClassDeclaration> parseClassDeclaration() { // TODO: ADD THIS
        expect(TokenType::TOKEN_CLASS);
        if (!checkTokenType(TokenType::TOKEN_IDENTIFIER)) {
            throw std::runtime_error("Expected a class name after 'class' at line " + std::to_string(currentToken().line) + ".");
        }

        enum AccessSpecifier {
            PRIVATE,
            PUBLIC,
            PROTECTED
        };

        AccessSpecifier accessSpecifier = AccessSpecifier::PUBLIC;

        std::string className = currentToken().lexeme;
        consumeToken();

        std::string baseClassName;
        if (checkTokenType(TokenType::TOKEN_EXTENDS)) {
            consumeToken();
            if (!checkTokenType(TokenType::TOKEN_IDENTIFIER)) {
                throw std::runtime_error("Expected a base class name after 'extends' at line " + std::to_string(currentToken().line) + ".");
            }
            baseClassName = currentToken().lexeme;
            consumeToken();
        }


        std::vector<std::unique_ptr<MemberVariable>> privateMembers;
        std::vector<std::unique_ptr<MemberVariable>> publicMembers;
        std::vector<std::unique_ptr<MemberVariable>> protectedMembers;
        std::vector<std::unique_ptr<FunctionDeclaration>> privateMethods;
        std::vector<std::unique_ptr<FunctionDeclaration>> publicMethods;
        std::vector<std::unique_ptr<FunctionDeclaration>> protectedMethods;

        expect(TokenType::TOKEN_LBRACE);

        std::unique_ptr<FunctionDeclaration> constructor = nullptr;
        std::unique_ptr<FunctionDeclaration> destructor = nullptr;

        while (!checkTokenType(TokenType::TOKEN_RBRACE) && !isAtEnd()) {  // Continue until '}' or end of input
            if (checkTokenType(TokenType::TOKEN_PRIVATE)) {
                consumeToken();
                expect(TokenType::TOKEN_COLON);
                accessSpecifier = AccessSpecifier::PRIVATE;
            } else if (checkTokenType(TokenType::TOKEN_PUBLIC)) {
                consumeToken();
                expect(TokenType::TOKEN_COLON);
                accessSpecifier = AccessSpecifier::PUBLIC;
                consumeToken();
            } else if (checkTokenType(TokenType::TOKEN_PROTECTED)) {
                consumeToken();
                expect(TokenType::TOKEN_COLON);
                accessSpecifier = AccessSpecifier::PROTECTED;
                consumeToken();
            } else if (checkTokenType(TokenType::TOKEN_FUNC)) {
                std::unique_ptr<FunctionDeclaration> method = parseMethodDeclaration();
                switch (accessSpecifier) {
                    case AccessSpecifier::PRIVATE:
                        privateMethods.push_back(std::move(method));
                        break;
                    case AccessSpecifier::PUBLIC:
                        publicMethods.push_back(std::move(method));
                        break;
                    case AccessSpecifier::PROTECTED:
                        protectedMethods.push_back(std::move(method));
                        break;
                }
            } else if (checkTokenType(TokenType::TOKEN_CONSTRUCTOR)) {
                constructor = parseConstructorOrDestructor();
            } else if (checkTokenType(TokenType::TOKEN_DESTRUCTOR)) {
                destructor = parseConstructorOrDestructor();
            } else {
                parseStatement();
            }
        }

        expect(TokenType::TOKEN_RBRACE);  // Consume the closing '}'
        expect(TokenType::TOKEN_SEMI_COLON);  // Consume the semicolon

        return std::make_unique<ClassDeclaration>(
                className, baseClassName, std::move(privateMembers), std::move(publicMembers),
                std::move(protectedMembers), std::move(privateMethods), std::move(publicMethods),
                std::move(protectedMethods), std::move(constructor), std::move(destructor)
        );
    }

    // Parse Function Call
    std::unique_ptr<FunctionCall> parseFunctionCall() {
        if (currentToken().type != TokenType::TOKEN_IDENTIFIER) {
            throw std::runtime_error("Expected a currentFunction name at line " + std::to_string(currentToken().line) + ".");
        }

        std::string functionName = currentToken().lexeme;
        consumeToken();

        // Check if the next token is an opening parenthesis
        if (currentToken().type != TokenType::TOKEN_LPAREN) {
            throw std::runtime_error("Expected '(' after currentFunction name at line " + std::to_string(currentToken().line) + ".");
        }
        consumeToken(); // consume '('

        std::vector<std::unique_ptr<Expression>> arguments;
        // Check if there are any arguments
        if (currentToken().type != TokenType::TOKEN_RPAREN) {
            do {
                arguments.push_back(parseExpression());

                // Check for more arguments
                if (currentToken().type != TokenType::TOKEN_COMMA) {
                    break;
                }
                consumeToken(); // consume ','
            } while (true);
        }

        // Check if the argument list is closed correctly
        if (currentToken().type != TokenType::TOKEN_RPAREN) {
            throw std::runtime_error("Expected ')' after arguments at line " + std::to_string(currentToken().line) + ".");
        }
        consumeToken(); // consume ')'

        return std::make_unique<FunctionCall>(functionName, std::move(arguments));
    }

    std::unique_ptr<ReturnStatement> parseReturnStatement() {
        expect(TokenType::TOKEN_RETURN);
        std::unique_ptr<Expression> returnValue = nullptr;
        if (!checkTokenType(TokenType::TOKEN_SEMI_COLON)) {
            returnValue = parseExpression();
        }
        expect(TokenType::TOKEN_SEMI_COLON);
        return std::make_unique<ReturnStatement>(std::move(returnValue));
    }

    std::unique_ptr<BreakStatement> parseBreakStatement() {
        expect(TokenType::TOKEN_BREAK);
        expect(TokenType::TOKEN_SEMI_COLON);
        return std::make_unique<BreakStatement>();
    }

    std::unique_ptr<ContinueStatement> parseContinueStatement() {
        expect(TokenType::TOKEN_CONTINUE);
        expect(TokenType::TOKEN_SEMI_COLON);
        return std::make_unique<ContinueStatement>();
    }

    // Reassigning Variables
    std::unique_ptr<AssignmentExpression> parseVariableReassignment() {
        if (currentToken().type != TokenType::TOKEN_IDENTIFIER) {
            throw std::runtime_error("Expected identifier at line " + std::to_string(currentToken().line) + ".");
        }
        std::unique_ptr<Identifier> left = std::make_unique<Identifier>(currentToken().lexeme);
        consumeToken();  // Consume the identifier token


        AssignmentOperator op;
        // Determine the operator based on the current token type
        switch (peek().type) {
            case TokenType::TOKEN_EQUAL:
                op = AssignmentOperator::ASSIGN;
                break;
            case TokenType::TOKEN_PLUS_PLUS:
                op = AssignmentOperator::ASSIGN_ADD;
                break;
            case TokenType::TOKEN_MINUS_EQUAL:
                op = AssignmentOperator::ASSIGN_SUB;
                break;
            case TokenType::TOKEN_ASTERISK_EQUAL:
                op = AssignmentOperator::ASSIGN_MUL;
                break;
            case TokenType::TOKEN_SLASH_EQUAL:
                op = AssignmentOperator::ASSIGN_DIV;
                break;
            case TokenType::TOKEN_PERCENT_EQUAL:
                op = AssignmentOperator::ASSIGN_MOD;
                break;
            default:
                throw std::runtime_error("Unexpected token at line " + std::to_string(currentToken().line) + ".");
        }
        consumeToken();  // Consume the assignment operator token

        // Now, parse the right-hand side expression.
        std::unique_ptr<Expression> right = parseExpression();

        // Finally, expect a semicolon to end the statement.
        expect(TokenType::TOKEN_SEMI_COLON);

        // Construct and return the AssignmentExpression node.
        return std::make_unique<AssignmentExpression>(op, std::move(left), std::move(right));
    }



    static bool isStartOfStatement(const Token& token) {
        switch(token.type) {
            case TOKEN_IF:         // if statement
            case TOKEN_WHILE:      // while loop
            case TOKEN_FOR:        // for loop
            case TOKEN_RETURN:     // return statement
            case TOKEN_BREAK:      // break statement
            case TOKEN_CONTINUE:   // continue statement
            case TOKEN_PRINT:      // print statement
            case TOKEN_IDENTIFIER: // variable assignment, currentFunction call, etc.
            case TOKEN_INT_TYPE: // Variable Declarations
            case TOKEN_SHORT_TYPE:
            case TOKEN_LONG_TYPE:
            case TOKEN_FLOAT_TYPE:
            case TOKEN_DOUBLE_TYPE:
            case TOKEN_CHAR_TYPE:
            case TOKEN_STRING_TYPE:
            case TOKEN_BOOL_TYPE:
            case TOKEN_VOID_TYPE:
            case TOKEN_LET:
            case TOKEN_CONST:
                return true;
            default:
                return false;
        }
    }

public:
//        Parser() : currentScope(new Scope()) {} // Start with a global scope
    Parser(std::vector<Token> tokens) {
        this->tokens = tokens;
        currentScope = new Scope();
    }

    void enterNewScope() {
        currentScope = currentScope->createChildScope();
    }

    void exitCurrentScope() {
        Scope* parent = currentScope->getParentScope();
        if (parent) { // Ensure we aren't at global scope
            delete currentScope; // Free up the current scope
            currentScope = parent;
        }
    }

    std::vector<std::unique_ptr<ASTNode>> parse() {
        std::vector<std::unique_ptr<ASTNode>> statements;
        while (currentToken().type != TokenType::TOKEN_EOF) {
            std::unique_ptr<ASTNode> statement = parseStatements();
            if (statement) {
                statements.push_back(std::move(statement));
            }
            if (currentToken().type == TokenType::TOKEN_NEWLINE || currentToken().type == TokenType::TOKEN_SEMI_COLON) {
                consumeToken();  // consume newline or semicolon to move to the next statement
            }
        }
        return statements;
    }

    std::unique_ptr<ASTNode> parseStatements() {
        switch(currentToken().type) {
            case TokenType::TOKEN_IMPORT:
                return parseImportStatement();
            case TokenType::TOKEN_INT_TYPE:
            case TokenType::TOKEN_SHORT_TYPE:
            case TokenType::TOKEN_LONG_TYPE:
            case TokenType::TOKEN_DOUBLE_TYPE:
            case TokenType::TOKEN_FLOAT_TYPE:
            case TokenType::TOKEN_BOOL_TYPE:
            case TokenType::TOKEN_STRING_TYPE:
            case TokenType::TOKEN_CHAR_TYPE:
            case TokenType::TOKEN_VOID_TYPE:
            case TokenType::TOKEN_LET:
            case TokenType::TOKEN_VECTOR:
            case TokenType::TOKEN_UNORDERED_MAP:
            case TokenType::TOKEN_MAP:
            case TokenType::TOKEN_CONST:
                return parseVariableDeclaration();
            case TokenType::TOKEN_IF:
                return parseIfStatement();
            case TokenType::TOKEN_WHILE:
                return parseWhileLoop();
            case TokenType::TOKEN_FOR:
                return parseForLoop();
            case TokenType::TOKEN_FUNC:
                return parseFunctionDeclaration();
            case TokenType::TOKEN_CLASS:
                return parseClassDeclaration();
            case TokenType::TOKEN_IDENTIFIER:
                if (peek(1).type == TokenType::TOKEN_LPAREN) {
                    return parseFunctionCall();
                } else if (peek(1).type == TokenType::TOKEN_EQUAL) {
                    return parseVariableReassignment();
                } else {
                    throw std::runtime_error("Unexpected token after identifier at line " + std::to_string(currentToken().line));
                }

            case TokenType::TOKEN_STRING_LITERAL:
            case TokenType::TOKEN_CHAR_LITERAL:
            case TokenType::TOKEN_NUMBER:
                if (peek(1).type == TokenType::TOKEN_DOLLAR_SIGN) {
                    return parseVariableDeclaration();
                }
                return parsePrimaryExpression();
            case TokenType::TOKEN_RETURN:
                return parseReturnStatement();
            case TokenType::TOKEN_BREAK:
                return parseBreakStatement();
            case TokenType::TOKEN_CONTINUE:
                return parseContinueStatement();
            case TokenType::TOKEN_PRINT:
                return parsePrintStatement();
            case TokenType::TOKEN_READ:
                return parseReadExpression();
            case TokenType::TOKEN_NEWLINE:
                consumeToken();  // consume newline if it's there
                break;

            default:
                throw std::runtime_error("Unexpected token at line " + std::to_string(currentToken().line) + ".");
        }
        return nullptr;
    }
};

#endif //ASTRALANG_ASTRAPARSER_H