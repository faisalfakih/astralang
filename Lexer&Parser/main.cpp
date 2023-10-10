#include "AstraLexer.h"
#include <memory>
#include <variant>
#include <vector>
#include <map>

namespace AstraLang {
    class ASTNode {
    public:
        virtual ~ASTNode() = default;
    };

    // Importing Libraries
    class Import : public ASTNode {
    public:
        explicit Import(std::string modulePath)
                : modulePath(std::move(modulePath)) {}

        std::string modulePath;
    };

    // Abstract class for all expressions
    class Expression : public ASTNode {};

    // Represent arithmetic operators
    class BinaryExpression : public Expression {
    public:
        enum Operator {
            PLUS,
            MINUS,
            MULTIPLY,
            DIVIDE,
            MODULO
        };

        BinaryExpression(Operator op, std::unique_ptr<Expression> left, std::unique_ptr<Expression> right)
                : op(op), left(std::move(left)), right(std::move(right)) {}

        Operator op;
        std::unique_ptr<Expression> left;
        std::unique_ptr<Expression> right;
    };

    enum AssignmentOperator { ASSIGN_ADD, ASSIGN_SUB, ASSIGN_MUL, ASSIGN_DIV, ASSIGN_MOD }; // (+=, -=, *=, /=, %=)
    class AssignmentExpression : public Expression {
    public:
        AssignmentExpression(AssignmentOperator op, std::unique_ptr<Expression> left, std::unique_ptr<Expression> right)
                : op(op), left(std::move(left)), right(std::move(right)) {}

        AssignmentOperator op;
        std::unique_ptr<Expression> left, right;
    };


    // Represent literals
    class Literal : public Expression {};

    // Number literals
    class NumberLiteral : public Literal {
    public:
        explicit NumberLiteral(double value) : value(value) {}

        double value;
    };

    // Char literals
    class CharLiteral : public Literal {
    public:
        explicit CharLiteral(char value) : value(value) {}

        char value;
    };

    // String literals
    class StringLiteral : public Literal {
    public:
        explicit StringLiteral(std::string value) : value(std::move(value)) {}

        std::string value;
    };

    // Boolean literals
    class BooleanLiteral : public Literal {
    public:
        explicit BooleanLiteral(bool value) : value(value) {}

        bool value;
    };

    // Represent identifiers (variables)
    class Identifier : public Expression {
    public:
        explicit Identifier(std::string name) : name(std::move(name)) {}

        std::string name;
    };

    // Represent function calls
    class FunctionCall : public Expression {
    public:
        FunctionCall(std::string name, std::vector<std::unique_ptr<Expression>> args)
                : name(std::move(name)), args(std::move(args)) {}

        std::string name;
        std::vector<std::unique_ptr<Expression>> args;
    };


    // Types
    class TypeRepresentation {
    public:
        virtual ~TypeRepresentation() = default;
    };

    class BasicType : public TypeRepresentation {
    public:
        enum Type {
            INT,
            SHORT,
            LONG,
            FLOAT,
            DOUBLE,
            CHAR,
            STRING,
            BOOL,
            VOID
        };

        explicit BasicType(Type type) : type(type) {}

        Type type;
    };

    class ArrayType : public TypeRepresentation {
    public:
        ArrayType(std::unique_ptr<TypeRepresentation> elementType, std::unique_ptr<Expression> size)
                : elementType(std::move(elementType)), size(std::move(size)) {}
        std::unique_ptr<TypeRepresentation> elementType;
        std::unique_ptr<Expression> size;
    };

    class PointerType : public TypeRepresentation {
    public:
        explicit PointerType(std::unique_ptr<TypeRepresentation> type) : type(std::move(type)) {}
        std::unique_ptr<TypeRepresentation> type;
    };

    class ReferenceType : public TypeRepresentation {
    public:
        explicit ReferenceType(std::unique_ptr<TypeRepresentation> type) : type(std::move(type)) {}
        std::unique_ptr<TypeRepresentation> type;
    };

    class MemberVariable : public TypeRepresentation {
    public:
        MemberVariable(std::unique_ptr<TypeRepresentation> type, std::string name)
                : type(std::move(type)), name(std::move(name)) {}

        std::unique_ptr<TypeRepresentation> type;
        std::string name;
    };


    // Parameter
    class Parameter {
    public:
        Parameter(std::string paramName, std::unique_ptr<TypeRepresentation> paramType)
                : name(std::move(paramName)), type(std::move(paramType)) {}
        std::string name;
        std::unique_ptr<TypeRepresentation> type;
    };

    // Declarations
    class Declaration : public ASTNode {};

    // Scope
    class Scope {
    public:
        Scope(Scope* parent = nullptr) : parentScope(parent) {}

        Scope* createChildScope() {
            return new Scope(this);
        }

        Scope* getParentScope() const {
            return parentScope;
        }

        void addSymbol(const std::string& name, std::unique_ptr<Declaration> decl) {
            symbols[name] = std::move(decl);
        }

        Declaration* getSymbol(const std::string& name) {
            if (symbols.find(name) != symbols.end()) {
                return symbols[name].get();
            }
            // If not in the current scope, checkTokenType in the parent scope.
            if (parentScope) {
                return parentScope->getSymbol(name);
            }
            return nullptr;  // Not found in any scope.
        }

    private:
        std::map<std::string, std::unique_ptr<Declaration>> symbols;
        Scope* parentScope;
    };

    // Modifiers & Qualifiers
    class Modifier {
    public:
        enum class Type {
            STATIC,
            CONST,
            VIRTUAL,
            OVERRIDE,
            ABSTRACT
        };

        explicit Modifier(Type mod) : type(mod) {}

        Type getType() const {
            return type;
        }

    private:
        Type type;
    };

    // Modifiable Declaration
    class ModifiableDeclaration : public Declaration {
    public:
        void addModifier(Modifier::Type mod) {
            modifiers.push_back(mod);
        }

        bool hasModifier(Modifier::Type mod) const {
            return std::find(modifiers.begin(), modifiers.end(), mod) != modifiers.end();
        }

    protected:
        std::vector<Modifier::Type> modifiers;
    };


    // Declarations
    class VariableDeclaration : public ModifiableDeclaration {
    public:
        VariableDeclaration(std::unique_ptr<TypeRepresentation> type, std::string name, std::unique_ptr<Expression> value = nullptr)
                : type(std::move(type)), name(std::move(name)), value(std::move(value)) {}
        std::unique_ptr<TypeRepresentation> type;
        std::string name;
        std::unique_ptr<Expression> value;
    };

    class FunctionDeclaration : public ModifiableDeclaration {
    public:
        FunctionDeclaration(std::string name, std::vector<std::unique_ptr<Parameter>> params, std::unique_ptr<TypeRepresentation> returnType = nullptr)
                : name(std::move(name)), params(std::move(params)), returnType(std::move(returnType)), functionScope(new Scope()) {}

        std::string name;
        std::vector<std::unique_ptr<Parameter>> params;
        std::unique_ptr<TypeRepresentation> returnType;
        std::unique_ptr<Scope> functionScope;
    };

    class ClassDeclaration : public ModifiableDeclaration {
    public:
        ClassDeclaration(std::string name, std::vector<std::unique_ptr<MemberVariable>> privateMembers,std::vector<std::unique_ptr<MemberVariable>> publicMembers,
                         std::vector<std::unique_ptr<MemberVariable>> protectedMembers, std::vector<std::unique_ptr<FunctionDeclaration>> methods)
                         : name(std::move(name)), privateMembers(std::move(privateMembers)), publicMembers(std::move(publicMembers)),
                            protectedMembers(std::move(protectedMembers)), methods(std::move(methods)), classScope(std::make_unique<Scope>()) {}
        std::string name;
        std::vector<std::unique_ptr<MemberVariable>> privateMembers;
        std::vector<std::unique_ptr<MemberVariable>> publicMembers;
        std::vector<std::unique_ptr<MemberVariable>> protectedMembers;
        std::vector<std::unique_ptr<FunctionDeclaration>> methods;
        std::unique_ptr<Scope> classScope;
    };
    class StructDeclaration : public ModifiableDeclaration {
    public:
        StructDeclaration(std::string name, std::vector<std::unique_ptr<MemberVariable>> privateMembers,std::vector<std::unique_ptr<MemberVariable>> publicMembers,
                          std::vector<std::unique_ptr<MemberVariable>> protectedMembers, std::vector<std::unique_ptr<FunctionDeclaration>> methods)
                : name(std::move(name)), privateMembers(std::move(privateMembers)), publicMembers(std::move(publicMembers)),
                  protectedMembers(std::move(protectedMembers)), methods(std::move(methods)), classScope(std::make_unique<Scope>()) {}
        std::string name;
        std::vector<std::unique_ptr<MemberVariable>> privateMembers;
        std::vector<std::unique_ptr<MemberVariable>> publicMembers;
        std::vector<std::unique_ptr<MemberVariable>> protectedMembers;
        std::vector<std::unique_ptr<FunctionDeclaration>> methods;
        std::unique_ptr<Scope> classScope;
    };

    // Base class for statements
    class Statement : public ASTNode {};

    class BlockStatement : public Statement {
    public:
        explicit BlockStatement(std::vector<std::unique_ptr<Statement>> statements)
                : statements(std::move(statements)), blockScope(new Scope()) {}

        std::vector<std::unique_ptr<Statement>> statements;
        std::unique_ptr<Scope> blockScope;
    };

    // Expression Statement
    class ExpressionStatement : public Statement {
    public:
        explicit ExpressionStatement(std::unique_ptr<Expression> expr) : expr(std::move(expr)) {}

        std::unique_ptr<Expression> expr;
    };

    // If Statement
    class IfStatement : public Statement {
    public:
        IfStatement(std::unique_ptr<Expression> condition, std::unique_ptr<Statement> trueBranch, std::unique_ptr<Statement> falseBranch = {})
                : condition(std::move(condition)), trueBranch(std::move(trueBranch)), falseBranch(std::move(falseBranch)), ifScope(std::make_unique<Scope>()) {}
        std::unique_ptr<Expression> condition;
        std::unique_ptr<Statement> trueBranch;
        std::unique_ptr<Statement> falseBranch;
        std::unique_ptr<Scope> ifScope;
    };

    // While Loop
    class WhileLoop : public Statement {
    public:
        WhileLoop(std::unique_ptr<Expression> condition, std::unique_ptr<Statement> body)
                : condition(std::move(condition)), body(std::move(body)), whileScope(std::make_unique<Scope>()) {}
        std::unique_ptr<Expression> condition;
        std::unique_ptr<Statement> body;
        std::unique_ptr<Scope> whileScope;
    };

    // For Loop
    class ForLoop : public Statement {
    public:
        ForLoop(std::unique_ptr<VariableDeclaration> initializer, std::unique_ptr<Expression> condition, std::unique_ptr<Expression> increment,
                     std::unique_ptr<Statement> body)
                : initializer(std::move(initializer)), condition(std::move(condition)), increment(std::move(increment)), body(std::move(body)),
                  forScope(std::make_unique<Scope>()) {}
        std::unique_ptr<VariableDeclaration> initializer;
        std::unique_ptr<Expression> condition;
        std::unique_ptr<Expression> increment;
        std::unique_ptr<Statement> body;
        std::unique_ptr<Scope> forScope;
    };

    // For Each
    class ForEachLoop : public Statement {
    public:
        ForEachLoop(std::unique_ptr<TypeRepresentation> variableType, std::string variableName, std::unique_ptr<Expression> iterable,
                    std::unique_ptr<Statement> body)
                : variableType(std::move(variableType)), variableName(std::move(variableName)), iterable(std::move(iterable)),
                  body(std::move(body)), forEachScope(std::make_unique<Scope>()) {}

        std::unique_ptr<TypeRepresentation> variableType;
        std::string variableName;
        std::unique_ptr<Expression> iterable;
        std::unique_ptr<Statement> body;
        std::unique_ptr<Scope> forEachScope;
    };
    // Return Statement
    class ReturnStatement : public Statement {
    public:
        explicit ReturnStatement(std::unique_ptr<Expression> returnValue)
                : returnValue(std::move(returnValue)) {}

        std::unique_ptr<Expression> returnValue;
    };

    // Break & Continue Statements
    class BreakStatement : public Statement {};
    class ContinueStatement : public Statement {};

    // Represents a single 'catch' block
    class CatchBlock : public Statement {
    public:
        CatchBlock(std::unique_ptr<TypeRepresentation> exceptionType, std::string exceptionName, std::unique_ptr<BlockStatement> block)
                : exceptionType(std::move(exceptionType)), exceptionName(std::move(exceptionName)), block(std::move(block)) {}

        std::unique_ptr<TypeRepresentation> exceptionType;
        std::string exceptionName;
        std::unique_ptr<BlockStatement> block;
    };

    // Try statement
    class TryStatement : public Statement {
    public:
        TryStatement(std::unique_ptr<BlockStatement> tryBlock, std::vector<std::unique_ptr<CatchBlock>> catchBlocks, std::unique_ptr<BlockStatement> finallyBlock = nullptr)
                : tryBlock(std::move(tryBlock)), catchBlocks(std::move(catchBlocks)), finallyBlock(std::move(finallyBlock)) {}

        std::unique_ptr<BlockStatement> tryBlock;
        std::vector<std::unique_ptr<CatchBlock>> catchBlocks;
        std::unique_ptr<BlockStatement> finallyBlock;
    };

    // Finally statement
    class FinallyStatement : public Statement {
    public:
        explicit FinallyStatement(std::unique_ptr<BlockStatement> finallyBlock)
                : finallyBlock(std::move(finallyBlock)) {}

        std::unique_ptr<BlockStatement> finallyBlock;
    };

    // Print Statement
    class PrintStatement : public Statement {
    public:
        explicit PrintStatement(std::unique_ptr<Expression> valueToPrint)
                : value(std::move(valueToPrint)) {}
        std::unique_ptr<Expression> value;
    };

    // Read Statement
    class ReadStatement : public Statement {
    public:
        explicit ReadStatement(std::unique_ptr<Expression> valueToRead)
            : value(std::move(valueToRead)) {}
        std::unique_ptr<Expression> value;
    };

    class UnaryExpression : public Expression {
    public:
        enum Operator {
            NEGATION,   // For '-'
            NOT,        // For '!'
            DEREFERENCE, // For '*'
            REFERENCE  // For '&'
        };

        UnaryExpression(Operator op, std::unique_ptr<Expression> operand)
                : op(op), operand(std::move(operand)) {}

        Operator op;
        std::unique_ptr<Expression> operand;
    };

    // Ternary Expressions
    class TernaryExpression : public Expression {
    public:
        TernaryExpression(std::unique_ptr<Expression> condition, std::unique_ptr<Expression> trueExpr, std::unique_ptr<Expression> falseExpr)
                : condition(std::move(condition)), trueExpr(std::move(trueExpr)), falseExpr(std::move(falseExpr)) {}

        std::unique_ptr<Expression> condition, trueExpr, falseExpr;
    };

    // Prefix expressions
    class PostfixExpression : public Expression {
    public:
        enum Operator { INCREMENT, DECREMENT }; // ++ and --
        PostfixExpression(Operator op, std::unique_ptr<Expression> operand)
                : op(op), operand(std::move(operand)) {}

        Operator op;
        std::unique_ptr<Expression> operand;
    };

    class PrefixExpression : public Expression {
    public:
        enum Operator { INCREMENT, DECREMENT };  // ++ and --
        PrefixExpression(Operator op, std::unique_ptr<Expression> operand)
                : op(op), operand(std::move(operand)) {}

        Operator op;
        std::unique_ptr<Expression> operand;
    };

    // Type Casting
    class TypeCastExpression : public Expression {
    public:
        TypeCastExpression(std::unique_ptr<TypeRepresentation> targetType, std::unique_ptr<Expression> expr)
                : targetType(std::move(targetType)), expr(std::move(expr)) {}

        std::unique_ptr<TypeRepresentation> targetType;
        std::unique_ptr<Expression> expr;
    };

    class TypeCheckExpression : public Expression {
    public:
        TypeCheckExpression(std::unique_ptr<Expression> expr, std::unique_ptr<TypeRepresentation> type)
                : expr(std::move(expr)), type(std::move(type)) {}

        std::unique_ptr<Expression> expr;
        std::unique_ptr<TypeRepresentation> type;
    };

    // Parser
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
                throw std::runtime_error("Unexpected token: " + currentToken().lexeme);
            }
            nextToken();
        }

        bool checkTokenType(TokenType type) {
            return peek().type == type;
        }


        // Variable Declaration
        std::unique_ptr<VariableDeclaration> parseVariableDelcaration() {
            std::unique_ptr<TypeRepresentation> type = parseType();
            if (type == nullptr) { // If there is no type
                throw std::runtime_error("Expected a type for variable declaration. Line: " + std::to_string(currentToken().line) + ", Column: " + std::to_string(currentToken().column));
            }

            expect(TokenType::TOKEN_IDENTIFIER);
            std::string varName = currentToken().lexeme;

            std::unique_ptr<Expression> initValue = nullptr;
            if (match(TokenType::TOKEN_EQUAL)) {
                initValue = parseExpression();
            }

            expect(TokenType::TOKEN_SEMI_COLON);

            return std::make_unique<VariableDeclaration>(std::move(type), varName, std::move(initValue));
        }

        std::unique_ptr<TypeRepresentation> parseType() {
            std::map<TokenType, BasicType::Type> typeMap = {
                    { TokenType::TOKEN_INT_TYPE, BasicType::INT },
                    { TokenType::TOKEN_SHORT_TYPE, BasicType::SHORT },
                    { TokenType::TOKEN_LONG_TYPE, BasicType::LONG },
                    { TokenType::TOKEN_FLOAT_TYPE, BasicType::FLOAT },
                    { TokenType::TOKEN_DOUBLE_TYPE, BasicType::DOUBLE },
                    { TokenType::TOKEN_CHAR_TYPE, BasicType::CHAR },
                    { TokenType::TOKEN_STRING_TYPE, BasicType::STRING },
                    { TokenType::TOKEN_BOOL_TYPE, BasicType::BOOL },
                    { TokenType::TOKEN_VOID_TYPE, BasicType::VOID }
            };

            for (const auto& [tokenType, basicType] : typeMap) {
                if (match(tokenType)) {
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
            return parseBinaryExpression();
        }
        std::unique_ptr<Expression> parseBinaryExpression() {
            return parseAdditionSubtraction();
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
            if (match(TokenType::TOKEN_NUMBER)) {
                return std::make_unique<NumberLiteral>(std::stod(currentToken().lexeme));
            }
            if (match(TokenType::TOKEN_CHAR_LITERAL)) {
                if (currentToken().lexeme.length() == 1) {
                    return std::make_unique<CharLiteral>(currentToken().lexeme[0]);
                } else {
                    throw std::runtime_error("Expected expression at line " + std::to_string(currentToken().line) + ", column " + std::to_string(currentToken().column));
                }
            }
            if (match(TokenType::TOKEN_STRING_LITERAL)) {
                return std::make_unique<StringLiteral>(currentToken().lexeme);
            }
            if (match(TokenType::TOKEN_TRUE) || match(TokenType::TOKEN_FALSE)) {
                bool value = currentToken().type == TokenType::TOKEN_TRUE;
                return std::make_unique<BooleanLiteral>(value);
            }
            if (match(TokenType::TOKEN_IDENTIFIER)) {
                return std::make_unique<Identifier>(currentToken().lexeme);
            }
            if (match(TokenType::TOKEN_LPAREN)) {
                auto expr = parseExpression();
                expect(TokenType::TOKEN_RPAREN);
                return expr;
            }
            throw std::runtime_error("Expected expression at line " + std::to_string(currentToken().line) + ", column " + std::to_string(currentToken().column));
        }

        // Parse Statement
        std::unique_ptr<Statement> parseStatement() {
            // TODO: Add extra statements
            if (match(TokenType::TOKEN_IF)) {
                return parseIfStatement();
            } else if (match(TokenType::TOKEN_WHILE)) {
                return parseWhileLoop();
            } else if (match(TokenType::TOKEN_FOR)) {
                return parseForLoop();
            } else if (match(TokenType::TOKEN_READ)) {
                return parseReadStatement();
            } else if (match(TokenType::TOKEN_PRINT)) {
                return parsePrintStatement();
            } else {
                throw std::runtime_error("Unexpected token at line " + std::to_string(currentToken().line) + " column " + std::to_string(currentToken().column));
            }
        }

        std::unique_ptr<Statement> parseBlockStatement() {
            std::vector<std::unique_ptr<Statement>> statements;

            expect(TokenType::TOKEN_LBRACE);

            while (!checkTokenType(TokenType::TOKEN_RBRACE) && !checkTokenType(TokenType::TOKEN_EOF)) {
                statements.push_back(parseStatement());
                expect(TokenType::TOKEN_SEMI_COLON);
            }

            consumeToken();  // Consume the closing brace TOKEN_RBRACE
            return std::make_unique<BlockStatement>(std::move(statements));
            // Assuming you have a BlockStatement class that takes a vector of Statements as its constructor.
        }


        // If Statements
        std::unique_ptr<Statement> parseIfStatement() {
            expect(TokenType::TOKEN_IF); // Check for the 'if' keyword
            expect(TokenType::TOKEN_LPAREN); // Then checkTokenType for an open bracket '('

            std::unique_ptr<Expression> condition = parseExpression(); // Parse the condition
            expect(TokenType::TOKEN_RPAREN); // Check for a closing bracket ')' after the condition

            std::unique_ptr<Statement> trueBranch;
            if (match((TokenType::TOKEN_LBRACE))) { // Check for an open curly brace '{'
                trueBranch = parseBlockStatement(); // Parse the true branch
            } else if (peek().type == TokenType::TOKEN_NEWLINE || isStartOfStatement(peek())) {
                consumeToken(); // Consume newline if it's there
                trueBranch = parseStatement(); // Parse a single statement
            } else {
                throw std::runtime_error("Expected '{' or newline after if condition at line " + std::to_string(currentToken().line));
            }

            std::unique_ptr<Statement> falseBranch = nullptr; // the else branch, initialized to null
            if (match(TokenType::TOKEN_ELSE)) { // If there is an else branch
                if (match(TokenType::TOKEN_IF)) { // If there's an 'else if'
                    falseBranch = parseIfStatement();
                } else if (match(TokenType::TOKEN_LBRACE)) { // If next token is '{'
                    falseBranch = parseBlockStatement(); // Parse a block of statements
                } else if (peek().type == TokenType::TOKEN_NEWLINE || isStartOfStatement(peek())) {
                    consumeToken(); // Consume newline if it's there
                    falseBranch = parseStatement(); // Parse a single statement
                } else {
                    throw std::runtime_error("Expected '{' or newline after else at line " + std::to_string(currentToken().line));
                }
            }

            return std::make_unique<IfStatement>(std::move(condition), std::move(trueBranch), std::move(falseBranch));
        }

        // While Loop
        std::unique_ptr<Statement> parseWhileLoop() {
            expect(TokenType::TOKEN_WHILE);
            expect(TokenType::TOKEN_LPAREN);

            std::unique_ptr<Expression> condition = parseExpression();
            expect(TokenType::TOKEN_RPAREN);

            std::unique_ptr<Statement> body;
            if (match(TokenType::TOKEN_LBRACE)) {
                body = parseBlockStatement();
            } else if (peek().type == TokenType::TOKEN_NEWLINE || isStartOfStatement(peek())) {
                consumeToken();  // consume newline if it's there
                body = parseStatement();
            } else {
                throw std::runtime_error("Expected '{', newline, or start of statement after while condition at line " + std::to_string(currentToken().line));
            }

            return std::make_unique<WhileLoop>(std::move(condition), std::move(body));
        }

        // For Loop
        std::unique_ptr<Statement> parseForLoop() {
            expect(TokenType::TOKEN_FOR);
            expect(TokenType::TOKEN_LPAREN);

            if (peek(1).type == TokenType::TOKEN_OF) {
                // A foreach loop is detected
                std::unique_ptr<TypeRepresentation> variableType = parseType();
                expect(TokenType::TOKEN_IDENTIFIER);
                std::string variableName = currentToken().lexeme;
                expect(TokenType::TOKEN_COLON);
                std::unique_ptr<Expression> iterable = parseExpression();
                expect(TokenType::TOKEN_RPAREN);

                std::unique_ptr<Statement> body;
                if (match(TokenType::TOKEN_LBRACE)) {
                    body = parseBlockStatement();
                } else {
                    throw std::runtime_error("Expected '{' after for-each declaration at line " + std::to_string(currentToken().line));
                }

                return std::make_unique<ForEachLoop>(std::move(variableType), variableName, std::move(iterable), std::move(body));
            } else {
                std::unique_ptr<VariableDeclaration> declaration = parseVariableDelcaration();
                expect(TokenType::TOKEN_SEMI_COLON);
                std::unique_ptr<Expression> condition = parseExpression();
                std::unique_ptr<Expression> increment = parseExpression();
                expect(TokenType::TOKEN_RPAREN);

                std::unique_ptr<Statement> body;
                if (match(TokenType::TOKEN_LBRACE)) {
                    body = parseBlockStatement();
                } else if (peek().type == TokenType::TOKEN_NEWLINE || isStartOfStatement(peek())) {
                    consumeToken();  // consume newline if it's there
                    body = parseStatement();
                } else {
                    throw std::runtime_error("Expected '{', newline, or start of statement after for condition at line " + std::to_string(currentToken().line));
                }

                return std::make_unique<ForLoop>(std::move(declaration), std::move(condition), std::move(increment), std::move(body));

            }
        }

        std::unique_ptr<Statement> parsePrintStatement() {
            expect(TokenType::TOKEN_PRINT);
            expect(TokenType::TOKEN_LPAREN);


            std::unique_ptr<Expression> expression = parseExpression();

            expect(TokenType::TOKEN_RPAREN);
            expect(TokenType::TOKEN_SEMI_COLON);

            return std::make_unique<PrintStatement>(std::move(expression));

        }

        std::unique_ptr<Statement> parseReadStatement() {
            expect(TokenType::TOKEN_READ);
            expect(TokenType::TOKEN_LPAREN);

            std::unique_ptr<Expression> expression = parseExpression();

            expect(TokenType::TOKEN_RPAREN);
            expect(TokenType::TOKEN_SEMI_COLON);
        }


        bool isStartOfStatement(const Token& token) {
            switch(token.type) {
                case TOKEN_IF:         // if statement
                case TOKEN_WHILE:      // while loop
                case TOKEN_FOR:        // for loop
                case TOKEN_RETURN:     // return statement
                case TOKEN_BREAK:      // break statement
                case TOKEN_CONTINUE:   // continue statement
                case TOKEN_PRINT:      // print statement
                case TOKEN_IDENTIFIER: // variable assignment, function call, etc.
                    return true;
                default:
                    return false;
            }
        }

    public:
        Parser() : currentScope(new Scope()) {} // Start with a global scope

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
    };
}