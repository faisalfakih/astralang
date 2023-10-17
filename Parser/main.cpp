#include "../Lexer/AstraLexer.h"
#include <string>
#include <memory>
#include <variant>
#include <vector>
#include <map>
#include <fstream>
#include <iostream>
#include <sstream>
#include <chrono>

#define GREEN "\033[32m"
#define RED "\033[31m"
#define BLUE "\033[34m"
#define RESET "\033[0m"
#define LOG(x) std::cout << x << std::endl;

namespace AstraLang {
    class ASTNode {
    public:
        virtual ~ASTNode() = default;
    };

    // Importing Libraries
    class Import : public ASTNode {
    public:
        explicit Import(std::string libraryName, std::string alias = "")
                : libraryName(std::move(libraryName)), alias(std::move(alias)) {}

        std::string libraryName;
        std::string alias;
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
        MemberVariable(std::unique_ptr<TypeRepresentation> type, std::string name, std::unique_ptr<Expression> initializer = nullptr)
            : type(std::move(type)), name(std::move(name)), initializer(std::move(initializer)) {}

        std::unique_ptr<TypeRepresentation> type;
        std::string name;
        std::unique_ptr<Expression> initializer;
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

    // Statements
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

    // Reassignment Statement
    enum AssignmentOperator { ASSIGN_ADD, ASSIGN_SUB, ASSIGN_MUL, ASSIGN_DIV, ASSIGN_MOD, ASSIGN }; // +=, -=, *=, /=, %=, =
    class AssignmentExpression : public Statement {
    public:
        AssignmentExpression(AssignmentOperator op, std::unique_ptr<Expression> left, std::unique_ptr<Expression> right)
                : op(op), left(std::move(left)), right(std::move(right)) {}

        AssignmentOperator op;
        std::unique_ptr<Expression> left, right;
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
        explicit ReadStatement(std::vector<std::unique_ptr<Statement>> valuesToRead)
                : values(std::move(valuesToRead)) {}
        std::vector<std::unique_ptr<Statement>> values;
    };

    // Declarations
    class VariableDeclaration : public Statement {
    public:
        VariableDeclaration(std::unique_ptr<TypeRepresentation> type, std::string name, std::unique_ptr<Expression> value = nullptr)
                : type(std::move(type)), name(std::move(name)), value(std::move(value)) {}
        std::unique_ptr<TypeRepresentation> type;
        std::string name;
        std::unique_ptr<Expression> value;
    };

    class VariableCall : public Statement{
    public:
        // Constructor taking the name of the variable being called.
        explicit VariableCall(const std::string& varName) : varName(varName) {}

        // Getter for the variable name.
        const std::string& getVarName() const { return varName; }

        // Other methods as needed for your application...

    private:
        std::string varName;  // Name of the variable being called.
    };

    class FunctionDeclaration : public ModifiableDeclaration {
    public:
        FunctionDeclaration(std::string name,
                            std::vector<std::unique_ptr<Parameter>> params,
                            std::unique_ptr<TypeRepresentation> returnType = nullptr,
                            std::unique_ptr<Statement> body = nullptr)
                : name(std::move(name)), params(std::move(params)), returnType(std::move(returnType)),
                  body(std::move(body)), functionScope(new Scope()) {}

        std::string name;
        std::vector<std::unique_ptr<Parameter>> params;
        std::unique_ptr<TypeRepresentation> returnType;
        std::unique_ptr<Statement> body;
        std::unique_ptr<Scope> functionScope;
    };


    class ClassDeclaration : public ModifiableDeclaration {
    public:
        ClassDeclaration(std::string name, std::string baseClassName,
                         std::vector<std::unique_ptr<MemberVariable>> privateMembers,
                         std::vector<std::unique_ptr<MemberVariable>> publicMembers,
                         std::vector<std::unique_ptr<MemberVariable>> protectedMembers,
                         std::vector<std::unique_ptr<FunctionDeclaration>> privateMethods,
                         std::vector<std::unique_ptr<FunctionDeclaration>> publicMethods,
                         std::vector<std::unique_ptr<FunctionDeclaration>> protectedMethods)
                : name(std::move(name)), baseClassName(std::move(baseClassName)),
                  privateMembers(std::move(privateMembers)),
                  publicMembers(std::move(publicMembers)),
                  protectedMembers(std::move(protectedMembers)),
                  privateMethods(std::move(privateMethods)),
                  publicMethods(std::move(publicMethods)),
                  protectedMethods(std::move(protectedMethods)),
                  classScope(std::make_unique<Scope>()) {}

        std::string name;
        std::string baseClassName;  // new member variable for base class name
        std::vector<std::unique_ptr<MemberVariable>> privateMembers;
        std::vector<std::unique_ptr<MemberVariable>> publicMembers;
        std::vector<std::unique_ptr<MemberVariable>> protectedMembers;
        std::vector<std::unique_ptr<FunctionDeclaration>> privateMethods;
        std::vector<std::unique_ptr<FunctionDeclaration>> publicMethods;
        std::vector<std::unique_ptr<FunctionDeclaration>> protectedMethods;
        std::unique_ptr<Scope> classScope;
    };


    class StructDeclaration : public ModifiableDeclaration {
    public:
        StructDeclaration(std::string name, std::string baseClassName,
                         std::vector<std::unique_ptr<MemberVariable>> privateMembers,
                         std::vector<std::unique_ptr<MemberVariable>> publicMembers,
                         std::vector<std::unique_ptr<MemberVariable>> protectedMembers,
                         std::vector<std::unique_ptr<FunctionDeclaration>> privateMethods,
                         std::vector<std::unique_ptr<FunctionDeclaration>> publicMethods,
                         std::vector<std::unique_ptr<FunctionDeclaration>> protectedMethods)
                : name(std::move(name)), baseClassName(std::move(baseClassName)),
                  privateMembers(std::move(privateMembers)),
                  publicMembers(std::move(publicMembers)),
                  protectedMembers(std::move(protectedMembers)),
                  privateMethods(std::move(privateMethods)),
                  publicMethods(std::move(publicMethods)),
                  protectedMethods(std::move(protectedMethods)),
                  classScope(std::make_unique<Scope>()) {}

        std::string name;
        std::string baseClassName;  // new member variable for base class name
        std::vector<std::unique_ptr<MemberVariable>> privateMembers;
        std::vector<std::unique_ptr<MemberVariable>> publicMembers;
        std::vector<std::unique_ptr<MemberVariable>> protectedMembers;
        std::vector<std::unique_ptr<FunctionDeclaration>> privateMethods;
        std::vector<std::unique_ptr<FunctionDeclaration>> publicMethods;
        std::vector<std::unique_ptr<FunctionDeclaration>> protectedMethods;
        std::unique_ptr<Scope> classScope;
    };


    // For Loops
    // For Loop
    class ForLoop : public Statement {
    public:
        ForLoop(std::unique_ptr<TypeRepresentation> type, std::string varName,
                std::unique_ptr<Expression> startExpr, std::unique_ptr<Expression> endExpr,
                std::unique_ptr<Statement> body)
                : type(std::move(type)), varName(std::move(varName)),
                  startExpr(std::move(startExpr)), endExpr(std::move(endExpr)),
                  body(std::move(body)), forScope(std::make_unique<Scope>()) {}

        std::unique_ptr<TypeRepresentation> type;
        std::string varName;
        std::unique_ptr<Expression> startExpr;
        std::unique_ptr<Expression> endExpr;
        std::unique_ptr<Statement> body;
        std::unique_ptr<Scope> forScope;
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

    // Logic Expression
    class LogicalExpression : public Expression {
    public:
        enum Operator {
            AND_AND,
            OR_OR
        };

        LogicalExpression(Operator op, std::unique_ptr<Expression> left, std::unique_ptr<Expression> right)
                : op(op), left(std::move(left)), right(std::move(right)) {}

        Operator op;
        std::unique_ptr<Expression> left, right;
    };

    class ComparisonExpression : public Expression {
    public:
        enum Operator {
            EQUAL,          // ==
            NOT_EQUAL,      // !=
            STRICT_EQUAL,   // ===
            STRICT_NOT_EQUAL, // !==
            GREATER,        // >
            LESS,           // <
            GREATER_EQUAL,  // >=
            LESS_EQUAL,     // <=
            LOGICAL_AND_AND,    // &&
            LOGICAL_OR_OR     // ||
        };

        ComparisonExpression(Operator op, std::unique_ptr<Expression> left, std::unique_ptr<Expression> right)
                : op(op), left(std::move(left)), right(std::move(right)) {}

        Operator op;
        std::unique_ptr<Expression> left, right;
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
                throw std::runtime_error("Unexpected token: " + currentToken().lexeme + " at line: " + std::to_string(currentToken().line)  + ". Was expecting " + std::to_string(type));
            }
            nextToken();
        }

        bool checkTokenType(TokenType type) {
            return peek().type == type;
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
            // TODO: Add extra statements
            if (checkTokenType(TokenType::TOKEN_IF)) {
                return parseIfStatement();
            } else if (checkTokenType(TokenType::TOKEN_WHILE)) {
                return parseWhileLoop();
            } else if (checkTokenType(TokenType::TOKEN_FOR)) {
                return parseForLoop();
            } else if (checkTokenType(TokenType::TOKEN_READ)) {
                return parseReadStatement();
            } else if (checkTokenType(TokenType::TOKEN_PRINT)) {
                return parsePrintStatement();
            } else if (checkTokenType(TokenType::TOKEN_IMPORT)) {
                Statement* stmtPtr = dynamic_cast<Statement*>(parseImportStatement().release());
                if(!stmtPtr) {
                    throw std::runtime_error("Unexpected error at line " + std::to_string(currentToken().line) + " column " + std::to_string(currentToken().column));
                }
                std::unique_ptr<Statement> import(stmtPtr);
            }
            else if (checkTokenType(TokenType::TOKEN_NEWLINE)) {
                consumeToken();  // consume newline if it's there
            }  else if (checkTokenType(TokenType::TOKEN_INT_TYPE) || checkTokenType(TokenType::TOKEN_SHORT_TYPE) || checkTokenType(TokenType::TOKEN_LONG_TYPE) || checkTokenType(TokenType::TOKEN_FLOAT_TYPE) || checkTokenType(TokenType::TOKEN_DOUBLE_TYPE) || checkTokenType(TokenType::TOKEN_CHAR_TYPE) || checkTokenType(TokenType::TOKEN_STRING_TYPE) || checkTokenType(TokenType::TOKEN_BOOL_TYPE) || checkTokenType(TokenType::TOKEN_VOID_TYPE)) {
                parseVariableDeclaration();
            } else if (checkTokenType(TokenType::TOKEN_IDENTIFIER)) {
                parseVariableReassignment();
            } else {
                throw std::runtime_error("Unexpected token at line " + std::to_string(currentToken().line) + " column " + std::to_string(currentToken().column) + " at token " + peek().lexeme);
            }
            return nullptr;
        }

        std::unique_ptr<Statement> parseBlockStatement() {
            expect(TokenType::TOKEN_LBRACE);
            std::vector<std::unique_ptr<Statement>> statements;

            if (checkTokenType(TokenType::TOKEN_NEWLINE)) {
                consumeToken();  // consume newline if it's there
            }

            while (!checkTokenType(TokenType::TOKEN_RBRACE) && !checkTokenType(TokenType::TOKEN_EOF)) {
                statements.push_back(parseStatement());
            }

            consumeToken();  // Consume the closing brace TOKEN_RBRACE
            return std::make_unique<BlockStatement>(std::move(statements));
        }


        // If Statements
        std::unique_ptr<Statement> parseIfStatement() {
            expect(TokenType::TOKEN_IF); // Check for the 'if' keyword
            expect(TokenType::TOKEN_LPAREN); // Then checkTokenType for an open bracket '('

            std::unique_ptr<Expression> condition = parseExpression(); // Parse the condition
            expect(TokenType::TOKEN_RPAREN); // Check for a closing bracket ')' after the condition

            std::unique_ptr<Statement> trueBranch;
            if (checkTokenType((TokenType::TOKEN_LBRACE))) { // Check for an open curly brace '{'
                trueBranch = parseBlockStatement(); // Parse the true branch
            } else if (peek().type == TokenType::TOKEN_NEWLINE) {
                consumeToken();  // consume newline if it's there
                trueBranch = parseStatement();
            } else if (isStartOfStatement(peek())) {
                trueBranch = parseStatement();
            } else {
                throw std::runtime_error("Expected '{' or newline after if condition at line " + std::to_string(currentToken().line));
            }

            std::unique_ptr<Statement> falseBranch = nullptr; // the else branch, initialized to null
            if (match(TokenType::TOKEN_ELSE)) { // If there is an else branch
                if (match(TokenType::TOKEN_IF)) { // If there's an 'else if'
                    falseBranch = parseIfStatement();
                } else if (match(TokenType::TOKEN_LBRACE)) { // If next token is '{'
                    falseBranch = parseBlockStatement(); // Parse a block of statements
                } else if (peek().type == TokenType::TOKEN_NEWLINE) {
                    consumeToken();  // consume newline if it's there
                    falseBranch = parseStatement();
                } else if (isStartOfStatement(peek())) {
                    falseBranch = parseStatement();
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
            if (checkTokenType(TokenType::TOKEN_LBRACE)) {
                body = parseBlockStatement();
            } else if (peek().type == TokenType::TOKEN_NEWLINE) {
                consumeToken();  // consume newline if it's there
                body = parseStatement();
            } else if (isStartOfStatement(peek())) {
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
                    std::unique_ptr<Expression> startExpr = parseExpression();
                    if (match(TokenType::TOKEN_ARROW)) {
                        std::unique_ptr<Expression> endExpr = parseExpression();
                        expect(TokenType::TOKEN_RPAREN);
                        std::unique_ptr<Statement> body = parseBlockStatement();
                        return std::make_unique<ForLoop>(std::move(type), std::move(varName),
                                                         std::move(startExpr), std::move(endExpr),
                                                         std::move(body));
                    } else {
                        throw std::runtime_error("Expected '->' at line " + std::to_string(currentToken().line));
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

        std::unique_ptr<Statement> parseReadStatement() {
            expect(TokenType::TOKEN_READ);
            expect(TokenType::TOKEN_LPAREN);

            std::vector<std::unique_ptr<Statement>> values;
            while (!checkTokenType(TokenType::TOKEN_RPAREN)) {
                std::unique_ptr<Statement> value = parseVariableDeclarationOrCall();
                values.push_back(std::move(value));
                if (checkTokenType(TokenType::TOKEN_COMMA)) {
                    consumeToken();  // Consume the comma token.
                } else if (!checkTokenType(TokenType::TOKEN_RPAREN)) {
                    throw std::runtime_error("Expected ',' or ')' at line " + std::to_string(currentToken().line));
                }
            }

            expect(TokenType::TOKEN_RPAREN);
            expect(TokenType::TOKEN_SEMI_COLON);

            return std::make_unique<ReadStatement>(std::move(values));
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
            expect(TokenType::TOKEN_FUNC);
            if (currentToken().type != TokenType::TOKEN_IDENTIFIER) {
                throw std::runtime_error("Expected a method name after 'func' at line " + std::to_string(currentToken().line) + ".");
            }

            std::string methodName = currentToken().lexeme;
            consumeToken();

            std::vector<std::unique_ptr<Parameter>> params = parseParameters();

            // Check if the method returns a type
            std::unique_ptr<TypeRepresentation> returnType = nullptr;
            if (checkTokenType(TokenType::TOKEN_ARROW)) {
                consumeToken();
                returnType = parseType();
                if (!returnType) {
                    throw std::runtime_error("Expected return type after '->' at line " + std::to_string(currentToken().line));
                }
            }

            std::unique_ptr<Statement> body;
            // Code block
            if (checkTokenType(TokenType::TOKEN_LBRACE)) {
                body = parseBlockStatement();
            } else if (peek().type == TokenType::TOKEN_NEWLINE) {
                consumeToken();  // consume newline if it's there
                body = parseStatement();
            } else if (isStartOfStatement(peek())) {
                body = parseStatement();
            } else {
                throw std::runtime_error("Expected statement, new line or open bracket at line " + std::to_string(currentToken().line));
            }

            return std::make_unique<FunctionDeclaration>(methodName, std::move(params), std::move(returnType), std::move(body));
        }


        // Parse Function Declaration
        std::unique_ptr<FunctionDeclaration> parseFunctionDeclaration() {
            expect(TokenType::TOKEN_FUNC);
            if (currentToken().type != TokenType::TOKEN_IDENTIFIER) {
                throw std::runtime_error("Expected a function name after 'func' at line " + std::to_string(currentToken().line) + ".");
            }

            std::string functionName = currentToken().lexeme;
            consumeToken();

            std::vector<std::unique_ptr<Parameter>> params = parseParameters();

            // Check if the function returns a type
            std::unique_ptr<TypeRepresentation> returnType = nullptr;
            if (checkTokenType(TokenType::TOKEN_ARROW)) {
                consumeToken();
                returnType = parseType();
                if (!returnType) {
                    throw std::runtime_error("Expected return type after '->' at line " + std::to_string(currentToken().line));
                }
            }

            std::unique_ptr<Statement> body;
            // Code block
            if (checkTokenType(TokenType::TOKEN_LBRACE)) {
                body = parseBlockStatement();
            } else if (peek().type == TokenType::TOKEN_NEWLINE) {
                consumeToken();  // consume newline if it's there
                body = parseStatement();
            } else if (isStartOfStatement(peek())) {
                body = parseStatement();
            } else {
                throw std::runtime_error("Expected statement, new line or open bracket at line " + std::to_string(currentToken().line));
            }

            return std::make_unique<FunctionDeclaration>(functionName, std::move(params), std::move(returnType), std::move(body));
        }

        // Parse Variable Declarations
        std::unique_ptr<VariableDeclaration> parseVariableDeclaration() {
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

        std::unique_ptr<VariableCall> parseVariableCall() {
            std::string varName = currentToken().lexeme;
            consumeToken();  // Consume the identifier token
            expect(TokenType::TOKEN_SEMI_COLON);
            return std::make_unique<VariableCall>(varName);
        }

        // Parse Variable Declaration or Reassignment
        std::unique_ptr<Statement> parseVariableDeclarationOrCall() { // TODO: FIX THIS FUNCTION
            if (checkTokenType(TokenType::TOKEN_IDENTIFIER)) {
                // Look ahead to the next token to decide whether it's a variable declaration or a call
                TokenType nextType = peek().type;
                if (nextType == TokenType::TOKEN_COLON || nextType == TokenType::TOKEN_EQUAL) {
                    // It's a variable declaration
                    return parseVariableDeclaration();
                } else {
                    // Assume it's a variable call
                    return parseVariableCall();
                }
            } else {
                throw std::runtime_error("Expected identifier at line " + std::to_string(currentToken().line));
            }
        }

        // Parse Class Declarations
        std::unique_ptr<ClassDeclaration> parseClassDeclaration() {
            expect(TokenType::TOKEN_CLASS);
            if (!checkTokenType(TokenType::TOKEN_IDENTIFIER)) {
                throw std::runtime_error("Expected a class name after 'class' at line " + std::to_string(currentToken().line) + ".");
            }

            enum AccessSpecifier {
                PRIVATE,
                PUBLIC,
                PROTECTED
            };

            AccessSpecifier accessSpecifier = AccessSpecifier::PRIVATE;

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

            expect(TokenType::TOKEN_LBRACE);  // Assuming the class body starts with a '{'

            while (!checkTokenType(TokenType::TOKEN_RBRACE) && !isAtEnd()) {  // Continue until '}' or end of input
                if (checkTokenType(TokenType::TOKEN_PRIVATE)) {
                    accessSpecifier = AccessSpecifier::PRIVATE;
                    consumeToken();
                } else if (checkTokenType(TokenType::TOKEN_PUBLIC)) {
                    accessSpecifier = AccessSpecifier::PUBLIC;
                    consumeToken();
                    consumeToken();
                } else if (checkTokenType(TokenType::TOKEN_PROTECTED)) {
                    accessSpecifier = AccessSpecifier::PROTECTED;
                    consumeToken();
                } else if (checkTokenType(TokenType::TOKEN_FUNC)) {
                    auto method = parseMethodDeclaration();
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
                } else {
                    parseStatement();
                }
            }

            expect(TokenType::TOKEN_RBRACE);  // Consume the closing '}'

            // Assuming your ClassDeclaration constructor matches this signature
            return std::make_unique<ClassDeclaration>(className, baseClassName, std::move(privateMembers), std::move(publicMembers), std::move(protectedMembers), std::move(privateMethods), std::move(publicMethods), std::move(protectedMethods));
        }

        std::unique_ptr<StructDeclaration> parseStructDeclaration() {
            enum AccessSpecifier {
                PRIVATE,
                PUBLIC,
                PROTECTED
            };

            AccessSpecifier accessSpecifier = AccessSpecifier::PUBLIC;

            expect(TokenType::TOKEN_STRUCT);
            if (!checkTokenType(TokenType::TOKEN_IDENTIFIER)) {
                throw std::runtime_error("Expected a struct name after 'struct' at line " + std::to_string(currentToken().line) + ".");
            }

            std::string className = currentToken().lexeme;
            consumeToken();

            std::string baseClassName;
            if (checkTokenType(TokenType::TOKEN_EXTENDS)) {
                consumeToken();
                if (!checkTokenType(TokenType::TOKEN_IDENTIFIER)) {
                    throw std::runtime_error("Expected a base struct name after 'extends' at line " + std::to_string(currentToken().line) + ".");
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

            expect(TokenType::TOKEN_LBRACE);  // Assuming the class body starts with a '{'

            while (!checkTokenType(TokenType::TOKEN_RBRACE) && !isAtEnd()) {  // Continue until '}' or end of input
                if (checkTokenType(TokenType::TOKEN_PRIVATE)) {
                    accessSpecifier = AccessSpecifier::PRIVATE;
                    consumeToken();
                } else if (checkTokenType(TokenType::TOKEN_PUBLIC)) {
                    accessSpecifier = AccessSpecifier::PUBLIC;
                    consumeToken();
                } else if (checkTokenType(TokenType::TOKEN_PROTECTED)) {
                    accessSpecifier = AccessSpecifier::PROTECTED;
                    consumeToken();
                } else if (checkTokenType(TokenType::TOKEN_FUNC)) {
                    auto method = parseMethodDeclaration();
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
                } else {
                    throw std::runtime_error("Unexpected token at line " + std::to_string(currentToken().line) + ".");
                }
            }

            expect(TokenType::TOKEN_RBRACE);  // Consume the closing '}'

            // Assuming your ClassDeclaration constructor matches this signature
            return std::make_unique<StructDeclaration>(className, baseClassName, std::move(privateMembers), std::move(publicMembers), std::move(protectedMembers), std::move(privateMethods), std::move(publicMethods), std::move(protectedMethods));
        }

        // Parse Function Call
        std::unique_ptr<FunctionCall> parseFunctionCall() {
            if (currentToken().type != TokenType::TOKEN_IDENTIFIER) {
                throw std::runtime_error("Expected a function name at line " + std::to_string(currentToken().line) + ".");
            }

            std::string functionName = currentToken().lexeme;
            consumeToken();

            // Check if the next token is an opening parenthesis
            if (currentToken().type != TokenType::TOKEN_LPAREN) {
                throw std::runtime_error("Expected '(' after function name at line " + std::to_string(currentToken().line) + ".");
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
                case TokenType::TOKEN_STRUCT:
                    return parseStructDeclaration();
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
                    return parseReadStatement();
                case TokenType::TOKEN_NEWLINE:
                    consumeToken();  // consume newline if it's there
                    break;

                default:
                    throw std::runtime_error("Unexpected token at line " + std::to_string(currentToken().line) + ".");
            }
            return nullptr;
        }
    };
}

int main() {
    auto start = std::chrono::high_resolution_clock::now();  // Record start time

    std::ios_base::sync_with_stdio(false);

    // Test
    std::ifstream file("/Users/faisalfakih/Desktop/Coding/AstraLang/Parser/test.astra");
    if (!file.is_open()) {
        std::cerr << "Unable to open file\n";
        return 1;
    }

    std::stringstream ss;
    std::string line;
    while (std::getline(file, line)) {
        ss << line << '\n';
    }
    file.close();

    std::string input = ss.str();
    AstraLang::Parser* parser = new AstraLang::Parser(Lexer(input));
    std::vector<std::unique_ptr<AstraLang::ASTNode>> parseTree = parser->parse();

    auto end = std::chrono::high_resolution_clock::now();  // Record end time

    std::chrono::duration<double> time_taken = end - start;  // Compute the difference
    LOG(GREEN << "Code works!\n" << BLUE << "Time taken: " << time_taken.count() * 1000  << std::setprecision(20) << " milliseconds" << RESET);
    return 0;
}