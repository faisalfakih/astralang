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

        void addSymbol(const std::string& name, std::unique_ptr<Declaration> decl) {
            symbols[name] = std::move(decl);
        }

        Declaration* getSymbol(const std::string& name) {
            if (symbols.find(name) != symbols.end()) {
                return symbols[name].get();
            }
            // If not in the current scope, check in the parent scope.
            if (parentScope) {
                return parentScope->getSymbol(name);
            }
            return nullptr;  // Not found in any scope.
        }

    private:
        std::map<std::string, std::unique_ptr<Declaration>> symbols;
        Scope* parentScope;
    };


    // Declarations
    class VariableDeclaration : public Declaration {
    public:
        VariableDeclaration(std::unique_ptr<TypeRepresentation> type, std::string name, std::unique_ptr<Expression> value)
                : type(std::move(type)), name(std::move(name)), value(std::move(value)) {}

        std::unique_ptr<TypeRepresentation> type;
        std::string name;
        std::unique_ptr<Expression> value;
    };

    class FunctionDeclaration : public Declaration {
    public:
        FunctionDeclaration(std::string name, std::vector<std::unique_ptr<Parameter>> params, std::unique_ptr<TypeRepresentation> returnType = nullptr)
                : name(std::move(name)), params(std::move(params)), returnType(std::move(returnType)), functionScope(new Scope()) {}

        std::string name;
        std::vector<std::unique_ptr<Parameter>> params;
        std::unique_ptr<TypeRepresentation> returnType;
        std::unique_ptr<Scope> functionScope;
    };

    class ClassDeclaration : public Declaration {
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
    class StructDeclaration : public Declaration {
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

    // While Statement
    class WhileStatement : public Statement {
    public:
        WhileStatement(std::unique_ptr<Expression> condition, std::unique_ptr<Statement> body)
                : condition(std::move(condition)), body(std::move(body)), whileScope(std::make_unique<Scope>()) {}
        std::unique_ptr<Expression> condition;
        std::unique_ptr<Statement> body;
        std::unique_ptr<Scope> whileScope;
    };

    // For Statement
    class ForStatement : public Statement {
    public:
        ForStatement(std::unique_ptr<Statement> initializer, std::unique_ptr<Expression> condition, std::unique_ptr<Expression> increment,
                     std::unique_ptr<Statement> body)
                : initializer(std::move(initializer)), condition(std::move(condition)), increment(std::move(increment)), body(std::move(body)),
                  forScope(std::make_unique<Scope>()) {}
        std::unique_ptr<Statement> initializer;
        std::unique_ptr<Expression> condition;
        std::unique_ptr<Expression> increment;
        std::unique_ptr<Statement> body;
        std::unique_ptr<Scope> forScope;
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
}