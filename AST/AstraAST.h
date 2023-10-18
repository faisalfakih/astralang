//
// Created by Faisal Fakih on 18/10/2023.
//

#ifndef ASTRALANG_ASTRAAST_H
#define ASTRALANG_ASTRAAST_H

#include <string>
#include <memory>
#include <variant>
#include <vector>
#include <map>
#include <fstream>
#include <iostream>
#include <sstream>
#include <chrono>

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
        VOID,
        VAR
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

class CustomType : public TypeRepresentation {
public:
    explicit CustomType(const std::string& typeName) : name(typeName) {}
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
    ReadStatement(std::vector<std::unique_ptr<Statement>> declarationsToRead,
                  std::vector<std::unique_ptr<Expression>> valuesToRead)
            : declarations(std::move(declarationsToRead)), values(std::move(valuesToRead)) {}

    std::vector<std::unique_ptr<Statement>> declarations;
    std::vector<std::unique_ptr<Expression>> values;
};

// Declarations
class VariableDeclaration : public Statement {
public:
    VariableDeclaration(std::unique_ptr<TypeRepresentation> type, std::string name, std::unique_ptr<Expression> value = nullptr, bool isConst = false)
            : type(std::move(type)), name(std::move(name)), value(std::move(value)), isConst(isConst) {}
    bool isConst;
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
    enum Kind {
        REGULAR,
        CONSTRUCTOR,
        DESTRUCTOR
    };

    FunctionDeclaration(std::string name,
                        std::vector<std::unique_ptr<Parameter>> params,
                        std::unique_ptr<TypeRepresentation> returnType = nullptr,
                        std::unique_ptr<Statement> body = nullptr, Kind kind = REGULAR)
            : name(std::move(name)), params(std::move(params)), returnType(std::move(returnType)),
              body(std::move(body)), functionScope(new Scope()), kind(kind) {}

    Kind kind;
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
                     std::vector<std::unique_ptr<FunctionDeclaration>> protectedMethods,
                     std::unique_ptr<FunctionDeclaration> constructor,
                     std::unique_ptr<FunctionDeclaration> destructor)
            : name(std::move(name)), baseClassName(std::move(baseClassName)),
              privateMembers(std::move(privateMembers)),
              publicMembers(std::move(publicMembers)),
              protectedMembers(std::move(protectedMembers)),
              privateMethods(std::move(privateMethods)),
              publicMethods(std::move(publicMethods)),
              protectedMethods(std::move(protectedMethods)),
              constructor(std::move(constructor)),
              destructor(std::move(destructor)),
              classScope(std::make_unique<Scope>()) {}

    std::string name;
    std::string baseClassName;  // new member variable for base class name
    std::vector<std::unique_ptr<MemberVariable>> privateMembers;
    std::vector<std::unique_ptr<MemberVariable>> publicMembers;
    std::vector<std::unique_ptr<MemberVariable>> protectedMembers;
    std::vector<std::unique_ptr<FunctionDeclaration>> privateMethods;
    std::vector<std::unique_ptr<FunctionDeclaration>> publicMethods;
    std::vector<std::unique_ptr<FunctionDeclaration>> protectedMethods;
    std::unique_ptr<FunctionDeclaration> constructor;
    std::unique_ptr<FunctionDeclaration> destructor;
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


#endif //ASTRALANG_ASTRAAST_H
