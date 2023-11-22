//
// Created by Faisal Fakih on 18/10/2023.
//

#ifndef ASTRALANG_ASTRASEMANTICANALYZER_H
#define ASTRALANG_ASTRASEMANTICANALYZER_H

#include "../AST/AstraAST.h"
#include <unordered_map>
#include <memory>
#include <vector>
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Support/raw_ostream.h"


#define LOG(x) std::cout << x << std::endl;


class SemanticAnalyzer {
public:
    SemanticAnalyzer()
            : context(std::make_unique<llvm::LLVMContext>()),
              module(std::make_unique<llvm::Module>("AstraModule", *context)),
              builder(std::make_unique<llvm::IRBuilder<>>(*context)),
              currentScope(new Scope),
              currentFunction(nullptr) {}

    ~SemanticAnalyzer() {
        delete currentFunction;
    }
    void analyze(const std::vector<std::unique_ptr<ASTNode>>& parseTree) {
        for (const std::unique_ptr<ASTNode>& node : parseTree) {
            analyzeNode(node.get());
        }
    }

    void printIR() {
        module->print(llvm::errs(), nullptr);
    }
private:
    std::unique_ptr<llvm::LLVMContext> context; // This is a container for all the LLVM IR
    std::unique_ptr<llvm::Module> module; // This is a container for all the LLVM IR
    std::unique_ptr<llvm::IRBuilder<>> builder; // This is a builder for LLVM IR
    std::unordered_map<std::string, llvm::Value*> namedValues;
    std::unordered_map<std::string, llvm::Value*> symbolTable;
    llvm::Function* currentFunction;
    std::unique_ptr<Scope> currentScope;

    void enterNewScope() {
        currentScope = std::make_unique<Scope>(currentScope->createChildScope());
    }
    void exitCurrentScope() {
        if (currentScope) {
            currentScope.reset(currentScope->getParentScope());
        }
        else {
            throw std::runtime_error("ERROR: Attempted to exit global scope.");
        }
    }

    void analyzeNode(const ASTNode* node) {
        switch (node->getNodeType()) {
            case ASTNodeType::VariableDeclaration: {
                const VariableDeclaration* varDecl = dynamic_cast<const VariableDeclaration*>(node);
                declareVariable(varDecl);
                break;
            }
            case ASTNodeType::FunctionDeclaration: {
                const FunctionDeclaration* funcDecl = dynamic_cast<const FunctionDeclaration*>(node);
                analyzeFunctionDeclaration(funcDecl);
                break;
            }
            case ASTNodeType::IfStatement: {
                const IfStatement* ifStatement = dynamic_cast<const IfStatement*>(node);
                analyzeIfStatement(ifStatement);
                break;
            }
            default:
                throw std::runtime_error("ERROR ENCOUNTERED");
        }
    }

    void declareVariable(const VariableDeclaration* varDecl) {
        llvm::Type* type = convertToLLVMType(varDecl->type.get());

        if (symbolTable.find(varDecl->name) != symbolTable.end()) {
            throw std::runtime_error("Variable '" + varDecl->name + "' already declared");
        }
        checkAssignment(varDecl, varDecl->value.get());

        // Handling Global Variables
        if (!currentFunction) {
            // Logic for global variable declaration
            llvm::Constant* initValue = nullptr;
            if (varDecl->value) {
                initValue = llvm::dyn_cast<llvm::Constant>(evaluateExpression(varDecl->value.get()));
                if (!initValue) {
                    throw std::runtime_error("Initializer for global variable '" + varDecl->name + "' is not a constant!");
                }
            } else {
                initValue = llvm::Constant::getNullValue(type);
            }

            llvm::GlobalVariable* globalVar = new llvm::GlobalVariable(
                    *module,
                    type,
                    varDecl->isConst,  // isConstant
                    llvm::GlobalValue::InternalLinkage,
                    initValue,  // Initializer
                    varDecl->name
            );

            symbolTable[varDecl->name] = globalVar;
            return;
        }

        // Handling Local Variables (within functions)
        llvm::BasicBlock* basicBlock = &currentFunction->getEntryBlock();
        builder->SetInsertPoint(basicBlock);

        llvm::AllocaInst* alloca = builder->CreateAlloca(type, nullptr, varDecl->name.c_str());
        if (varDecl->value) {
            llvm::Value* initValue = evaluateExpression(varDecl->value.get());
            builder->CreateStore(initValue, alloca);
        } else {
            llvm::Constant* defaultValue = llvm::Constant::getNullValue(type);
            builder->CreateStore(defaultValue, alloca);
        }

        symbolTable[varDecl->name] = alloca;
    }


    llvm::Type* convertToLLVMType(const TypeRepresentation* typeRep) {
        if (const BasicType* basicType = dynamic_cast<const BasicType*>(typeRep)) {
            switch (basicType->type) {
                case BasicType::Type::INT:
                    return llvm::Type::getInt32Ty(*context);
                case BasicType::Type::LONG:
                    return llvm::Type::getInt64Ty(*context);
                case BasicType::Type::SHORT:
                    return llvm::Type::getInt16Ty(*context);
                case BasicType::Type::FLOAT:
                    return llvm::Type::getFloatTy(*context);
                case BasicType::Type::DOUBLE:
                    return llvm::Type::getDoubleTy(*context);
                case BasicType::Type::BOOL:
                    return llvm::Type::getInt1Ty(*context);
                case BasicType::Type::CHAR:
                    return llvm::Type::getInt8Ty(*context);
                case BasicType::Type::STRING:
                    return llvm::Type::getInt8PtrTy(*context);
                default:
                    throw std::runtime_error("Incorrect type");
            }
        } else if (const ArrayType* arrayType = dynamic_cast<const ArrayType*>(typeRep)) {
            llvm::Type* elementType = convertToLLVMType(arrayType->elementType.get());
            return llvm::ArrayType::get(elementType, arrayType->size);
        } else if (const PointerType* pointerType = dynamic_cast<const PointerType*>(typeRep)) {
            llvm::Type* baseType = convertToLLVMType(pointerType->type.get());
            return llvm::PointerType::getUnqual(baseType);
        }
        // TODO: Add more types if necessary
        throw std::runtime_error("Unknown type representation");
    }


    // Variable Declaration
    void checkAssignment(const VariableDeclaration* varDecl, const Expression* rhs) {
        // Convert lhs and rhs types to LLVM types
        llvm::Type* lhsType = convertToLLVMType(varDecl->type.get());
        llvm::Type* rhsType = determineExpressionType(rhs, lhsType);

        // Compare the LLVM type representations
        if (!compareDataTypes(lhsType, rhsType)) {
            throw std::runtime_error("Type mismatch in assignment of variable " + varDecl->name );
        }

        if (lhsType->isPointerTy() && rhsType->isPointerTy()) {
//            llvm::AllocaInst* lhsAlloca = llvm::dyn_cast<llvm::AllocaInst>(lhsType);
//            llvm::AllocaInst* rhsAlloca = llvm::dyn_cast<llvm::AllocaInst>(rhsType);
//            if (lhsAlloca && rhsAlloca) {
//                llvm::Type* lhsPointerType = lhsAlloca->getAllocatedType();
//                llvm::Type* rhsPointerType = rhsAlloca->getAllocatedType();
//                if (!compareDataTypes(lhsPointerType, rhsPointerType)) {
//                    throw std::runtime_error("Type mismatch in pointer assignment of variable " + varDecl->name);
//                }
//            }

            llvm::Value* lhsValue = symbolTable[varDecl->name];
            llvm::Value* rhsValue = evaluateExpression(rhs);

            llvm::Type* lhsPtrType = lhsValue->getType();
            llvm::Type* rhsPtrType = rhsValue->getType();

        }

    }

    llvm::Value* evaluateExpression(const Expression* expression) {
        switch (expression->getNodeType()) {
            case ASTNodeType::NumberLiteral: {
                const NumberLiteral* numberLiteral = dynamic_cast<const NumberLiteral*>(expression);
                if (numberLiteral->isDecimal()) {
                    return llvm::ConstantFP::get(*context, llvm::APFloat(numberLiteral->getFloatValue()));
                } else {
                    return llvm::ConstantInt::get(*context, llvm::APInt(32, numberLiteral->getIntValue(), true));  // Assuming 32-bit integers
                }
            }
            case ASTNodeType::BinaryExpression: {
                const BinaryExpression* binaryExpression = dynamic_cast<const BinaryExpression*>(expression);
                llvm::Value* leftValue = evaluateExpression(binaryExpression->left.get());
                llvm::Value* rightValue = evaluateExpression(binaryExpression->right.get());
                switch (binaryExpression->op) {
                    case BinaryExpression::Operator::PLUS:
                        return builder->CreateAdd(leftValue, rightValue, "addtmp");
                    case BinaryExpression::Operator::MINUS:
                        return builder->CreateSub(leftValue, rightValue, "subtmp");
                    case BinaryExpression::Operator::MULTIPLY:
                        return builder->CreateMul(leftValue, rightValue, "multmp");
                    case BinaryExpression::Operator::DIVIDE:
                        return builder->CreateSDiv(leftValue, rightValue, "divtmp");
                    case BinaryExpression::Operator::MODULO:
                        return builder->CreateSRem(leftValue, rightValue, "modtmp");
                    default:
                        throw std::runtime_error("Unsupported binary operator");
                }
            }
                // TODO: Add other expression types
            default:
                throw std::runtime_error("Unsupported expression type");
        }
    }

    bool compareDataTypes(llvm::Type* type1, llvm::Type* type2) {
        return (type1->isIntegerTy() && type2->isIntegerTy()) ||
               (type1->isPointerTy() && type2->isPointerTy()) ||
               (type1->isVoidTy() && type2->isVoidTy()) ||
               (type1->isFloatTy() && type2->isFloatTy()) ||
               (type1->isDoubleTy() && type2->isDoubleTy()) ||
               (type1->isArrayTy() && type2->isArrayTy()); // TODO: Add more types
    }

    unsigned getBitWidth(llvm::Type* type) {
        return module->getDataLayout().getTypeSizeInBits(type); // <-- Retrieve DataLayout from within the module then get its size in bits
    }

    llvm::Type* determineExpressionType(const Expression* expression, llvm::Type* lhsType) {
        switch (expression->getNodeType()) {
            case ASTNodeType::NumberLiteral: {
                const NumberLiteral* numberLiteral = dynamic_cast<const NumberLiteral*>(expression);
                if (numberLiteral->isDecimal()) { // It's a decimal number
                    switch (getBitWidth(lhsType)) {
                        case 32:
                            return llvm::Type::getFloatTy(*context);
                        case 64:
                            return llvm::Type::getDoubleTy(*context);
                        default:
                            throw std::runtime_error("Unsupported bit width for float: " + std::to_string(getBitWidth(lhsType)));
                    }
                } else { // It's an integer
                    switch (getBitWidth(lhsType)) {
                        case 16:
                            return llvm::Type::getInt16Ty(*context);
                        case 32:
                            return llvm::Type::getInt32Ty(*context);
                        case 64:
                            return llvm::Type::getInt64Ty(*context);
                        default:
                            throw std::runtime_error("Unsupported bit width for integer: " + std::to_string(getBitWidth(lhsType)));
                    }
                }
            }
            case ASTNodeType::BinaryExpression: {
                const BinaryExpression* binaryExpression = dynamic_cast<const BinaryExpression*>(expression);
                // Assume both operands have the same type for simplicity, or check them separately
                return determineExpressionType(binaryExpression->left.get(), lhsType);
            }
            default:
                llvm::Value* expressionValue = evaluateExpression(expression);
                return expressionValue->getType(); // Get the type of the evaluated expression
        }
    }

    // TODO: Analyze currentFunction declarations
    void analyzeFunctionDeclaration(const FunctionDeclaration* funcDecl) {
        if (currentScope->getSymbol(funcDecl->name)) {
            throw std::runtime_error("Function '" + funcDecl->name + "' already declared");
        }

        llvm::Type* returnType = convertToLLVMType(funcDecl->returnType.get());

        std::vector<llvm::Type*> paramTypes;
        paramTypes.reserve(funcDecl->params.size());
        for (const std::unique_ptr<Parameter>& param : funcDecl->params) {
            paramTypes.push_back(convertToLLVMType(param->type.get()));
        }

        llvm::FunctionType* funcType = llvm::FunctionType::get(returnType, paramTypes, false);
        llvm::Function* function = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, funcDecl->name, module.get());


        // Change the current function
        currentFunction = function;
        enterNewScope();

        size_t idx = 0;
        for (auto& arg : function->args()) {
            arg.setName(funcDecl->params[idx]->name);
            symbolTable[arg.getName().str()] = &arg;
            ++idx;
        }

        for (const std::unique_ptr<Statement>& stmt : funcDecl->body->statements) {
            analyzeNode(stmt.get());
        }

        exitCurrentScope();
        currentFunction = nullptr;
    }

    void analyzeIfStatement(const IfStatement* ifStmt) {
        llvm::Value* conditionValue = analyzeLogicalExpression(dynamic_cast<const LogicalExpression*>(ifStmt->condition.get()));
        if (!conditionValue) {
            throw std::runtime_error("Failed to analyze condition");
        }

        // Convert condition to a boolean to check with 0 or 1
        conditionValue = builder->CreateICmpNE(conditionValue, llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context), 0, true), "ifcond");

        llvm::Function* function = builder->GetInsertBlock()->getParent();

        // Create blocks for the then and else case. Insert the 'then' block at the end of the function.
        llvm::BasicBlock* thenBlock = llvm::BasicBlock::Create(*context, "then", function);
        llvm::BasicBlock* elseBlock = llvm::BasicBlock::Create(*context, "else", function);
        llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(*context, "ifcont", function);

        builder->CreateCondBr(conditionValue, thenBlock, elseBlock);

        // Emit then block.
        builder->SetInsertPoint(thenBlock);
        analyzeNode(ifStmt->trueBranch.get());
        builder->CreateBr(mergeBlock);
        thenBlock = builder->GetInsertBlock();

        // Emit else block.
        builder->SetInsertPoint(elseBlock);
        if (ifStmt->falseBranch) {
            analyzeNode(ifStmt->falseBranch.get());
        }
        builder->CreateBr(mergeBlock);
        elseBlock = builder->GetInsertBlock();

        // Emit merge block.
        builder->SetInsertPoint(mergeBlock);
    }



    llvm::Value* analyzeLogicalExpression(const LogicalExpression* logExpr) {
        switch (logExpr->getNodeType()) {
            case ASTNodeType::ComparisonExpression: {
                const ComparisonExpression* compExpr = dynamic_cast<const ComparisonExpression*>(logExpr);
                llvm::Value* leftValue = evaluateExpression(compExpr->left.get());
                llvm::Value* rightValue = evaluateExpression(compExpr->right.get());
                switch (compExpr->op) {
                    case ComparisonExpression::Operator::EQUAL:
                        return builder->CreateICmpEQ(leftValue, rightValue, "eqtmp");
                    case ComparisonExpression::Operator::NOT_EQUAL:
                        return builder->CreateICmpNE(leftValue, rightValue, "netmp");
                    case ComparisonExpression::Operator::LESS:
                        return builder->CreateICmpSLT(leftValue, rightValue, "lttmp");
                    case ComparisonExpression::Operator::LESS_EQUAL:
                        return builder->CreateICmpSLE(leftValue, rightValue, "letmp");
                    case ComparisonExpression::Operator::GREATER:
                        return builder->CreateICmpSGT(leftValue, rightValue, "gttmp");
                    case ComparisonExpression::Operator::GREATER_EQUAL:
                        return builder->CreateICmpSGE(leftValue, rightValue, "getmp");
                    default:
                        throw std::runtime_error("Unsupported comparison operator");
                }
            }
            case ASTNodeType::LogicalExpression: {
                llvm::Value* leftValue = evaluateExpression(logExpr->left.get());
                llvm::Value* rightValue = evaluateExpression(logExpr->right.get());
                switch (logExpr->op) {
                    case LogicalExpression::Operator::AND:
                        return builder->CreateAnd(leftValue, rightValue, "andtmp");
                    case LogicalExpression::Operator::OR:
                        return builder->CreateOr(leftValue, rightValue, "ortmp");
                    default:
                        throw std::runtime_error("Unsupported logical operator");
                }
            }
            case ASTNodeType::UnaryExpression: {
                const UnaryExpression* unaryExpr = dynamic_cast<const UnaryExpression*>(logExpr);
                if (unaryExpr->op == UnaryExpression::Operator::NOT) {
                    llvm::Value* value = evaluateExpression(unaryExpr->operand.get()); // Updated this line
                    return builder->CreateNot(value, "nottmp");
                } else {
                    throw std::runtime_error("Unsupported unary operator in logical context");
                }
            }
            default:
                throw std::runtime_error("Unsupported logical expression type");
        }
    }


};


#endif //ASTRALANG_ASTRASEMANTICANALYZER_H