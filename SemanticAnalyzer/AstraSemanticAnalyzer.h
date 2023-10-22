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
              currentScope(new Scope) {}

    void analyze(const std::vector<std::unique_ptr<ASTNode>>& parseTree) {
        for (const std::unique_ptr<ASTNode>& node : parseTree) {
            analyzeNode(node.get());
        } // TODO: Add checks for a main function
    }

    void printIR() {
        module->print(llvm::errs(), nullptr);
    }
private:
    bool hasMainFunction = false;
    std::unique_ptr<llvm::LLVMContext> context; // This is a container for all the LLVM IR
    std::unique_ptr<llvm::Module> module; // This is a container for all the LLVM IR
    std::unique_ptr<llvm::IRBuilder<>> builder; // This is a builder for LLVM IR
    std::unordered_map<std::string, llvm::Value*> namedValues;
    std::unordered_map<std::string, llvm::Value*> symbolTable;
    Scope* currentScope;

    void enterNewScope() {
        currentScope = currentScope->createChildScope();
    }

    void exitCurrentScope() {
        Scope* parent = currentScope->getParentScope();
        if (parent) { // If parent exists (if it's not equal to nullptr)
            delete currentScope;
            currentScope = parent;
        }
    }

    void analyzeNode(const ASTNode* node) {
        switch (node->getNodeType()) {
            case ASTNodeType::VariableDeclaration: {
                const VariableDeclaration* varDecl = dynamic_cast<const VariableDeclaration*>(node);
                declareVariable(varDecl);
                break;
            }
            default:
                throw std::runtime_error("ERROR ENCOUNTERED");
        }
    }

    void declareVariable(const VariableDeclaration* varDecl) {
        llvm::Type* type = convertToLLVMType(varDecl->type.get(), varDecl);

        if (symbolTable.find(varDecl->name) != symbolTable.end()) {
            throw std::runtime_error("Variable '" + varDecl->name + "' has already been declared within a parent scope or its current scope!");
        }
        checkAssignment(varDecl, varDecl->value.get());

        llvm::Function* function = llvm::Function::Create(
                llvm::FunctionType::get(llvm::Type::getVoidTy(*context), false),
                llvm::Function::ExternalLinkage,
                "main",
                module.get()
        );
        llvm::BasicBlock* basicBlock = llvm::BasicBlock::Create(*context, "entry", function);
        builder->SetInsertPoint(basicBlock);

        if (varDecl->isConst) {  // Check if it's a constant
            if (varDecl->value == nullptr) {
                throw std::runtime_error("Constant '" + varDecl->name + "' must be initialized!");
            }
            const NumberLiteral* numberLiteral = dynamic_cast<const NumberLiteral*>(varDecl->value.get());
            if (!numberLiteral) {
                throw std::runtime_error("Value for constant variable '" + varDecl->name + "' must be a literal value!");
            }

            llvm::Type* rhsType = determineExpressionType(numberLiteral, type); // Determine the type of it
            llvm::Constant* value = nullptr;  // Initialize value to nullptr

            if (rhsType->isIntegerTy()) {
                value = llvm::ConstantInt::get(*context, llvm::APInt(getBitWidth(rhsType), numberLiteral->getIntValue(), true));
            } else if (rhsType->isFloatTy() || rhsType->isDoubleTy()) {
                value = llvm::ConstantFP::get(*context, llvm::APFloat(numberLiteral->getFloatValue()));
            } else {
                throw std::runtime_error("Unsupported type for constant variable '" + varDecl->name + "'");
            }

            // Create a global constant variable
            llvm::GlobalVariable* globalVar = new llvm::GlobalVariable(
                    *module,
                    type,
                    true,  // isConstant
                    llvm::GlobalValue::InternalLinkage,
                    value,  // Initializer
                    varDecl->name
            );

            // Add the global variable to the symbolTable
            symbolTable[varDecl->name] = globalVar;

        } else {  // Handle non-constant variables
            llvm::AllocaInst* alloca = builder->CreateAlloca(type, nullptr, varDecl->name.c_str()); // Allocates the variable on the stack

            if (varDecl->value) {
                const NumberLiteral* numberLiteral = dynamic_cast<const NumberLiteral*>(varDecl->value.get());
                if (!numberLiteral) {
                    throw std::runtime_error("Value for variable '" + varDecl->name + "' must be a literal value!");
                }

                llvm::Type* rhsType = determineExpressionType(numberLiteral, type); // Determine the type of it
                llvm::Constant* value = nullptr;  // Initialize value to nullptr

                if (rhsType->isIntegerTy()) {
                    value = llvm::ConstantInt::get(*context, llvm::APInt(getBitWidth(rhsType), numberLiteral->getIntValue(), true));
                } else if (rhsType->isFloatTy() || rhsType->isDoubleTy()) {
                    value = llvm::ConstantFP::get(*context, llvm::APFloat(numberLiteral->getFloatValue()));
                } else {
                    throw std::runtime_error("Unsupported type for variable '" + varDecl->name + "'");
                }

                builder->CreateStore(value, alloca);  // Stores the value in the variable  <-- This line is crucial

            } else {
                llvm::Constant* defaultValue = llvm::Constant::getNullValue(type);
                builder->CreateStore(defaultValue, alloca);
            }

            symbolTable[varDecl->name] = alloca; // Adds the variable to the symbolTable hashmap
        }
    }


    llvm::Type* convertToLLVMType(const TypeRepresentation* typeRep, const VariableDeclaration* varDecl) {
        if (const BasicType* basicType = dynamic_cast<const BasicType*>(typeRep)) {
            switch (basicType->type) { // Basic type variables
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
            llvm::Type* elementType = convertToLLVMType(arrayType->elementType.get(), varDecl);
            return llvm::ArrayType::get(elementType, varDecl->size);
        } // TODO: add support for vector types and hashmap types (map and unordered_map)
        throw std::runtime_error("Incorrect type");
    }


    // Variable Declaration
    void checkAssignment(const VariableDeclaration* varDecl, const Expression* rhs) {
        // Convert lhs and rhs types to LLVM types
        llvm::Type* lhsType = convertToLLVMType(varDecl->type.get(), varDecl);
        llvm::Type* rhsType = determineExpressionType(rhs, lhsType);

        // Compare the LLVM type representations
        if (!compareDataTypes(lhsType, rhsType)) {
            throw std::runtime_error("Type mismatch in assignment of variable " + varDecl->name );
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
            default:
                throw std::runtime_error("Unsupported node type in determineExpressionType");
        }
    }
};


#endif //ASTRALANG_ASTRASEMANTICANALYZER_H