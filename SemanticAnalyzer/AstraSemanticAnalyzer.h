//
// Created by Faisal Fakih on 18/10/2023.
//

#ifndef ASTRALANG_ASTRASEMANTICANALYZER_H
#define ASTRALANG_ASTRASEMANTICANALYZER_H

#include "../Parser/AstraParser.h"
#include "../AST/AstraAST.h"
#include <unordered_map>
#include <memory>
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
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
        for (const auto& node : parseTree) {
            analyzeNode(node.get());
        }
    }
private:
    std::unique_ptr<llvm::LLVMContext> context; // This is a container for all the LLVM IR
    std::unique_ptr<llvm::Module> module; // This is a container for all the LLVM IR
    std::unique_ptr<llvm::IRBuilder<>> builder; // This is a builder for LLVM IR
    std::unordered_map<std::string, llvm::Value*> namedValues;
    std::unordered_map<std::string, llvm::AllocaInst*> symbolTable;
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
                auto varDecl = dynamic_cast<const VariableDeclaration*>(node);
                declareVariable(varDecl);
                break;
            }
            default:
                throw std::runtime_error("ERROR ENCOUNTERED");
        }
    }

    void declareVariable(const VariableDeclaration* varDecl) {
        llvm::Type* type = convertToLLVMType(varDecl->type.get());
        if (symbolTable.find(varDecl->name) != symbolTable.end()) {
            throw std::runtime_error("Variable '" + varDecl->name + "' has already been declared within a parent scope or its current scope!");
        }

        llvm::Function* function = llvm::Function::Create(
                llvm::FunctionType::get(llvm::Type::getVoidTy(*context), false),
                llvm::Function::ExternalLinkage,
                "main",
                module.get()
        );
        llvm::BasicBlock* basicBlock = llvm::BasicBlock::Create(*context, "entry", function);
        builder->SetInsertPoint(basicBlock);

        llvm::AllocaInst* alloca = builder->CreateAlloca(type, nullptr, varDecl->name.c_str()); // Allocates the variable on the stack
        symbolTable[varDecl->name] = alloca; // Adds the variable to the symbolTable hashmap
    }


    llvm::Type* convertToLLVMType(const TypeRepresentation* typeRep) {
        if (const BasicType* basicType = dynamic_cast<const BasicType*>(typeRep)) {
            switch (basicType->type) { // Basic type variables
                case BasicType::Type::INT:
                    return llvm::Type::getInt32Ty(*context);
                case BasicType::Type::LONG:
                    return llvm::Type::getInt64Ty(*context);
                case BasicType::Type::SHORT:
                    return llvm::Type::getInt8Ty(*context);
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
        }
        throw std::runtime_error("Incorrect type");
    }
};


#endif //ASTRALANG_ASTRASEMANTICANALYZER_H