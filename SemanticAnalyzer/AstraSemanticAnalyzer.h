//
// Created by Faisal Fakih on 18/10/2023.
//

#ifndef ASTRALANG_ASTRASEMANTICANALYZER_H
#define ASTRALANG_ASTRASEMANTICANALYZER_H

#include "../Parser/AstraParser.h"
#include "../AST/AstraAST.h"

class SemanticAnalyzer {
public:
    SemanticAnalyzer() : m_currentScope(new Scope) {}

    void analyze(const std::vector<std::unique_ptr<ASTNode>>& ast) {
        return;
    }
private:
    Scope* m_currentScope;
};


#endif //ASTRALANG_ASTRASEMANTICANALYZER_H
