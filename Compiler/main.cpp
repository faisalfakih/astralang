#include <chrono>
#include <fstream>
#include <iostream>
#include <sstream>
#include "../Parser/AstraParser.h"
#include "../SemanticAnalyzer/AstraSemanticAnalyzer.h"

#define GREEN "\033[32m"
#define RED "\033[31m"
#define BLUE "\033[34m"
#define RESET "\033[0m"
#define LOG(x) std::cout << x << std::endl;


int main() {
    // Test
    auto start = std::chrono::high_resolution_clock::now();  // Record start time
    std::ifstream file("../Compiler/test.astra");
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
    std::ios_base::sync_with_stdio(false);
    Parser* parser = new Parser(Lexer(input));
    std::vector<std::unique_ptr<ASTNode>> parseTree = parser->parse();
    SemanticAnalyzer* semanticAnalyzer = new SemanticAnalyzer();
    semanticAnalyzer->analyze(parseTree);
    delete parser;
    auto end = std::chrono::high_resolution_clock::now();  // Record end time

    std::chrono::duration<double> time_taken = end - start;  // Compute the difference
    LOG(GREEN << "Code works!\n" << BLUE << "Time taken: " << time_taken.count() * 1000000  << std::setprecision(20) << " microseconds" << RESET);
    return 0;
}