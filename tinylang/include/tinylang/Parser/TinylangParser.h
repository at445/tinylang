#ifndef TINYLANG_PARSER_H
#define TINYLANG_PARSER_H

#include "tinylang/Parser/ParserBase.h"

namespace tinylang {
    class Parser : public ParserBase {
    public:
        void clearMemo(void) override {
            memoization.clear();
        }
        Parser(Lexer &lexer, DiagnosticsEngine & diag) 
            :ParserBase(lexer, diag){}

        bool parseCompilationUnit(void);

    private:
        // Structure for speculation
        // key: start Position among buffer
        // value: end Position among buffer 
        //          means how far the speculation has reached
        //          if this is -1, means speculate process is usless
        llvm::DenseMap<int, int> memoization;

        bool parseImport(void);
        bool parseIdentList(void);

        bool parseBlock(void);

        bool parseDeclaration();
        bool parseConstantDeclaration();
        bool parseVariableDeclaration();
        bool parseProcedureDeclaration();

        bool parseStatement();
        bool parseActualParams();

        bool parseExpression();
        bool parseprefixedExpression();
        bool parseTerm();
        bool parseFactor();
        
        bool parseExpressionList();
        bool parseIfStatement();
        bool parseWhileStatement();
        bool parseReturnStatement();
        bool parseFormalParameter();
        bool parseFormalParameterList();
        bool parseFormalParameters();


        bool parseStatementSequence();
        bool parseQualident(void);
    };
}
#endif
