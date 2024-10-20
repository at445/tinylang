#ifndef TINYLANG_PARSER_H
#define TINYLANG_PARSER_H

#include "tinylang/AST/AST.h"
#include "tinylang/Sema/Sema.h"
#include "tinylang/Parser/ParserBase.h"

namespace tinylang {
    class Parser : public ParserBase {
        Sema &Semantic;
    public:
        void clearMemo(void) override {
        }
        Parser(Lexer &lexer, Sema &semantic, DiagnosticsEngine & diag) 
            :ParserBase(lexer, diag),
            Semantic(semantic){};

        bool parseCompilationUnit(ModuleDeclaration *& module);

    private:
        bool parseImport(void);
        bool parseIdentList(IdentList&);

        bool parseBlock(DeclList&, StmtList&);

        bool parseDeclaration(DeclList& decls);
        bool parseConstantDeclaration(DeclList& decls);
        bool parseVariableDeclaration(DeclList& decls);
        bool parseProcedureDeclaration(DeclList& decls);

        bool parseStatement();
        bool parseActualParams();

        bool parseExpression(Expr *&E);
        bool parseSimpleExpression(Expr *&E);
        bool parsePrefixExpression(Expr *&E);
        bool parseprefixedExpression(Expr *&E);
        bool parseTerm(Expr *&E);
        bool parseFactor(Expr *&E);
        
        bool parseExpressionList();
        bool parseIfStatement();
        bool parseWhileStatement();
        bool parseReturnStatement();
        bool parseFormalParameter();
        bool parseFormalParameterList();
        bool parseFormalParameters();


        bool parseStatementSequence();
        bool parseQualident(Decl *&D, SMLoc& lastLoc);
    private:
        OperatorInfo generateOp();
    };
}
#endif
