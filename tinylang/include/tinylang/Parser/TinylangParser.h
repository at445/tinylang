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

        bool parseStatement(StmtList& stmts);
        bool parseActualParams(ExprList &exprs);

        bool parseExpression(Expr *&E);
        bool parseSimpleExpression(Expr *&E);
        bool parsePrefixExpression(Expr *&E);
        bool parseTerm(Expr *&E);
        bool parseFactor(Expr *&E);
        
        bool parseExpressionList(ExprList& exprList);
        bool parseIfStatement(StmtList& stmts);
        bool parseWhileStatement(StmtList& stmts);
        bool parseReturnStatement(StmtList& stmts);
        bool parseFormalParameter(FormalParamList& params);
        bool parseFormalParameterList(FormalParamList& params);
        bool parseFormalParameters(ProcudureDeclaration*& procDecl);


        bool parseStatementSequence(StmtList& stmts);
        bool parseQualident(Decl *&D, SMLoc& lastLoc);
    private:
        OperatorInfo generateOp();
    };
}
#endif
