#pragma once

#include "tinylang/AST/AST.h"
#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Sema/Scope.h"
#include <memory>

namespace tinylang {
    class Sema {
        friend class EnterDeclScope;
        void enterScope(Decl *);
        void leaveScope();
        Scope *CurrentScope;
        Decl *CurrentDecl;
        DiagnosticsEngine &Diags;

        TypeDeclaration *IntegerType;
        TypeDeclaration *BooleanType;
        BooleanLiteral *TrueLiteral;
        BooleanLiteral *FalseLiteral;
        ConstantDeclaration *TrueConst;
        ConstantDeclaration *FalseConst;
    public:
        Sema(DiagnosticsEngine &Diags)
            : CurrentScope(nullptr), CurrentDecl(nullptr),
                Diags(Diags) {
            initialize();
        }
        void initialize();

        Decl *getCurrentDecl() {return CurrentDecl;}
        
        Expr* actOnIntegerLiteral(SMLoc Loc,StringRef Literal);
        Decl* actOnQualIdentPart(Decl *Prev, SMLoc Loc, StringRef Name);
        Expr* actOnPrefixedExpr(Expr *expr, OperatorInfo &op);
        bool actOnSimpleExpr(Expr *&ret, Expr *lExpr, Expr *rExpr, OperatorInfo &op);
        bool actOnTerm(const SMLoc& Loc, const OperatorInfo &op, Expr *&ret, Expr *lExpr, Expr *rExpr);
        
        bool actOnConstantDecl(DeclList& decls, SMLoc Loc, StringRef Name, Expr *E);
        bool actOnVariableDeclarationPart(DeclList& decls, const IdentList& ids, Decl* type, const SMLoc& lstLoc);
        bool actOnFormalParameter(FormalParamList &decls, const IdentList &identList, Decl *type, bool isVar, const SMLoc& lstLoc);
        bool isOperatorForType(tok::TokenKind Op,TypeDeclaration *Ty);

        bool actOnAccess(SMLoc Loc, StringRef Name, Expr *&E);
    };

    class EnterDeclScope {
        Sema &Semantics;

    public:
        EnterDeclScope(Sema &Semantics, Decl *D)
            : Semantics(Semantics) {
            Semantics.enterScope(D);
        }
        ~EnterDeclScope() { Semantics.leaveScope(); }
    };
}