#pragma once

#include "llvm/Support/SMLoc.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"
namespace tinylang {
    class Decl {
    public:
        enum DeclKind {
        DK_Module,
        DK_Const,
        DK_Type,
        DK_Var,
        DK_Param,
        DK_Proc
        };

    private:
        DeclKind Kind;

    protected:
        Decl *EnclosingDecL;
        llvm::SMLoc Loc;
        llvm::StringRef Name;

    public:
        Decl(DeclKind Kind, Decl *EnclosingDecL, llvm::SMLoc Loc, llvm::StringRef Name)
            : Kind(Kind), EnclosingDecL(EnclosingDecL), Loc(Loc),Name(Name) {}

        DeclKind getKind() const { return Kind; }
        llvm::SMLoc getLocation() { return Loc; }
        llvm::StringRef getName() { return Name; }
        Decl *getEnclosingDecl() { return EnclosingDecL; }
        virtual void print(llvm::raw_ostream & rawStream) = 0;
    };
    class TypeDeclaration;
    class Expr {
    public:
        enum ExprKind {
            EK_Infix,
            EK_Prefix,
            EK_Int,
            EK_Bool,
            EK_Var,
            EK_Const,
            EK_Func,
        };

    private:
        const ExprKind Kind;
        TypeDeclaration *Ty;
        bool IsConstant;

    protected:
        Expr(ExprKind Kind, TypeDeclaration *Ty, bool IsConst)
            : Kind(Kind), Ty(Ty), IsConstant(IsConst) {}

    public:
        ExprKind getKind() const { return Kind; }
        TypeDeclaration *getType() { return Ty; }
        void setType(TypeDeclaration *T) { Ty = T; }
        bool isConst() { return IsConstant; }

        virtual void print(llvm::raw_ostream & rawStream) = 0;
    };
    class Stmt {
    public:
        enum StmtKind {
        SK_Assign,
        SK_ProcCall,
        SK_If,
        SK_While,
        SK_Return
        };

    private:
        StmtKind Kind;

    protected:
        Stmt(StmtKind Kind) : Kind(Kind) {}

    public:
        StmtKind getKind() const { return Kind; }
        virtual void print(llvm::raw_ostream & rawStream) = 0;
    };
}