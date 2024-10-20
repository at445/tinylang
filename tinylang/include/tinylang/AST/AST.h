#pragma once

#include "llvm/Support/raw_ostream.h"
#include "tinylang/Basic/LLVM.h"
#include "tinylang/Basic/TokenKinds.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include <string>
#include <vector>
namespace tinylang {
  class FormalParameterDeclaration;
  class Decl;
  class Stmt;
  class Expr;
  class Ident {
    SMLoc Loc;
    StringRef Name;

  public:
    Ident(SMLoc Loc, const StringRef &Name)
        : Loc(Loc), Name(Name) {}
    SMLoc getLocation() const { return Loc; }
    const StringRef &getName() const { return Name; }
  };

  using IdentList = std::vector<Ident>;
  using DeclList = std::vector<Decl *>;
  using FormalParamList = std::vector<FormalParameterDeclaration *>;
  using ExprList = std::vector<Expr *>;
  using StmtList = std::vector<Stmt *>;

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
    SMLoc Loc;
    StringRef Name;

  public:
    Decl(DeclKind Kind, Decl *EnclosingDecL, SMLoc Loc,StringRef Name)
        : Kind(Kind), EnclosingDecL(EnclosingDecL), Loc(Loc),Name(Name) {}

    DeclKind getKind() const { return Kind; }
    SMLoc getLocation() { return Loc; }
    StringRef getName() { return Name; }
    Decl *getEnclosingDecl() { return EnclosingDecL; }
    virtual void print(void) = 0;
  };

  class TypeDeclaration : public Decl {
  public:
    TypeDeclaration(Decl *EnclosingDecL, SMLoc Loc,
                    StringRef Name)
        : Decl(DK_Type, EnclosingDecL, Loc, Name) {}

    static bool classof(const Decl *D) {
      return D->getKind() == DK_Type; 
    }
    virtual void print(void) override {
      llvm::outs() << Name;
    }
  };

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

    virtual void print(void) = 0;
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
  };

  class ModuleDeclaration : public Decl {
    DeclList Decls;
    StmtList Stmts;

  public:
    ModuleDeclaration(Decl *EnclosingDecL, SMLoc Loc,
                      StringRef Name)
        : Decl(DK_Module, EnclosingDecL, Loc, Name) {}

    ModuleDeclaration(Decl *EnclosingDecL, SMLoc Loc,
                      StringRef Name, DeclList &Decls,
                      StmtList &Stmts)
        : Decl(DK_Module, EnclosingDecL, Loc, Name),
          Decls(Decls), Stmts(Stmts) {}

    const DeclList &getDecls() { return Decls; }
    void setDecls(const DeclList &D) { Decls = D; }
    const StmtList &getStmts() { return Stmts; }
    void setStmts(const StmtList &L) { Stmts = L; }

    static bool classof(const Decl *D) {
      return D->getKind() == DK_Module;
    }
    virtual void print(void) override {
      llvm::outs() << "Module " <<  Name << ";\n";
      for(auto& decl : Decls) {
        llvm::outs() << "\t";
        decl->print();
      }
      llvm::outs() << "End" << ";\n";
    }
};

  class VariableDeclaration : public Decl {
  public:
    TypeDeclaration* type;
    VariableDeclaration(Decl *EnclosingDecL, SMLoc Loc,
                        StringRef Name, TypeDeclaration *Ty)
        : Decl(DK_Var, EnclosingDecL, Loc, Name),
          type(Ty) {}

    TypeDeclaration *getType() { return type; }

    static bool classof(const Decl *D) {
      return D->getKind() == DK_Var; 
    }
    virtual void print(void) override {
      llvm::outs() << "VAR " << Name << ": ";
      type->print();
      llvm::outs() << ";\n";
    }
  };

  

  class ConstantDeclaration : public Decl {
    Expr *E;

  public:
    ConstantDeclaration(Decl *EnclosingDecL, SMLoc Loc,
                        StringRef Name, Expr *E)
        : Decl(DK_Const, EnclosingDecL, Loc, Name), E(E) {}

    Expr *getExpr() { return E; }

    static bool classof(const Decl *D) {
      return D->getKind() == DK_Const;
    }
    virtual void print(void) override {
      llvm::outs() << "CONST " << Name << " = ";
      E->print();
      llvm::outs() << ";\n";
    }
  };

  class FormalParameterDeclaration : public Decl {
    TypeDeclaration *Ty;
    bool IsVar;

  public:
    FormalParameterDeclaration(Decl *EnclosingDecL, SMLoc Loc,
                              StringRef Name,
                              TypeDeclaration *Ty,
                              bool IsVar)
        : Decl(DK_Param, EnclosingDecL, Loc, Name), Ty(Ty),
          IsVar(IsVar) {}

    TypeDeclaration *getType() { return Ty; }
    bool isVar() { return IsVar; }

    static bool classof(const Decl *D) {
      return D->getKind() == DK_Param;
    }
  };

  
  class BooleanLiteral : public Expr {
    bool Value;

  public:
    BooleanLiteral(bool Value, TypeDeclaration *Ty)
        : Expr(EK_Bool, Ty, true), Value(Value) {}
    bool getValue() { return Value; }

    static bool classof(const Expr *E) {
      return E->getKind() == EK_Bool;
    }
    virtual void print(void) override {
      llvm::outs() << (Value ? "True" : "False");
    }
  };

  class IntegerLiteral : public Expr {
    SMLoc Loc;
    llvm::APSInt Value;

  public:
    IntegerLiteral(SMLoc Loc, const llvm::APSInt &Value,
                  TypeDeclaration *Ty)
        : Expr(EK_Int, Ty, true), Loc(Loc), Value(Value) {}

    llvm::APSInt &getValue() { return Value; }

    static bool classof(const Expr *E) {
      return E->getKind() == EK_Int;
    }
    virtual void print(void) override {
      llvm::outs() << Value;
    }
  };

  class OperatorInfo {
    SMLoc Loc;
    uint32_t Kind : 16;

  public:
    OperatorInfo()
        : Loc(), Kind(tok::unknown) {}
    OperatorInfo(SMLoc Loc, tok::TokenKind Kind)
        : Loc(Loc), Kind(Kind) {
    }

    SMLoc getLocation() const { return Loc; }
    tok::TokenKind getKind() const {
      return static_cast<tok::TokenKind>(Kind);
    }
  };
  class InfixExpression : public Expr {
    Expr *Left;
    Expr *Right;
    const OperatorInfo Op;

    public:
      InfixExpression(Expr *Left, Expr *Right, OperatorInfo Op,
                      TypeDeclaration *Ty, bool IsConst)
          : Expr(EK_Infix, Ty, IsConst), Left(Left),
            Right(Right), Op(Op) {}

      Expr *getLeft() { return Left; }
      Expr *getRight() { return Right; }
      const OperatorInfo &getOperatorInfo() { return Op; }

      static bool classof(const Expr *E) {
        return E->getKind() == EK_Infix;
      }
      virtual void print(void) override {
        llvm::outs() << "(";
        Left->print();
        llvm::outs() << " " << tok::getSpelling(Op.getKind())  << " ";
        Right->print();
        llvm::outs() << ")";
      }
  };
  class PrefixExpression : public Expr {
    Expr *E;
    const OperatorInfo Op;

  public:
    PrefixExpression(Expr *E, OperatorInfo Op,
                    TypeDeclaration *Ty, bool IsConst)
        : Expr(EK_Prefix, Ty, IsConst), E(E), Op(Op) {}

    Expr *getExpr() { return E; }
    const OperatorInfo &getOperatorInfo() { return Op; }

    static bool classof(const Expr *E) {
      return E->getKind() == EK_Prefix;
    }
    virtual void print(void) override {
       llvm::outs() << "(" << tok::getSpelling(Op.getKind()) << " ";
       E->print();
       llvm::outs() << ")";
    }
  };
  class VariableAccess : public Expr {
    Decl *Var;

  public:
    VariableAccess(VariableDeclaration *Var)
        : Expr(EK_Var, Var->getType(), false), Var(Var) {}
    VariableAccess(FormalParameterDeclaration *Param)
        : Expr(EK_Var, Param->getType(), false), Var(Param) {}

    Decl *getDecl() { return Var; }

    static bool classof(const Expr *E) {
      return E->getKind() == EK_Var;
    }

    virtual void print(void) override {
      llvm::outs() << Var->getName();
    }
  };

  class ConstantAccess : public Expr {
    ConstantDeclaration *Const;

  public:
    ConstantAccess(ConstantDeclaration *Const)
        : Expr(EK_Const, Const->getExpr()->getType(), true),
          Const(Const) {}

    ConstantDeclaration *geDecl() { return Const; }

    static bool classof(const Expr *E) {
      return E->getKind() == EK_Const;
    }

    virtual void print(void) override {
      llvm::outs() << Const->getName();
    }
  };

  
}
