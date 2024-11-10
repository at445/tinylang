#pragma once
#include <numeric>
#include "llvm/Support/raw_ostream.h"
#include "tinylang/AST/Base.h"
#include "tinylang/Basic/LLVM.h"
#include "tinylang/Basic/TokenKinds.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace tinylang {
  class Ident;
  class FormalParameterDeclaration;
  using IdentList = std::vector<Ident>;
  using DeclList = std::vector<Decl *>;
  using FormalParamList = std::vector<FormalParameterDeclaration *>;
  using ExprList = std::vector<Expr *>;
  using StmtList = std::vector<Stmt *>;
  std::string generateTabs(int numTabs);

  template <typename T>
  std::string Concat(const std::vector<T>& vecs, int numHuriTab = 1) {
      std::vector<std::string> declsStr;
      declsStr.reserve(vecs.size());
      for (size_t i = 0; i < vecs.size(); i++) {
        std::string outputString;
        llvm::raw_string_ostream rss(outputString);
        vecs[i]->print(rss);
        rss.flush();
        declsStr.push_back(std::move(outputString));
      }
      std::size_t idx =0;
      std::string result = std::accumulate(declsStr.begin(), declsStr.end() , generateTabs(numHuriTab), 
        [&idx, &declsStr, numHuriTab](std::string& ret, const std::string& b) {
          ret.append(b);
          bool isLastElement = (idx == declsStr.size() - 1);
          if (!isLastElement) {
            ret.append("\n");
            ret.append(generateTabs(numHuriTab));
        }
          idx++;
          return ret;
        });
      return result;
  };

  class Ident {
    SMLoc Loc;
    StringRef Name;

  public:
    Ident(SMLoc Loc, const StringRef &Name)
        : Loc(Loc), Name(Name) {}
    SMLoc getLocation() const { return Loc; }
    const StringRef &getName() const { return Name; }
  };


  
  class TypeDeclaration : public Decl {
  public:
    TypeDeclaration(Decl *EnclosingDecL, SMLoc Loc,
                    StringRef Name)
        : Decl(DK_Type, EnclosingDecL, Loc, Name) {}

    static bool classof(const Decl *D) {
      return D->getKind() == DK_Type; 
    }
    virtual void print(llvm::raw_ostream & rawStream) override {
      rawStream << Name;
    }
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
    virtual void print(llvm::raw_ostream & rawStream = llvm::outs()) override {
      rawStream << "Module " <<  Name << ";\n";
      rawStream << Concat(Decls);
      rawStream << "\nEnd" << ";\n";
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
    virtual void print(llvm::raw_ostream & rawStream) override {
      rawStream << "VAR " << Name << ": ";
      type->print(rawStream);
      rawStream << ";";
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
    virtual void print(llvm::raw_ostream & rawStream) override {
      rawStream << "CONST " << Name << " = ";
      E->print(rawStream);
      rawStream << ";";
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
    virtual void print(llvm::raw_ostream & rawStream) override {
      rawStream << Name << ": ";
      Ty->print(rawStream);
      rawStream << ", ";
    }
  };

  class ProcudureDeclaration : public Decl {
    TypeDeclaration *Ty;
    FormalParamList FormalPara;
    DeclList Decls;
    StmtList Stmts;
  private:
    void removeLastNCharacters(std::string &inputString, size_t n = 2) {
      if (inputString.length() >= n) {
          inputString = inputString.substr(0, inputString.length() - n);
      }
    }
  public:
    ProcudureDeclaration(Decl *EnclosingDecL, SMLoc Loc, StringRef Name)
      :Decl(DK_Proc, EnclosingDecL, Loc, Name) {}
    void setReturnType(TypeDeclaration * typ) {
      Ty = typ;
    }
    TypeDeclaration *getReturnType() {
      return Ty;
    }
    void addFormalParam(FormalParameterDeclaration* param) {
      FormalPara.push_back(param);
    }

    const StmtList& getStmts() {
      return Stmts;
    }
    void addStmt(Stmt* stmt) {
      Stmts.push_back(stmt);
    }
    void concatStmts(StmtList& stmts) {
      Stmts.insert(Stmts.end(), stmts.begin(), stmts.end());
    }

    const DeclList& getDecls() {
      return Decls;
    }
    void addStmt(Decl* decl) {
      Decls.push_back(decl);
    }
    void concatStmts(DeclList& decls) {
      Decls.insert(Decls.end(), decls.begin(), decls.end());
    }
    static bool classof(const Decl *D) {
      return D->getKind() == DK_Proc;
    }
    virtual void print(llvm::raw_ostream & rawStream = llvm::outs()) override {
      rawStream << "\tPROCEDURE " <<  Name << "( ";
      std::for_each(FormalPara.begin(), FormalPara.end(), [&rawStream](auto para){para->print(rawStream);});
      rawStream << " ) : ";
      Ty->print(rawStream);
      rawStream << ";\n";
      rawStream << Concat(Decls, 2);
      rawStream << "\n\tBEGIN";
      rawStream << Concat(Stmts, 2);
      rawStream << "\n\tEND;\n";
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
    virtual void print(llvm::raw_ostream & rawStream) override {
      rawStream << (Value ? "True" : "False");
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
    virtual void print(llvm::raw_ostream & rawStream) override {
      rawStream << Value;
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
      virtual void print(llvm::raw_ostream & rawStream) override {
        rawStream << "(";
        Left->print(rawStream);
        rawStream << " " << tok::getSpelling(Op.getKind())  << " ";
        Right->print(rawStream);
        rawStream << ")";
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
    virtual void print(llvm::raw_ostream & rawStream) override {
      rawStream << "(" << tok::getSpelling(Op.getKind()) << " ";
      E->print(rawStream);
      rawStream << ")";
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

    virtual void print(llvm::raw_ostream & rawStream) override {
      rawStream << Var->getName();
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

    virtual void print(llvm::raw_ostream & rawStream) override {
      rawStream << Const->getName();
    }
  };

    
}
