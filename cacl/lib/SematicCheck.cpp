#include "SematicCheck.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/raw_ostream.h"
class DeclCheck : public ASTVisitor
{
private:
    enum ErrTpe {
        Multi_Declare,
        Not_Declare
    };
    void error( llvm::StringRef idName, ErrTpe tpe) {
        if (tpe == Not_Declare) {
            llvm::errs() << "Variable " << idName << " is not declared\n";
        } else {
            llvm::errs() << "Variable " << idName << " has already be declared\n";
        }
        errorFlg = true;
    }
    llvm::StringSet<> Scope;
    bool errorFlg;

public:
    DeclCheck():errorFlg(false){};
    ~DeclCheck() {};

    bool hasError() {
        return errorFlg;
    }

    virtual void visit(AST &){};
    virtual void visit(Expr &){};
    virtual void visit(Factor &factor) {
        if (factor.getKind() == Factor::Number) return;

        auto val = factor.getVal();
        if (!Scope.count(val)) {
            error(val, Not_Declare);
        }
    };
    virtual void visit(BinaryOp &binaryOp) {
        auto lExpr = binaryOp.getLeft();
        lExpr->accept(*this);
        if (hasError()) return;
        auto rExpr = binaryOp.getRight();
        rExpr->accept(*this);
    };
    virtual void visit(WithDecl &decl) {
        for(auto I = decl.begin(); I != decl.end(); ++I) {
            auto ret = Scope.insert(*I);
            if(!ret.second) {
                error(*I, Multi_Declare);
                return;
            }
        }
        auto expr = decl.getExpr();
        expr->accept(*this);
    };
};




bool SematicCheck::check(AST *Tree) {
  if (!Tree)
    return false;
  DeclCheck Check;
  Tree->accept(Check);
  return Check.hasError();
}
