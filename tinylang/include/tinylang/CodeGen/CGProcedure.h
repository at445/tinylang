#include "tinylang/AST/AST.h"
#include "tinylang/CodeGen/CGModule.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
namespace tinylang {
  class CGProcedure {
    CGModule& m_cgModule;
    llvm::IRBuilder<> m_Builder;
    llvm::BasicBlock *m_CurrBB;

    llvm::FunctionType *m_FuncTy;
    llvm::Function *m_Func;
    struct BasicBlockDef {
      llvm::DenseMap<Decl *, llvm::TrackingVH<llvm::Value>> Defs;
      llvm::DenseMap<llvm::PHINode *, Decl *> IncompletePhis;
      unsigned Sealed : 1;

      BasicBlockDef() : Sealed(0) {}
    }
    llvm::DenseMap<llvm::BasicBlock*, BasicBlockDef> m_Defs;
  public:
    CGProcedure(const CGModule& mod)
    : m_cgModule(mod) {

    }
    void emitStmt(WhileStatement *Stmt);

    void emitStmt(StmtList& stmts);

    void setCurrBB(llvm::BasicBlock *BB) {
      m_CurrBB = BB;
      m_Builder.SetInsertPoint(BB);
    }
    void writeLocalVariable(llvm::BasicBlock *BB, Decl *decl, llvm::Value * val);
    llvm::Value * readLocalVariable(llvm::BasicBlock *BB, Decl *decl);
    llvm::Value * readLocalVariableRecursive(llvm::BasicBlock *BB, Decl *decl);
    llvm::Value emitExpr(Expr* expr);
  private:
    llvm::PHINode *addEmptyPhi(llvm::BasicBlock *BB, Decl* decl);
    void addPhiOperands(llvm::BasicBlock *BB, Decl *decl, llvm::PHINode *Phi);
    void sealBlock(llvm::BasicBlock *BB);
  }
}
