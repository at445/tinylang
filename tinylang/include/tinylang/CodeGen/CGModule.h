#include "tinylang/AST/AST.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

namespace tinylang {
  class CGModule {
    llvm::Module *M;

  public:
    llvm::Type *VoidTy;
    llvm::Type *Int1Ty;
    llvm::Type *Int32Ty;
    llvm::Type *Int64Ty;
    llvm::Constant *Int32Zero;
  public:
    CGModule(llvm::Module *mod);
    llvm::LLVMContext &getLLVMCtx() {return M->getContext();}
  }
}