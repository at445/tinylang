#include "tinylang/CodeGen/CGModule.h"

tinylang::CGModule::CGModule(llvm::Module *mod)
: M(mod)
{
  VoidTy = llvm::Type::getVoidTy(getLLVMCtx());
  Int1Ty = llvm::Type::getInt1Ty(getLLVMCtx());
  Int32Ty = llvm::Type::getInt32Ty(getLLVMCtx());
  Int64Ty = llvm::Type::getInt64Ty(getLLVMCtx());
  Int32Zero = llvm::Constant::get(Int32Ty, 0, /*isSigned*/ true);
}