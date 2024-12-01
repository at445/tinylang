#include "tinylang/CodeGen/CGProcedure.h"
#include "CGProcedure.h"

void CGProcedure::emitStmt(WhileStatement *Stmt)
{
  llvm::BasicBlock *WhileCondBB = llvm::BasicBlock::Create(
    m_cgModule.getLLVMCtx(), "while.cond", m_Func);
  llvm::BasicBlock *WhileBody = llvm::BasicBlock::Create(
    m_cgModule.getLLVMCtx(), "while.body", m_Func);
  llvm::BasicBlock *AfterWhileBB = llvm::BasicBlock::Create(
    m_cgModule.getLLVMCtx(), "after.while", m_Func);

  m_Builder.CreateBr(WhileCondBB);
  setCurrBB(WhileCondBB);
  llvm::Value *Cond = emitExpr(Stmt->getCondExpr());

  m_Builder.CreateCondBr(Cond, WhileBody, AfterWhileBB);
  setCurrBB(WhileBody);
  emitStmt(Stmt->getStmts());

  setCurrBB(AfterWhileBB);
}
void tinylang::CGProcedure::emitStmt(StmtList &stmts)
{
}
lvm::Value tinylang::CGProcedure::emitExpr(Expr *expr)
{
  return llvm::Value();
}

llvm::PHINode *tinylang::CGProcedure::addEmptyPhi(llvm::BasicBlock *BB, Decl *decl)
{
  if (BB->empty()) {
    return llvm::PHINode::Create(mapType(decl), 0, "", BB);
  } else {
    return llvm::PHINode::Create(mapType(decl), 0, "", BB->front());
  }
}

void tinylang::CGProcedure::addPhiOperands(llvm::BasicBlock *BB, Decl *decl, llvm::PHINode *Phi)
{
  for (auto I = llvm::pred_begin(BB), E = llvm::pred_end(BB); I != E; ++I) {
    Phi->addIncoming(readLocalVariable(*I, decl), *I);
  }
}

void tinylang::CGProcedure::sealBlock(llvm::BasicBlock *BB)
{
  for (auto phiDecl : m_Defs[BB].IncompletePhis) {
    addPhiOperands(BB, phiDecl.second, phiDecl.first);
  }
  m_Defs[BB].IncompletePhis.clear();
  m_Defs[BB].Sealed = true;
}

void tinylang::CGProcedure::writeLocalVariable(llvm::BasicBlock *BB, Decl *decl, llvm::Value * val) {
  m_Defs[BB].Defs[decl] = val;
}

llvm::Value *tinylang::CGProcedure::readLocalVariable(llvm::BasicBlock *BB, Decl *decl)
{
  auto iter = m_Defs[BB].Defs.find(decl);
  if(iter != m_Defs[BB].Defs.end()) return iter->second;
  return readLocalVariableRecursive(BB, decl);
}

llvm::Value *tinylang::CGProcedure::readLocalVariableRecursive(llvm::BasicBlock *BB, Decl *decl)
{
  llvm::Value *val = nullptr;
  
  if (!m_Defs[BB].Sealed) { //当前还不存在
    llvm::PHINode * Phi = addEmptyPhi(BB, decl);
    m_Defs[BB].IncompletePhis[Phi] = decl;
    val = Phi;
  } else if(auto *PredBB = BB->getSignedPredicate()) { //只有一个前驱
    val = readLocalVariable(PredBB, decl);
  } else { //有多个前驱
    llvm::PHINode * Phi = addEmptyPhi(BB, decl);
    val = Phi;
    //writeLocalVariable(BB, decl, val);
    addPhiOperands(BB, decl, Phi);
  }
  writeLocalVariable(BB, decl, val);
}
