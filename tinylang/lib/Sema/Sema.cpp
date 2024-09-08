#include "tinylang/Sema/Sema.h"

using namespace tinylang;

void Sema::initialize()
{
    CurrentScope = new Scope();
    IntegerType = new TypeDeclaration(nullptr, SMLoc(), "INTEGER");
    BooleanType = new TypeDeclaration(nullptr, SMLoc(), "BOOLEAN");
    TrueLiteral = new BooleanLiteral(true, BooleanType);
    FalseLiteral = new BooleanLiteral(false, BooleanType);
    TrueConst = new ConstantDeclaration(nullptr, SMLoc(), "TRUE", TrueLiteral);
    FalseConst = new ConstantDeclaration(nullptr, SMLoc(), "FALSE", FalseLiteral);
    CurrentScope->insert(IntegerType);
    CurrentScope->insert(BooleanType);
    CurrentScope->insert(TrueConst);
    CurrentScope->insert(FalseConst);
}

void Sema::enterScope(Decl * decl)
{
    CurrentScope = new Scope(CurrentScope);
    CurrentDecl = decl;
}
void Sema::leaveScope()
{
    auto parent = CurrentScope->getParent();
    delete CurrentScope;
    CurrentScope = parent;
    CurrentDecl = CurrentDecl->getEnclosingDecl();
}
Expr* Sema::actOnIntegerLiteral(SMLoc Loc,StringRef Literal) 
{
    uint8_t Radix = 10;
    if (Literal.endswith("H")) {
        Literal = Literal.drop_back();
        Radix = 16;
    }
    auto value = llvm::APInt(64, Literal, Radix);
    return new IntegerLiteral(Loc, llvm::APSInt(value, false), IntegerType);
}
bool Sema::isOperatorForType(tok::TokenKind Op,
                             TypeDeclaration *Ty) {
  switch (Op) {
  case tok::plus:
  case tok::minus:
  case tok::star:
  case tok::kw_DIV:
  case tok::kw_MOD:
    return Ty == IntegerType;
  case tok::slash:
    return false; // REAL not implemented
  case tok::kw_AND:
  case tok::kw_OR:
  case tok::kw_NOT:
    return Ty == BooleanType;
  default:
    llvm_unreachable("Unknown operator");
  }
}
Decl *Sema::actOnQualIdentPart(Decl *Prev, SMLoc Loc, StringRef Name) {
    if (Prev == nullptr) {
        Decl *D = CurrentScope->lookup(Name);
        if (D != nullptr) return D;
    } else if (auto *Mod = dyn_cast<ModuleDeclaration>(Prev)) {
        auto Decls = Mod->getDecls();
        for (auto I = Decls.begin(), E = Decls.end(); I != E; ++I) {
            if ((*I)->getName() == Name) {
                return *I;
            }
        }
    } else {
        llvm_unreachable("actOnQualIdentPart only callable with module declarations");
    }
    Diags.report(Loc, diag::err_undeclared_name, Name);
    return nullptr;
}

bool Sema::actOnVariableDeclarationPart(DeclList& decls, const IdentList& idents, Decl* type, const SMLoc& lstLoc) {
    assert(CurrentScope && "current scope should be defined");
    if (TypeDeclaration* typ = dyn_cast<TypeDeclaration>(type)) {
        for (auto I = idents.begin(), E = idents.end(); I != E; ++I) {
            auto decl = new VariableDeclaration(CurrentDecl, I->getLocation(), I->getName(),typ);
            if (CurrentScope->insert(decl)) {
                decls.push_back(decl);
            }else {
                Diags.report(I->getLocation(), diag::err_symbold_declared);
                return false;
            }
        }
        return true;
    } 

    Diags.report(lstLoc, diag::err_vardecl_requires_type);
    return false;
}