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
void Sema::enterScope(Decl *decl)
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
    if (Literal.ends_with("H")) {
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
  case tok::slash:
    return Ty == IntegerType;
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

Expr * Sema::actOnPrefixedExpr(Expr *expr, OperatorInfo &op)
{
    if (!expr) return nullptr;
    if (!isOperatorForType(op.getKind(), expr->getType())) {
        Diags.report(
            op.getLocation(),
            diag::err_types_for_operator_not_compatible,
            tok::getSpelling(op.getKind()));
    }
    if (expr->isConst() && op.getKind() == tok::kw_NOT) {
        if (auto *L = dyn_cast<BooleanLiteral>(expr)) {
            return L->getValue() ? FalseLiteral : TrueLiteral;
        } 
    }

    if (op.getKind() == tok::minus) {
        bool Ambiguous = true;
        if (isa<IntegerLiteral>(expr) || isa<VariableAccess>(expr) || 
            isa<ConstantAccess>(expr) || isa<PrefixExpression>(expr))
            Ambiguous = false;
        else if (auto *Infix = dyn_cast<InfixExpression>(expr)) {
        tok::TokenKind Kind = Infix->getOperatorInfo().getKind();
        if (Kind == tok::star || Kind == tok::slash)
            Ambiguous = false;
        }
        if (Ambiguous) {
            Diags.report(op.getLocation(), diag::warn_ambigous_negation);
        }
    }

    return new PrefixExpression(expr, std::move(op), expr->getType(), expr->isConst());
}

bool Sema::actOnSimpleExpr(Expr *&ret, Expr *lExpr, Expr *rExpr, OperatorInfo &op)
{
    if ((lExpr->getType() != rExpr->getType()) ||
        (!isOperatorForType(op.getKind(), lExpr->getType()))) {
        Diags.report(op.getLocation(), diag::err_types_for_operator_not_compatible);
        return false;
    }
    bool isConst = lExpr->isConst() && rExpr->isConst();
    ret = new InfixExpression(lExpr, rExpr, std::move(op), lExpr->getType(), isConst);
    return true;
}

bool Sema::actOnTerm(const SMLoc& Loc, const OperatorInfo &op, Expr *&ret, Expr *lExpr, Expr *rExpr)
{

    if (!isOperatorForType(op.getKind(), lExpr->getType())) {
        Diags.report(Loc, diag::err_types_for_operator_not_compatible, 
                    lExpr->getType()->getName(), 
                    tok::getSpelling(op.getKind()));
        return false;
    }
    bool isConst = lExpr->isConst() && rExpr->isConst();
    if (isConst) {
        auto l = dyn_cast<BooleanLiteral>(lExpr);
        auto r = dyn_cast<BooleanLiteral>(rExpr);
        if (l && r) {
            if (op.getKind() == tok::kw_AND) {
                ret = (l->getValue() && r->getValue()) ? TrueLiteral : FalseLiteral;
            } else if (op.getKind() == tok::kw_OR) {
                ret = (l->getValue() || r->getValue()) ? TrueLiteral : FalseLiteral;
            }
            return true;
        }
    }

    ret = new InfixExpression(lExpr, rExpr, std::move(op), lExpr->getType(), isConst);
    return true;
}
bool Sema::actOnVariableDeclarationPart(DeclList &decls, const IdentList &idents, Decl *type, const SMLoc &lstLoc)
{
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
bool Sema::actOnConstantDecl(DeclList& decls, SMLoc Loc, StringRef Name, Expr *E)
{
    assert(CurrentScope && "current scope should be defined");
    auto decl = new ConstantDeclaration(CurrentDecl, Loc, Name, E);
    if (!CurrentScope->insert(decl)) {
        Diags.report(Loc, diag::err_symbold_declared, Name);
        return false;
    }
    if (!E->isConst()) {
        Diags.report(Loc, diag::err_not_const_for_const_declaration, Name);
        return false;
    }
    decls.push_back(decl);
    return true;
}
bool Sema::actOnAccess(SMLoc Loc, StringRef Name, Expr *&E) {
    assert(CurrentScope && "current scope should be defined");
    auto decl = CurrentScope->lookup(Name);
    if (!decl) {
        Diags.report(Loc, diag::err_undeclared_name, Name);
        return false;
    }
    if (auto *V = dyn_cast<VariableDeclaration>(decl)) {
        E = new VariableAccess(V);
    } else if (auto *V = dyn_cast<FormalParameterDeclaration>(decl)) {
        E = new VariableAccess(V);
    } else if (auto *V = dyn_cast<ConstantDeclaration>(decl)) {
        if (V == TrueConst)
            E = TrueLiteral;
        else if (V == FalseConst) 
            E = FalseLiteral;
        else 
            E = new ConstantAccess(V);
    }
    return true;
}