#include "tinylang/Sema/Scope.h"
#include "tinylang/AST/AST.h"

using namespace tinylang;

bool Scope::insert(Decl * Decl)
{
    auto kv = Symbols.insert({Decl->getName(), Decl});
    return kv.second;
}

Decl *Scope::lookup(StringRef Name)
{
    auto *S = this;
    while(S != nullptr) {
        auto iter = S->Symbols.find(Name);
        if (iter != S->Symbols.end()) {
            return iter->second;
        }
        S = S->getParent();
    }
    return nullptr;
}
