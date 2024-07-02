#ifndef TINYLANG_LEXER_LEXER_H
#define TINYLANG_LEXER_LEXER_H

#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/LLVM.h"
#include "tinylang/Basic/TokenKinds.h"
// #include "tinylang/Lexer/Token.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"

namespace tinylang {
    class KeywordFilter {
        llvm::StringMap<tok::TokenKind> keywords;
    public:
        void addKeyWords(void);
        tok::TokenKind getKeyWords(llvm::StringRef key, tok::TokenKind default_kind = tok::unknown);
    };
}
#endif