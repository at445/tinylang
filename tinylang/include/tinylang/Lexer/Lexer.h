#ifndef TINYLANG_LEXER_LEXER_H
#define TINYLANG_LEXER_LEXER_H

#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/LLVM.h"
#include "tinylang/Basic/TokenKinds.h"
#include "tinylang/Lexer/Token.h"
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
    
    class Lexer {
        SourceMgr &SrcMgr;
        DiagnosticsEngine &Diags;

        const char *CurPtr;
        StringRef CurBuf;

        /// CurBuffer - This is the current buffer index we're
        /// lexing from as managed by the SourceMgr object.
        unsigned CurBuffer = 0;

        KeywordFilter Keywords;

        public:
        Lexer(SourceMgr &SrcMgr, DiagnosticsEngine &Diags)
            : SrcMgr(SrcMgr), Diags(Diags) {
            CurBuffer = SrcMgr.getMainFileID();
            CurBuf = SrcMgr.getMemoryBuffer(CurBuffer)->getBuffer();
            CurPtr = CurBuf.begin();
            Keywords.addKeyWords();
        }

        DiagnosticsEngine &getDiagnostics() const {
            return Diags;
        }

        /// Returns the next token from the input.
        void next(Token &Result);

        private:
        void identifierOrKeyword(Token &Result);
        void number(Token &Result);
        void string(Token &Result);
        void comment();

        void consume(void) {
            CurPtr++;
        }

        bool speculateNumber(void);

        SMLoc getLoc() { return SMLoc::getFromPointer(CurPtr); }

        void formToken(Token &Result, const char *TokEnd, tok::TokenKind Kind);
    };  
}
#endif