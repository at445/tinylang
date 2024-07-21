#ifndef TINYLANG_LEXER_TOKEN_H
#define TINYLANG_LEXER_TOKEN_H

#include "tinylang/Basic/LLVM.h"
#include "tinylang/Basic/TokenKinds.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace tinylang {
    class Lexer;
    class Token {
        friend class Lexer;
        const char * Ptr;

        size_t Length;

        tok::TokenKind kind;

    public:
        tok::TokenKind getKind() const { return kind; }
        void setKind(tok::TokenKind K) { kind = K; }

        bool is(tok::TokenKind K) const { return kind == K; }
        bool isNot(tok::TokenKind K) const { return kind != K; }

        bool isOneOf(tok::TokenKind K1, tok::TokenKind K2) const {
            return is(K1) || is(K2);
        }

        template <typename... Ts>
        bool isOneOf(tok::TokenKind K1, tok::TokenKind K2, Ts... Ks) const {
            return is(K1) || isOneOf(K2, Ks...);
        }


        const char *getName() const {
            return tok::getTokenName(kind);
        }

        const char *getSpell() const {
            return tok::getSpelling(kind);
        }
        SMLoc getLocation() const {
            return SMLoc::getFromPointer(Ptr);
        }

        size_t getLength() const {
            return Length;
        }

        StringRef getIdentifier() const {
            assert(is(tok::identifier) && "cannot obtain String from a non-identifiter");
            return StringRef(Ptr, Length);
        }

        StringRef getLiteralData() const {
            assert(isOneOf(tok::integer_literal, tok::string_literal) && "cannot obtain String from a non-identifiter");
            return StringRef(Ptr, Length);
        }

        friend raw_ostream& operator << (raw_ostream& ss, Token& tok);
    };
}
#endif