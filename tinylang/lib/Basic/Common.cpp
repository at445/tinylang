#include "tinylang/Lexer/Token.h"
#include "llvm/Support/raw_ostream.h"
namespace tinylang {
    raw_ostream& operator<<(raw_ostream& ss, Token& tok) {
        ss << "Token type: " << tok.getKind() << " name: " << tok.getName();
        if (tok.is(tok::identifier)) {
            ss << "  " << tok.getIdentifier();
        } else if (tok.isOneOf(tok::integer_literal, tok::string_literal)) {
            ss << "  " << tok.getLiteralData();
        }
        auto spell = tok.getSpell();
        if(spell != "") {
            ss << "  " << spell;
        }
        ss << "\n";
    }
}
