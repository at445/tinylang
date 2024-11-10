#include "tinylang/Lexer/Token.h"

namespace tinylang {
    raw_ostream& operator<<(raw_ostream& ss, Token& tok) {
        // ss << "Token type: " << tok.getKind() << " name: " << tok.getName();
        // if (tok.is(tok::identifier)) {
        //     ss << "  " << tok.getIdentifier();
        // } else if (tok.isOneOf(tok::integer_literal, tok::string_literal)) {
        //     ss << "  " << tok.getLiteralData();
        // }
        // auto spell = tok.getSpell();
        // if(spell == nullptr) {
        //     ss << "  " << spell;
        // }
        // ss << "\n";
        return ss;
    }
    
    std::string generateTabs(int numTabs) {
        std::string result;
        for (int i = 0; i < numTabs; ++i) {
            result += "\t";
        }
        return result;
    }
}
