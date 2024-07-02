#include "tinylang/Lexer/Lexer.h"
using namespace tinylang;
void KeywordFilter::addKeyWords(void)
{
    for (size_t i = tok::KW_ENUM_START; i <= tok::KW_ENUM_END; i++)
    {
        tok::TokenKind kind = static_cast<tok::TokenKind>(i);
        keywords.insert({tok::getTokenName(kind), kind});
    }
    
}

tok::TokenKind KeywordFilter::getKeyWords(llvm::StringRef key, tok::TokenKind default_kind)
{
    auto iter = keywords.find(key);
    if (iter != keywords.end()) {
        return iter->second;
    }
    return default_kind;
}
