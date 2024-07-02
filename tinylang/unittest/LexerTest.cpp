#include <gtest/gtest.h>
#include <sstream>
#include "tinylang/Lexer/Lexer.h"

using namespace tinylang;
TEST(LexerTest, test1) {
    KeywordFilter fliter;
    fliter.addKeyWords();
    std::stringstream ss;
    for (size_t i = 0; i < tok::MAX_TOKENS_NUM; i++)
    {
        tok::TokenKind kind = static_cast<tok::TokenKind>(i);
        auto name = tok::getTokenName(kind);
        auto ret = fliter.getKeyWords(name, tok::identifier);
        ss << ret;
    }
    EXPECT_STREQ("222222222222222222222222232425262728293031323334353637383940", ss.str().c_str());
    
}