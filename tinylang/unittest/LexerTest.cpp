#include <gtest/gtest.h>
#include <sstream>
#include "tinylang/Lexer/Lexer.h"

using namespace tinylang;
extern std::string GetDirectoryName(std::string path);
#define TEST_CASE_DIRECTORY 

TEST(LexerTest, Keywords) {
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

std::string expectRet = "Token type: 33 name: MODULE  \n\
Token type: 2 name: identifier  Gcd  \n\
Token type: 12 name: semi  ;\n\
Token type: 39 name: VAR  \n\
Token type: 2 name: identifier  x  \n\
Token type: 13 name: colon  :\n\
Token type: 2 name: identifier  INTEGER  \n\
Token type: 12 name: semi  ;\n\
Token type: 36 name: PROCEDURE  \n\
Token type: 2 name: identifier  GCD  \n\
Token type: 20 name: l_paren  (\n\
Token type: 2 name: identifier  a  \n\
Token type: 11 name: comma  ,\n\
Token type: 2 name: identifier  b  \n\
Token type: 13 name: colon  :\n\
Token type: 2 name: identifier  INTEGER  \n\
Token type: 21 name: r_paren  )\n\
Token type: 13 name: colon  :\n\
Token type: 2 name: identifier  INTEGER  \n\
Token type: 12 name: semi  ;\n\
Token type: 39 name: VAR  \n\
Token type: 2 name: identifier  t  \n\
Token type: 13 name: colon  :\n\
Token type: 2 name: identifier  INTEGER  \n\
Token type: 12 name: semi  ;\n\
Token type: 23 name: BEGIN  \n\
Token type: 30 name: IF  \n\
Token type: 2 name: identifier  b  \n\
Token type: 14 name: equal  =\n\
Token type: 3 name: integer_literal  0  \n\
Token type: 38 name: THEN  \n\
Token type: 37 name: RETURN  \n\
Token type: 2 name: identifier  a  \n\
Token type: 12 name: semi  ;\n\
Token type: 27 name: END  \n\
Token type: 12 name: semi  ;\n\
Token type: 40 name: WHILE  \n\
Token type: 2 name: identifier  b  \n\
Token type: 15 name: hash  #\n\
Token type: 3 name: integer_literal  0  \n\
Token type: 26 name: DO  \n\
Token type: 2 name: identifier  t  \n\
Token type: 9 name: colonequal  :=\n\
Token type: 2 name: identifier  a  \n\
Token type: 32 name: MOD  \n\
Token type: 3 name: integer_literal  1AH  \n\
Token type: 12 name: semi  ;\n\
Token type: 2 name: identifier  a  \n\
Token type: 9 name: colonequal  :=\n\
Token type: 2 name: identifier  b  \n\
Token type: 12 name: semi  ;\n\
Token type: 2 name: identifier  b  \n\
Token type: 9 name: colonequal  :=\n\
Token type: 2 name: identifier  t  \n\
Token type: 12 name: semi  ;\n\
Token type: 27 name: END  \n\
Token type: 12 name: semi  ;\n\
Token type: 37 name: RETURN  \n\
Token type: 2 name: identifier  a  \n\
Token type: 12 name: semi  ;\n\
Token type: 27 name: END  \n\
Token type: 2 name: identifier  GCD  \n\
Token type: 12 name: semi  ;\n\
Token type: 27 name: END  \n\
Token type: 2 name: identifier  Gcd  \n\
Token type: 10 name: period  .\n\
Token type: 1 name: eof  \n";

TEST(DiagnosticTest, lexerTest) {
  auto fileName = GetDirectoryName(__FILE__) + "input/Gcd.txt";
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrErr = llvm::MemoryBuffer::getFile(fileName);
  
  if (std::error_code BufferError = FileOrErr.getError()) {
    llvm::errs() << "Error reading " << "Gcd.txt" << ": " << BufferError.message() << "\n";
    return;
  }

  llvm::SourceMgr SrcMgr;
  DiagnosticsEngine Diags(SrcMgr);

  // Tell SrcMgr about this buffer, which is what the
  // parser will pick up.
  SrcMgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());

  Lexer lexer(SrcMgr, Diags);
  Token tk;
  std::string str;
  llvm::raw_string_ostream output(str);
  do {
    lexer.next(tk);
    output << tk;
  } while (tk.getKind() != tok::TokenKind::eof && tk.getKind() != tok::TokenKind::unknown);
  EXPECT_STREQ(expectRet.c_str(), output.str().c_str());
}
  
