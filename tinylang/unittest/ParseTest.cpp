#include <gtest/gtest.h>
#include <sstream>
#include "tinylang/Lexer/Lexer.h"
#include "tinylang/Parser/TinylangParser.h"

using namespace tinylang;
extern std::string GetDirectoryName(std::string path);
#define TEST_CASE_DIRECTORY 

TEST(ParseTest, parseTest) {
  auto fileName = GetDirectoryName(__FILE__) + "input/ParseInput.txt";
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrErr = llvm::MemoryBuffer::getFile(fileName);
  
  if (std::error_code BufferError = FileOrErr.getError()) {
    llvm::errs() << "Error reading " << "ParseInput.txt" << ": " << BufferError.message() << "\n";
    return;
  }

  llvm::SourceMgr SrcMgr;
  DiagnosticsEngine Diags(SrcMgr);

  // Tell SrcMgr about this buffer, which is what the
  // parser will pick up.
  SrcMgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());

  auto lexer = Lexer(SrcMgr, Diags);

  auto sema = Sema(Diags);

  Parser parser(lexer, sema, Diags);

  ModuleDeclaration * module = nullptr;
  EXPECT_TRUE(parser.parseCompilationUnit(module));
}
  
