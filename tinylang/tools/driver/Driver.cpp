#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/Version.h"
#include "tinylang/Basic/TokenKinds.h"
#include "tinylang/Lexer/Lexer.h"
#include "tinylang/Parser/TinylangParser.h"
#include "llvm/Support/InitLLVM.h"

using namespace tinylang;

std::string fileName = "/home/jasson/Practice/tinylang/Gcd.mod";
int main(int argc_, const char **argv_) {
  llvm::InitLLVM X(argc_, argv_);

  llvm::outs() << "Tinylang version is " << tinylang::getTinylangVersion() << "\n\n";

  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrErr = llvm::MemoryBuffer::getFile(fileName);
  
  if (std::error_code BufferError = FileOrErr.getError()) {
    llvm::errs() << "Error reading " << "Gcd.txt" << ": " << BufferError.message() << "\n";
    return -1;
  }

  llvm::SourceMgr srcMgr;
  DiagnosticsEngine diags(srcMgr);

  // Tell SrcMgr about this buffer, which is what the
  // parser will pick up.
  srcMgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());

  auto lexer = Lexer(srcMgr, diags);

  auto sema = Sema(diags);

  Parser parser(lexer, sema, diags);

  return parser.parseCompilationUnit();
}