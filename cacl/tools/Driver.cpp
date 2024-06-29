#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/StringRef.h"
#include "Version.h"
#include "Lexer.h"
#include "Parser.h"
#include "SematicCheck.h"
#include "CodeGen.h"
static llvm::cl::opt<std::string>
    Input(llvm::cl::Positional,
          llvm::cl::desc("<input expression>"),
          llvm::cl::init(""));

int main(int argc_, const char **argv_) {
  llvm::InitLLVM X(argc_, argv_);
  llvm::cl::ParseCommandLineOptions(
      argc_, argv_, "calc - the expression compiler\n");
  Lexer lexr(Input);
  Parser parser(lexr);
  AST * ast = parser.parse();
  if (ast == nullptr) return -1;
  SematicCheck sema;
  if (sema.check(ast)) {
    llvm::errs() << "Semantic errors occured\n";
    return 1;
  } else {
    CodeGen CodeGenerator;
    CodeGenerator.compile(ast);
    return 0;
  }
}