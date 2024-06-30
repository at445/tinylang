#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/Version.h"
#include "tinylang/Basic/TokenKinds.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_ostream.h"
#include <iostream>
using namespace tinylang;
std::string fileName = "/home/jasson/Practice/tinylang/Gcd.mod";
int main(int argc_, const char **argv_) {
  llvm::InitLLVM X(argc_, argv_);
  // // llvm::SmallVector<const char *, 256> argv(argv_ + 1,
  // //                                           argv_ + argc_);
  llvm::outs() << "Tinylang version is " << tinylang::getTinylangVersion() << "\n\n";


  for (size_t i = 0; i < tok::MAX_TOKENS_NUM; i++)
  {
    auto spelling = tok::getSpelling(static_cast<tok::TokenKind>(i));
    std::cout << tok::getTokenName(static_cast<tok::TokenKind>(i)) << " :" << spelling << "\n";
  }
  

  
  
  return 0;
}