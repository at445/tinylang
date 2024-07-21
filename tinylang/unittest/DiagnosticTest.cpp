#include "gtest/gtest.h"
#include <gmock/gmock.h>
#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/LLVM.h"

using namespace tinylang;

#define TEST_CASE_DIRECTORY GetDirectoryName(__FILE__)

std::string GetDirectoryName(std::string path){
    const size_t last_slash_idx = path.rfind('/');
    if (std::string::npos != last_slash_idx)
    {
        return path.substr(0, last_slash_idx + 1);
    }
    return "";
}

TEST(DiagnosticTest, test1) {
  auto fileName = std::string(TEST_CASE_DIRECTORY) + "Input.txt";
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrErr = llvm::MemoryBuffer::getFile(fileName);
  
  if (std::error_code BufferError = FileOrErr.getError()) {
    llvm::errs() << "Error reading " << "input.txt" << ": " << BufferError.message() << "\n";
    return;
  }

  llvm::SourceMgr SrcMgr;
  DiagnosticsEngine Diags(SrcMgr);

  // Tell SrcMgr about this buffer, which is what the
  // parser will pick up.
  SrcMgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());

  auto CurBuf = SrcMgr.getMemoryBuffer(SrcMgr.getMainFileID())->getBufferStart();

  auto loc = SMLoc::getFromPointer(CurBuf+2);
  std::string str;
  llvm::raw_string_ostream output(str);
  Diags.report(output, loc, diag::err_symbold_declared, "chen");

  EXPECT_THAT(output.str(), testing::HasSubstr("error: symbol chen already declared"));
}
  
