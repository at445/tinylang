#ifndef TINYLANG_BASIC_DIAGNOSTIC_H
#define TINYLANG_BASIC_DIAGNOSTIC_H

#include "tinylang/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include <utility>

namespace tinylang {
  namespace diag {
    enum DIAG_ID {
      err_unterminated_block_comment,
      err_unterminated_char_or_string,
      err_hex_digit_in_decimal,

      err_expected,
      err_module_identifier_not_equal,
      note_module_identifier_declaration,
      err_proc_identifier_not_equal,
      note_proc_identifier_declaration,

      err_symbold_declared,
      err_types_for_operator_not_compatible,
      err_undeclared_name,
      err_if_expr_must_be_bool,
      err_while_expr_must_be_bool,
      err_vardecl_requires_type,
      err_returntype_must_be_type,
      err_function_call_on_nonfunction,
      err_procedure_call_on_nonprocedure,
      err_wrong_number_of_parameters,
      err_formal_and_actual_parameter_not_compatible,
      err_var_parameter_requires_var,
      warn_ambigous_negation,
      err_function_requires_return,
      err_procedure_requires_empty_return,
      err_function_and_return_type,

      err_not_yet_implemented,
    };
  } // namespace diag

  class DiagnosticsEngine {
    static const char *getDiagnosticText(diag::DIAG_ID typ);
    static SourceMgr::DiagKind getDiagnosticKind(diag::DIAG_ID typ);
  public:
    DiagnosticsEngine(SourceMgr &SrcMgr) : SrcMgr(SrcMgr), NumErrors(0) {}

    unsigned numErrors() { return NumErrors; }

    template <typename... Args>
    void report(SMLoc Loc, diag::DIAG_ID DiagID, Args &&... Arguments) {
      auto text = getDiagnosticText(DiagID);
      std::string Msg =llvm::formatv(text, std::forward<Args>(Arguments)...).str();
      SourceMgr::DiagKind Kind = getDiagnosticKind(DiagID);
      SrcMgr.PrintMessage(Loc, Kind, Msg);
      NumErrors += (Kind == SourceMgr::DK_Error);
    }
  private:
    SourceMgr &SrcMgr;
    unsigned NumErrors;
  };
} // namespace tinylang
/*
  how to use:


  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrErr = llvm::MemoryBuffer::getFile(fileName);
  
  if (std::error_code BufferError = FileOrErr.getError()) {
    llvm::errs() << "Error reading " << fileName << ": " << BufferError.message() << "\n";
    return 1;
  }

  llvm::SourceMgr SrcMgr;
  DiagnosticsEngine Diags(SrcMgr);

  // Tell SrcMgr about this buffer, which is what the
  // parser will pick up.
  SrcMgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());

  auto CurBuf = SrcMgr.getMemoryBuffer(SrcMgr.getMainFileID())->getBufferStart();

  Diags.report(SMLoc::getFromPointer(CurBuf+2), diag::err_symbold_declared, "chen", "jinsong");
*/
#endif