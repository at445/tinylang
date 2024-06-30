#include "tinylang/Basic/Diagnostic.h"
using namespace tinylang;

namespace {
  struct Diag_Preset_Content{
    SourceMgr::DiagKind diagKind;
    const char * text;
  };
  
  Diag_Preset_Content diagContent[] = {
    {/*diag::err_unterminated_block_comment,                 */ SourceMgr::DK_Error, "unterminated (* comment"},
    {/*diag::err_unterminated_char_or_string,                */ SourceMgr::DK_Error, "missing terminating character"},
    {/*diag::err_hex_digit_in_decimal,                       */ SourceMgr::DK_Error, "decimal number contains hex digit"},

    {/*diag::err_expected,                                   */ SourceMgr::DK_Error, "expected {0} but found {1}"},
    {/*diag::err_module_identifier_not_equal,                */ SourceMgr::DK_Error, "module identifier at begin and end not equal"},
    {/*diag::note_module_identifier_declaration,             */ SourceMgr::DK_Note, "module identifier declared here"},
    {/*diag::err_proc_identifier_not_equal,                  */ SourceMgr::DK_Error, "procedure identifier at begin and end not equal"},
    {/*diag::note_proc_identifier_declaration,               */ SourceMgr::DK_Note, "procedure identifier declared here"},

    {/*diag::err_symbold_declared,                           */ SourceMgr::DK_Error, "symbol {0} already declared"},
    {/*diag::err_types_for_operator_not_compatible,          */ SourceMgr::DK_Error, "types not compatible for operator {0}"},
    {/*diag::err_undeclared_name,                            */ SourceMgr::DK_Error, "undeclared name {0}"},
    {/*diag::err_if_expr_must_be_bool,                       */ SourceMgr::DK_Error, "expression of IF statement must have type BOOLEAN"},
    {/*diag::err_while_expr_must_be_bool,                    */ SourceMgr::DK_Error, "expression of IF statement must have type BOOLEAN"},
    {/*diag::err_vardecl_requires_type,                      */ SourceMgr::DK_Error, "variable declaration requires type"},
    {/*diag::err_returntype_must_be_type,                    */ SourceMgr::DK_Error, "return type of function must be declared type"},
    {/*diag::err_function_call_on_nonfunction,               */ SourceMgr::DK_Error, "function call requires a function"},
    {/*diag::err_procedure_call_on_nonprocedure,             */ SourceMgr::DK_Error, "procedure call requires a procedure"},
    {/*diag::err_wrong_number_of_parameters,                 */ SourceMgr::DK_Error, "wrong number of parameters"},
    {/*diag::err_formal_and_actual_parameter_not_compatible, */ SourceMgr::DK_Error, "type of formal and actual parameter are not compatible"},
    {/*diag::err_var_parameter_requires_var,                 */ SourceMgr::DK_Error, "VAR parameter requires variable as argument"},
    {/*diag::warn_ambigous_negation,                         */ SourceMgr::DK_Warning, "Negation is ambigous. Please consider using parenthesis."},
    {/*diag::err_function_requires_return,                   */ SourceMgr::DK_Error, "Function requires RETURN with value"},
    {/*diag::err_procedure_requires_empty_return,            */ SourceMgr::DK_Error, "Procedure does not allow RETURN with value"},
    {/*diag::err_function_and_return_type,                   */ SourceMgr::DK_Error, "Type of RETURN value is not compatible with function type"},

    {/*diag::err_not_yet_implemented,                        */ SourceMgr::DK_Error, "module imports are not yet implemented"},
  };
} // namespace


const char *DiagnosticsEngine::getDiagnosticText(diag::DIAG_ID diag_id)
{
    return diagContent[diag_id].text;
}

SourceMgr::DiagKind DiagnosticsEngine::getDiagnosticKind(diag::DIAG_ID diag_id)
{
    return diagContent[diag_id].diagKind;
}

