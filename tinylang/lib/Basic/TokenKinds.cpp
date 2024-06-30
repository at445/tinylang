#include "tinylang/Basic/TokenKinds.h"
#include "llvm/Support/ErrorHandling.h"

using namespace tinylang;
namespace {
  struct TOKEN_CONTENT{
    const char* name;
    const char* spell; //only the punctuator have this content, others will be null
  };

  TOKEN_CONTENT tokenContent[] = {
    { /*tok::unknown,          */ "unknown", nullptr},
    { /*tok::eof,              */ "eof", nullptr},

    { /*tok::identifier,       */ "identifier", nullptr},         // abcde123

    { /*tok::integer_literal,  */ "integer_literal", nullptr},    // 123, 12/*3B, 123H
    { /*tok::string_literal,   */ "string_literal", nullptr},      // "foo", 'foo'

    { /*tok::plus,             */ "plus", "+"},         
    { /*tok::minus,            */ "minus", "-"},       
    { /*tok::star,             */ "star", "*"},       
    { /*tok::slash,            */ "slash", "/"},       
    { /*tok::colonequal,       */ "colonequal", ":="},
    { /*tok::period,           */ "period", "."},   
    { /*tok::comma,            */ "comma", ","},    
    { /*tok::semi,             */ "semi", ";"},     
    { /*tok::colon,            */ "colon", ":"},     
    { /*tok::equal,            */ "equal", "="},    
    { /*tok::hash,             */ "hash", "#"},     
    { /*tok::less,             */ "less", "<"},     
    { /*tok::greater,          */ "greater", ">"},  
    { /*tok::lessequal,        */ "lessequal", "<="}, 
    { /*tok::greaterequal,     */ "greaterequal", ">="},
    { /*tok::l_paren,          */ "l_paren", "("}, 
    { /*tok::r_paren,          */ "r_paren", ")"},  

    { /*tok::kw_AND,           */ "AND", nullptr}, 
    { /*tok::kw_BEGIN,         */ "BEGIN", nullptr},
    { /*tok::kw_CONST,         */ "CONST", nullptr},
    { /*tok::kw_DIV,           */ "DIV", nullptr},
    { /*tok::kw_DO,            */ "DO", nullptr}, 
    { /*tok::kw_END,           */ "END", nullptr},
    { /*tok::kw_ELSE,          */ "ELSE", nullptr},
    { /*tok::kw_FROM,          */ "FROM", nullptr},
    { /*tok::kw_IF,            */ "IF", nullptr},
    { /*tok::kw_IMPORT,        */ "IMPORT", nullptr},
    { /*tok::kw_MOD,           */ "MOD", nullptr},
    { /*tok::kw_MODULE,        */ "MODULE", nullptr},
    { /*tok::kw_NOT,           */ "NOT", nullptr},
    { /*tok::kw_OR,            */ "OR", nullptr},
    { /*tok::kw_PROCEDURE,     */ "PROCEDURE", nullptr},
    { /*tok::kw_RETURN,        */ "RETURN", nullptr},  
    { /*tok::kw_THEN,          */ "THEN", nullptr},
    { /*tok::kw_VAR,           */ "VAR", nullptr},
    { /*tok::kw_WHILE,         */ "WHILE", nullptr},
  };
}

const char *tok::getTokenName(TokenKind Kind) {
  if (Kind < MAX_TOKENS_NUM)
    return tokenContent[Kind].name;
  llvm_unreachable("unknown TokenKind");
  return nullptr;
}

const char *tok::getSpelling(TokenKind Kind){
  if (Kind < MAX_TOKENS_NUM) {
    if (tokenContent[Kind].spell == nullptr) return "";
    return tokenContent[Kind].spell;
  }
  llvm_unreachable("unknown TokenKind");
  return nullptr;
}
