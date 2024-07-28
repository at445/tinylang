#ifndef TINYLANG_BASIC_TOKENKINDS_H
#define TINYLANG_BASIC_TOKENKINDS_H

#include "llvm/Support/Compiler.h"

namespace tinylang {

namespace tok {
  enum TokenKind : unsigned short {
    unknown = 0,
    eof,
    
    identifier,          // abcde123
    
    integer_literal,     // 123, 123B, 123H
    string_literal,      // "foo", 'foo'
    
    plus,         
    minus,        
    star,         
    slash,        
    colonequal,   
    period,       
    comma,        
    semi,         
    colon,        
    equal,        
    hash,         
    less,         
    greater,      
    lessequal,    
    greaterequal, 
    l_paren,      
    r_paren,      

    KW_ENUM_START,
    kw_AND = KW_ENUM_START,     
    kw_BEGIN,
    kw_CONST,
    kw_DIV,  
    kw_DO,   
    kw_END,  
    kw_ELSE, 
    kw_FROM, 
    kw_IF,   
    kw_IMPORT, 
    kw_MOD,  
    kw_MODULE,
    kw_NOT,  
    kw_OR,   
    kw_PROCEDURE,
    kw_RETURN,   
    kw_THEN, 
    kw_VAR,
    kw_WHILE,
    KW_ENUM_END = kw_WHILE,

    MAX_TOKENS_NUM,
  };

  const char *getTokenName(TokenKind Kind) LLVM_READNONE;

  const char *getSpelling(TokenKind Kind) LLVM_READNONE;
} // namespace tok
} // namespace tinylang

#endif