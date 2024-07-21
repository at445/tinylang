#include "tinylang/Lexer/Lexer.h"
using namespace tinylang;
void KeywordFilter::addKeyWords(void)
{
    for (size_t i = tok::TokenKind::KW_ENUM_START; i <= tok::TokenKind::KW_ENUM_END; i++)
    {
        tok::TokenKind kind = static_cast<tok::TokenKind>(i);
        keywords.insert({tok::getTokenName(kind), kind});
    }
    
}

tok::TokenKind KeywordFilter::getKeyWords(llvm::StringRef key, tok::TokenKind default_kind)
{
    auto iter = keywords.find(key);
    if (iter != keywords.end()) {
        return iter->second;
    }
    return default_kind;
}


namespace charinfo {
LLVM_READNONE inline bool isASCII(char Ch) {
  return static_cast<unsigned char>(Ch) <= 127;
}

LLVM_READNONE inline bool isVerticalWhitespace(char Ch) {
  return isASCII(Ch) && (Ch == '\r' || Ch == '\n');
}

LLVM_READNONE inline bool isHorizontalWhitespace(char Ch) {
  return isASCII(Ch) && (Ch == ' ' || Ch == '\t' ||
                         Ch == '\f' || Ch == '\v');
}

LLVM_READNONE inline bool isWhitespace(char Ch) {
  return isHorizontalWhitespace(Ch) ||
         isVerticalWhitespace(Ch);
}

LLVM_READNONE inline bool isDigit(char Ch) {
  return isASCII(Ch) && Ch >= '0' && Ch <= '9';
}

LLVM_READNONE inline bool isHexDigit(char Ch) {
  return isASCII(Ch) && (Ch >= 'A' && Ch <= 'F');
}

LLVM_READNONE inline bool isIdentifierHead(char Ch) {
  return isASCII(Ch) &&
         (Ch == '_' || (Ch >= 'A' && Ch <= 'Z') ||
          (Ch >= 'a' && Ch <= 'z'));
}

LLVM_READNONE inline bool isIdentifierBody(char Ch) {
  return isIdentifierHead(Ch) || isDigit(Ch);
}

LLVM_READNONE inline bool isStringBoundary(char Ch) {
  return Ch == '"' || Ch == '\'';
}

} // namespace charinfo

void Lexer::formToken(Token &Result, const char *TokEnd, tok::TokenKind Kind)
{
    Result.Ptr = CurPtr;
    Result.Length = TokEnd - CurPtr;
    Result.kind = Kind;
    CurPtr = TokEnd;
}

void Lexer::next(Token &Result) {
    while (*CurPtr && charinfo::isWhitespace(*CurPtr)) ++CurPtr;
 
    if(*CurPtr == '\0') {
        Result.setKind(tok::TokenKind::eof);
        return;
    }

    if(speculateNumber()) {
        number(Result);
        return;
    }
    if (charinfo::isIdentifierHead(*CurPtr)) {
        identifierOrKeyword(Result);
        return;
    } 
    if (charinfo::isStringBoundary(*CurPtr)) {
        string(Result);
        return;
    } 
    switch (*CurPtr)
    {
    case '+':
        formToken(Result, CurPtr+1, tok::plus);
        break;
    case '-':
        formToken(Result, CurPtr+1, tok::minus);
        break;
    case '*':
        formToken(Result, CurPtr+1, tok::star);
        break;
    case '/':
        formToken(Result, CurPtr+1, tok::slash);
        break;
    case '.':
        formToken(Result, CurPtr+1, tok::period);
        break;
    case ',':
        formToken(Result, CurPtr+1, tok::comma);
        break;
    case ';':
        formToken(Result, CurPtr+1, tok::semi);
        break;
    case '=':
        formToken(Result, CurPtr+1, tok::equal);
        break;
    case '#':
        formToken(Result, CurPtr+1, tok::hash);
        break;
    case ')':
        formToken(Result, CurPtr+1, tok::r_paren);
        break;
    // the following will look forward 2 charactors
    case ':':
        if (*(CurPtr+1) == '=') {
            formToken(Result, CurPtr+2, tok::colonequal);
        } else  {
            formToken(Result, CurPtr+1, tok::colon);
        }
        break;
    case '<':
        if (*(CurPtr+1) == '=') {
            formToken(Result, CurPtr+2, tok::lessequal);
        } else  {
            formToken(Result, CurPtr+1, tok::less);
        }
        break;
    case '>':
        if (*(CurPtr+1) == '=') {
            formToken(Result, CurPtr+2, tok::greaterequal);
        } else  {
            formToken(Result, CurPtr+1, tok::greater);
        }
        break;
    case '(':
        if (*(CurPtr+1) == '*') {
            comment();
        } else {
            formToken(Result, CurPtr+1, tok::l_paren);
        }
        break;
    default:
        Diags.report(getLoc(), diag::DIAG_ID::err_unknow_token);
        while(*CurPtr != '\0' && !charinfo::isVerticalWhitespace(*CurPtr)) {
            CurPtr++;
        }
        Result.setKind(tok::TokenKind::unknown);
        break;
    }
}

void tinylang::Lexer::identifierOrKeyword(Token &Result)
{
    auto endPtr = CurPtr + 1;
    while (*endPtr != '\0' && charinfo::isIdentifierBody(*endPtr))
    {
        endPtr++;
    }

    StringRef content(CurPtr, endPtr - CurPtr);

    auto kind = Keywords.getKeyWords(content, tok::TokenKind::identifier);
    
    formToken(Result, endPtr, kind);
}

bool tinylang::Lexer::speculateNumber(void)
{
    bool isHex = false;

    auto ptr = CurPtr;
    while(*ptr != '\0' && (charinfo::isDigit(*ptr) || charinfo::isHexDigit(*ptr))) {
        if (charinfo::isHexDigit(*ptr)) isHex = true;
        ptr++;
    }

    return (isHex) ? *ptr == 'H' : ptr != CurPtr;
}

void tinylang::Lexer::comment(void)
{
    const char *End = CurPtr + 2;

    while(*End != '\0' ) {
        if ((*End != '*' && *(End+1) != ')')) {
            End += 2;
            break;
        }
        End++;
    }
    if (*End == '\0') 
        Diags.report(getLoc(), diag::DIAG_ID::err_unterminated_block_comment);

    CurPtr = End;
}

void tinylang::Lexer::number(Token &Result) 
{
    bool isHex = false;

    auto endPtr = CurPtr + 1;
    while(*endPtr != '\0' && (charinfo::isDigit(*endPtr) || charinfo::isHexDigit(*endPtr))) {
        if (charinfo::isHexDigit(*endPtr)) isHex = true;
        endPtr++;
    }
    if (isHex) 
        endPtr += 1;

    formToken(Result, endPtr, tok::TokenKind::integer_literal);
}

void tinylang::Lexer::string(Token &Result)
{
    auto endPtr = CurPtr + 1;
    while(*endPtr != '\0' && (
            !charinfo::isVerticalWhitespace(*endPtr) || 
            !charinfo::isStringBoundary(*endPtr))) {
        endPtr++;
    }
    if (*endPtr != *CurPtr) 
        Diags.report(getLoc(), diag::err_expected, *CurPtr, *endPtr);

    formToken(Result, endPtr, tok::TokenKind::string_literal);
}
