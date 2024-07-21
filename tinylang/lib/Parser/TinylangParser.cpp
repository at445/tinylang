#include "tinylang/Parser/TinylangParser.h"

bool tinylang::Parser::parseCompilationUnit(void)
{
    if (!consume(tok::kw_MODULE)) {
        return false;
    }
    if (!consume(tok::identifier)) {
        return false;
    }
    if (!consume(tok::semi)) {
        return false;
    }

    while (curToken().isOneOf(tok::kw_FROM, tok::kw_IMPORT)) {
      if (!parseImport())
        return false;
    }

    return false;
}
bool tinylang::Parser::parseImport(void)
{
    if (curToken().is(tok::kw_FROM)) {
        advance();

        if (!consume(tok::identifier)) {
            return false;
        }
    } 

    if (!consume(tok::kw_IMPORT)) {
        return false;
    }

    if (!parseIdentList()) {
        return false;
    }
    if (!consume(tok::semi)) {
        return false;
    }

    return false;
}
bool tinylang::Parser::parseIdentList(void)
{
    if(!expect(tok::identifier)) return false;
    advance();

    while (curToken().is(tok::comma)) {
      advance();
      if (!expect(tok::identifier))
        return false;
      advance();
    }
    return true;
}