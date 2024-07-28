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

    if (!parseBlock())
        return false;

    if (!consume(tok::identifier)) {
        return false;
    }
    if (!consume(tok::period)) {
        return false;
    }
    return true;
}

bool tinylang::Parser::parseBlock() {

    while (curToken().isOneOf(tok::kw_CONST, tok::kw_PROCEDURE, tok::kw_VAR)) {
        if (!parseDeclaration())
        return false;
    }

    if (curToken().is(tok::kw_BEGIN)) {
        if (!consume(tok::kw_BEGIN)) return false;

        if (!parseStatementSequence()) return false;
    }

    if (!consume(tok::kw_END)) return false;
    return true;
}
bool tinylang::Parser::parseConstantDeclaration()
{
    while (curToken().is(tok::identifier)) {
        advance();
        if (!consume(tok::equal)) return false;

        if (!parseExpression()) return false;

        if (!consume(tok::semi)) return false;
    }
    return true;
}

bool tinylang::Parser::parseExpression()
{
    if (!parseprefixedExpression()) return false;

    if(curToken().isOneOf(tok::equal, tok::hash, tok::less,
        tok::lessequal,tok::greater,tok::greaterequal)) {
            advance();
            if (!parseprefixedExpression()) return false;    
    }
    return true;
}
bool tinylang::Parser::parseprefixedExpression() {
    if (curToken().isOneOf(tok::plus, tok::minus)) {
        advance();
    }

    if (!parseTerm()) return false;

    while (curToken().isOneOf(tok::plus, tok::minus, tok::kw_OR)) {
        advance();

        if (!parseTerm()) return false;
    }
    return true;
}

bool tinylang::Parser::parseTerm()
{
    if (!parseFactor()) return false;

    if (curToken().isOneOf(tok::star, tok::slash, tok::kw_AND,
                       tok::kw_DIV, tok::kw_MOD)) {
        advance();

        if (!parseFactor()) return false;
    }
    return true;
}
bool tinylang::Parser::parseFactor()
{
    if (curToken().is(tok::integer_literal)) {
        advance();
        return true;
    }

    if (curToken().is(tok::l_paren)) {
        advance();

        if (!parseExpression()) return false;

        if (!consume(tok::r_paren)) return false;
        return true;
    }

    if (curToken().is(tok::kw_NOT)) {
        return parseFactor();
    }

    if (curToken().is(tok::identifier)) {
        if (!parseQualident()) return false;

        while (curToken().is(tok::l_paren)) {
            advance();
            if (!parseExpressionList()) return false;

            if (!consume(tok::r_paren)) return false;
        }
    }
    return true;
}
bool tinylang::Parser::parseExpressionList() {
    if (!parseExpression()) return false;

    while(curToken().is(tok::comma)) {
        advance();

        if (!parseExpression()) return false;
    }

    return true;
}

bool tinylang::Parser::parseDeclaration() {
    if (curToken().is(tok::kw_CONST)) {
      advance();
      while (curToken().is(tok::identifier)) {
        if (!parseConstantDeclaration())
          return false;
      }
    } else if (curToken().is(tok::kw_VAR)) {
      advance();
      while (curToken().is(tok::identifier)) {
        if (!parseVariableDeclaration())
          return false;
      }
    } else if (curToken().is(tok::kw_PROCEDURE)) {
      if (!parseProcedureDeclaration())
        return false;
      if (!consume(tok::semi))
        return false;
    } else {
      /*ERROR*/
      return false;
    }
    return true;
}

bool tinylang::Parser::parseStatementSequence() {
    if (!parseStatement()) return false;
    while (curToken().is(tok::semi))
    {
        advance();

        if (!parseStatement()) return false;
    }
    
    return true;
}
bool tinylang::Parser::parseVariableDeclaration() {
    while (curToken().is(tok::identifier))
    {
        if (!parseIdentList()) return false;
        
        if (!consume(tok::colon))
            return false;
        
        if (!parseQualident()) return false;
        
        if (!consume(tok::semi))
            return false;
    }
    
    return true;
}
bool tinylang::Parser::parseProcedureDeclaration() {
    if (!consume(tok::kw_PROCEDURE))
        return false;

    if (!consume(tok::identifier))
        return false;

    if (!parseFormalParameters()) return false;

    if (!consume(tok::semi))
        return false;

    if (!parseBlock()) return false;

    if (!consume(tok::identifier))
        return false;
    return true;
}
bool tinylang::Parser::parseStatement()
{
    if (curToken().is(tok::identifier)) {
        if (!parseQualident()) return false;

        if (curToken().is(tok::colonequal)) {
            advance();
            if (!parseExpression()) return false;
        } else {
            if (!parseActualParams()) return false;
        }
        return true;
    }

    if (curToken().is(tok::kw_IF)) {
        return parseIfStatement();
    }

    if (curToken().is(tok::kw_WHILE)) {
        return parseWhileStatement();
    }

    if (curToken().is(tok::kw_RETURN)) {
        return parseReturnStatement();
    }

    return true;
}

bool tinylang::Parser::parseIfStatement() {
    if (!consume(tok::kw_IF)) {
        return false;
    }
    if (!parseExpression()) return false;

    if (!consume(tok::kw_THEN)) {
        return false;
    }

    if (!parseStatementSequence()) return false;

    if (curToken().is(tok::kw_ELSE)) {
        advance();

        if (!parseStatementSequence()) return false;
    }

    if (!consume(tok::kw_END)) {
        return false;
    }
    return true;
}
bool tinylang::Parser::parseWhileStatement()
{
    if (!consume(tok::kw_WHILE)) {
        return false;
    }
    if (!parseExpression()) return false;

    if (!consume(tok::kw_DO)) {
        return false;
    }

    if (!parseStatementSequence()) return false;

    if (!consume(tok::kw_END)) {
        return false;
    }
    return true;
}
bool tinylang::Parser::parseReturnStatement()
{
     if (!consume(tok::kw_RETURN)) {
        return false;
    }
    if (!curToken().is(tok::semi)) {
        return parseExpression();
    }
    return true;
}

bool tinylang::Parser::parseFormalParameter()
{
    if (curToken().is(tok::kw_VAR)) {
        advance();
    }
    if (!parseIdentList()) return false;

    if (!consume(tok::colon)) return false;

    if (!parseQualident()) return false;

    return true;
}
bool tinylang::Parser::parseFormalParameterList()
{
    if (curToken().isOneOf(tok::kw_VAR, tok::identifier))
    {
        if (!parseFormalParameter()) return false;
    }

    while(curToken().is(tok::semi)) {
        advance();
        if (!parseFormalParameter()) return false;
    }

    return true;
}


bool tinylang::Parser::parseFormalParameters()
{
    if (curToken().is(tok::l_paren)) {
        advance();

        if (!parseFormalParameterList()) return false;

        if (!consume(tok::r_paren)) return false;

        if (curToken().is(tok::colon)) {

            advance();

            if (!parseQualident()) return false;
        }
    }
    return true;
}

bool tinylang::Parser::parseActualParams()
{
    while (curToken().is(tok::l_paren)) {
        advance();

        if (!curToken().is(tok::r_paren)) {
            if (!parseExpressionList()) return false;
        }

        if (!consume(tok::kw_THEN)) return false;

        if (!parseStatementSequence()) return false;

        if(!curToken().is(tok::kw_ELSE)) {
            if (!parseStatementSequence()) return false;
        }
    }
    return true;
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

    return true;
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

bool tinylang::Parser::parseQualident(void)
{
    if(!expect(tok::identifier)) return false;
    advance();

    while (curToken().is(tok::period)) {
      advance();
      if (!expect(tok::identifier))
        return false;
      advance();
    }

    return true;
}