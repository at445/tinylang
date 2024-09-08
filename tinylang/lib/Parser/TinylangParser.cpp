#include "tinylang/Parser/TinylangParser.h"


bool tinylang::Parser::parseCompilationUnit(void)
{
    if (!consume(tok::kw_MODULE)) {
        return false;
    }
    if (!expect(tok::identifier)) {
        return false;
    }
    auto module = ModuleDeclaration(nullptr, 
                                    curToken().getLocation(),
                                    curToken().getIdentifier());
    EnterDeclScope _(Semantic, &module);
    advance();
    if (!consume(tok::semi)) {
        return false;
    }

    while (curToken().isOneOf(tok::kw_FROM, tok::kw_IMPORT)) {
      if (!parseImport())
        return false;
    }
    DeclList Decls;
    StmtList Stmts;
    if (!parseBlock(Decls, Stmts))
        return false;

    if (!consume(tok::identifier)) {
        return false;
    }
    if (!consume(tok::period)) {
        return false;
    }
    return true;
}

bool tinylang::Parser::parseBlock(DeclList& decls, StmtList& stmts) {

    while (curToken().isOneOf(tok::kw_CONST, tok::kw_PROCEDURE, tok::kw_VAR)) {
        if (!parseDeclaration(decls))
        return false;
    }

    if (curToken().is(tok::kw_BEGIN)) {
        if (!consume(tok::kw_BEGIN)) return false;

        if (!parseStatementSequence()) return false;
    }

    if (!consume(tok::kw_END)) return false;
    return true;
}
bool tinylang::Parser::parseConstantDeclaration(DeclList& decls)
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
    Expr* expr = nullptr;
    if (!parseFactor(expr)) return false;

    if (curToken().isOneOf(tok::star, tok::slash, tok::kw_AND,
                       tok::kw_DIV, tok::kw_MOD)) {
        advance();

        if (!parseFactor(expr)) return false;
    }
    return true;
}
bool tinylang::Parser::parseFactor(Expr *&E)
{
    if (curToken().is(tok::integer_literal)) {
        E = Semantic.actOnIntegerLiteral(
            curToken().getLocation(),
            curToken().getIdentifier()
        );
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
        Expr* expr = nullptr;
        advance();
        return parseFactor(expr);
    }

    if (curToken().is(tok::identifier)) {

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

bool tinylang::Parser::parseDeclaration(DeclList& decls) {
    if (curToken().is(tok::kw_CONST)) {
      advance();
      while (curToken().is(tok::identifier)) {
        if (!parseConstantDeclaration(decls))
          return false;
      }
    } else if (curToken().is(tok::kw_VAR)) {
      advance();
      while (curToken().is(tok::identifier)) {
        if (!parseVariableDeclaration(decls))
          return false;
      }
    } else if (curToken().is(tok::kw_PROCEDURE)) {
      if (!parseProcedureDeclaration(decls))
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
bool tinylang::Parser::parseVariableDeclaration(DeclList& decls) {

    IdentList ids;
    if (!parseIdentList(ids)) 
        return false;
    
    if (!consume(tok::colon))
        return false;
    
    Decl* D = nullptr;
    SMLoc lstLoc;
    if (!parseQualident(D, lstLoc)) 
        return false;
    
    if (!consume(tok::semi))
        return false;
    return Semantic.actOnVariableDeclarationPart(decls, ids, D, lstLoc);
}
bool tinylang::Parser::parseProcedureDeclaration(DeclList& decls) {
    if (!consume(tok::kw_PROCEDURE))
        return false;

    if (!consume(tok::identifier))
        return false;

    if (!parseFormalParameters()) return false;

    if (!consume(tok::semi))
        return false;

    DeclList Decls;
    StmtList Stmts;
    if (!parseBlock(Decls, Stmts)) return false;

    if (!consume(tok::identifier))
        return false;
    return true;
}
bool tinylang::Parser::parseStatement()
{
    if (curToken().is(tok::identifier)) {
        Decl* D = nullptr;
        SMLoc lstLoc;
        if (!parseQualident(D, lstLoc)) return false;

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
    IdentList ids;
    if (curToken().is(tok::kw_VAR)) {
        advance();
    }
    if (!parseIdentList(ids)) return false;

    if (!consume(tok::colon)) return false;

    Decl* D = nullptr;
    SMLoc lstLoc;
    if (!parseQualident(D, lstLoc)) return false;

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

            Decl* D = nullptr;
            SMLoc lstLoc;
            if (!parseQualident(D, lstLoc)) return false;
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
    diags.report(curToken().getLocation(), diag::err_not_yet_implemented);
    return false;
    IdentList ids;
    if (curToken().is(tok::kw_FROM)) {
        advance();

        if (!consume(tok::identifier)) {
            return false;
        }
    } 

    if (!consume(tok::kw_IMPORT)) {
        return false;
    }

    if (!parseIdentList(ids)) {
        return false;
    }
    if (!consume(tok::semi)) {
        return false;
    }
    
    return true;
}
bool tinylang::Parser::parseIdentList(IdentList &identList)
{
    if(!expect(tok::identifier)) return false;
    auto ident = Ident(curToken().getLocation(), curToken().getIdentifier());
    identList.push_back(std::move(ident));
    advance();

    while (curToken().is(tok::comma)) {
        advance();
        if (!expect(tok::identifier)) return false;
        auto ident = Ident(curToken().getLocation(), curToken().getIdentifier());
        identList.push_back(std::move(ident));
        advance();
    }
    return true;
}
//qualident -> identifier.(qualident)* 
bool tinylang::Parser::parseQualident(Decl *&D, SMLoc& lastLoc)
{
    if(!expect(tok::identifier)) return false;
    // 找到Module的定义
    D = nullptr;
    D = Semantic.actOnQualIdentPart(D, curToken().getLocation(),
                                    curToken().getIdentifier());
    lastLoc = curToken().getLocation();
    advance();
    if(!D) {
        return false;
    }

    while (curToken().is(tok::period) && isa<ModuleDeclaration>(D)) {
        advance();
        if (!expect(tok::identifier)) return false;
        D = Semantic.actOnQualIdentPart(D, curToken().getLocation(),
                                        curToken().getIdentifier());
        advance();
    }

    
    return true;
}