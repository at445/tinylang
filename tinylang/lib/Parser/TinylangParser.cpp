#include "tinylang/Parser/TinylangParser.h"

bool tinylang::Parser::parseCompilationUnit(ModuleDeclaration *& module)
{
    if (!consume(tok::kw_MODULE)) {
        return false;
    }
    if (!expect(tok::identifier)) {
        return false;
    }
    module = new ModuleDeclaration(nullptr, 
                                    curToken().getLocation(),
                                    curToken().getIdentifier());
    EnterDeclScope _(Semantic, module);
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
    if (!parseBlock(Decls, Stmts)) {
        module->setDecls(std::move(Decls));
        module->setStmts(std::move(Stmts));
        return false;
    }
    if (!consume(tok::identifier)) {
        return false;
    }
    if (!consume(tok::period)) {
        return false;
    }
    module->setDecls(std::move(Decls));
    module->setStmts(std::move(Stmts));
    return true;
}

bool tinylang::Parser::parseBlock(DeclList& decls, StmtList& stmts) {

    while (curToken().isOneOf(tok::kw_CONST, tok::kw_PROCEDURE, tok::kw_VAR)) {
        if (!parseDeclaration(decls))
        return false;
    }

    if (curToken().is(tok::kw_BEGIN)) {
        if (!consume(tok::kw_BEGIN)) return false;

        if (!parseStatementSequence(stmts)) return false;
    }

    if (!consume(tok::kw_END)) return false;
    return true;
}
bool tinylang::Parser::parseConstantDeclaration(DeclList& decls)
{
    while (curToken().is(tok::identifier)) {
        auto loc = curToken().getLocation();
        auto name = curToken().getIdentifier();
        advance();
        if (!consume(tok::equal)) return false;
        Expr * expr = nullptr; 
        if (!parseExpression(expr)) return false;

        if (!consume(tok::semi)) return false;
        Semantic.actOnConstantDecl(decls, loc, name, expr);
    }
    return true;
}

bool tinylang::Parser::parseExpression(Expr *&E)
{
    
    if (!parseSimpleExpression(E)) return false;

    if(curToken().isOneOf(tok::equal, tok::hash, tok::less,
        tok::lessequal,tok::greater,tok::greaterequal)) {
         auto op = generateOp();
         advance();
         Expr * rExpr = nullptr;
         if (!parseSimpleExpression(rExpr)) return false;
         E = Semantic.actOnRelationExpr(E, rExpr, op);
    }
    return true;
}
bool tinylang::Parser::parseSimpleExpression(Expr *&E) {
    if (!parsePrefixExpression(E)) return false;

    while (curToken().isOneOf(tok::plus, tok::minus, tok::kw_OR)) {
        auto op = generateOp();
        advance();
        Expr * rExpr = nullptr;
        if (!parsePrefixExpression(rExpr)) return false; 
        E = Semantic.actOnSimpleExpr(E, rExpr, op);
    }
    return true;
}
bool tinylang::Parser::parsePrefixExpression(Expr *&E) {
    if (curToken().isOneOf(tok::plus, tok::minus)) {
        auto op = generateOp();
        advance();
        Expr * rExpr = nullptr;
        if (!parseTerm(rExpr)) return false;
        E = Semantic.actOnPrefixedExpr(rExpr, op);
    }
    
    if (!parseTerm(E)) return false; 
    return true;
}


bool tinylang::Parser::parseTerm(Expr *&E)
{
    SMLoc loc = curToken().getLocation();
    if (!parseFactor(E)) return false;

    if (!curToken().isOneOf(tok::star, tok::slash, tok::kw_AND,
                       tok::kw_DIV, tok::kw_MOD)) {
                        return true;
    }
    auto op = generateOp();
    advance();
    Expr* rExpr = nullptr;
    if (!parseTerm(rExpr)) return false;
    return Semantic.actOnTerm(loc, op, E, E, rExpr);
}
bool tinylang::Parser::parseFactor(Expr *&E)
{
    if (curToken().is(tok::integer_literal)) {

        Token tok = curToken();
        
        SMLoc loc = tok.getLocation();
        StringRef val = tok.getLiteralData();
        E = Semantic.actOnIntegerLiteral(loc, val);
        advance();
        return true;
    }

    if (curToken().is(tok::l_paren)) {
        advance();
        if (!parseExpression(E)) return false;

        if (!consume(tok::r_paren)) return false;
        return true;
    }

    if (curToken().is(tok::kw_NOT)) {
        auto op = generateOp();
        advance();
        Expr* expr = nullptr;
        if(!parseFactor(expr)) return false;
        E = Semantic.actOnPrefixedExpr(expr, op);
        return true;
    }

    if (curToken().is(tok::identifier)) {
        Token tok = curToken();
        advance();
        SMLoc loc = tok.getLocation();
        StringRef val = tok.getIdentifier();
        if (!Semantic.actOnAccess(loc, val, E)) return false;
        while (curToken().is(tok::l_paren)) {
            advance();
            ExprList exprs;
            if (!parseExpressionList(exprs)) return false;

            if (!consume(tok::r_paren)) return false;
        }
    }
    return true;
}
bool tinylang::Parser::parseExpressionList(ExprList& exprList) {
    Expr * p = nullptr;
    if (!parseExpression(p)) return false;
    exprList.push_back(p);

    while(curToken().is(tok::comma)) {
        advance();
        if (!parseExpression(p)) return false;
        exprList.push_back(p);
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

bool tinylang::Parser::parseStatementSequence(StmtList& stmts) {
    if (!parseStatement(stmts)) return false;
    while (curToken().is(tok::semi))
    {
        advance();

        if (!parseStatement(stmts)) return false;
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
    auto loc = curToken().getLocation();
    advance();

    if (!expect(tok::identifier)) {
        return false;
    }
    auto name = curToken().getIdentifier();
    advance();

    ProcudureDeclaration * procDecl = nullptr;

    procDecl = new ProcudureDeclaration(Semantic.getCurrentDecl(), loc, name);
    EnterDeclScope _(Semantic, procDecl);

    if (!parseFormalParameters(procDecl)) return false;

    if (!consume(tok::semi))
        return false;

    DeclList Decls;
    StmtList Stmts; 
    if (!parseBlock(Decls, Stmts)) return false;
    procDecl->addDecl(Decls);
    procDecl->concatStmts(Stmts);
    if (!consume(tok::identifier))
        return false;
    decls.push_back(procDecl);
    return true;
}
bool tinylang::Parser::parseStatement(StmtList& stmts)
{
    if (curToken().is(tok::identifier)) {
        Expr* E = nullptr;
        auto tok = curToken();
        advance();
        SMLoc loc = tok.getLocation();
        StringRef val = tok.getIdentifier();

        if (!Semantic.actOnAccess(loc, val, E)) return false;
        auto *V = dyn_cast<VariableAccess>(E);
        if (!V) {
         diags.report(tok.getLocation(), diag::err_undeclared_name, tok.getIdentifier());
         return false;
        }
         if (curToken().is(tok::colonequal)) {
            advance();
            Expr * p = nullptr; 
            if (!parseExpression(p)) return false;
            stmts.push_back(new AssignStatement(V, p));
         } else {
            if (!parseActualParams()) return false;
         }
    }

    if (curToken().is(tok::kw_IF)) {
        return parseIfStatement(stmts);
    }

    if (curToken().is(tok::kw_WHILE)) {
        return parseWhileStatement(stmts);
    }

    if (curToken().is(tok::kw_RETURN)) {
        return parseReturnStatement(stmts);
    }

    return true;
}

bool tinylang::Parser::parseIfStatement(StmtList& stmts) {
    if (!consume(tok::kw_IF)) {
        return false;
    }
    Expr * p = nullptr; //dummy
    if (!parseExpression(p)) return false;
    IfStatement* ifStmt = new IfStatement(p);
    stmts.push_back(ifStmt);
    if (!consume(tok::kw_THEN)) {
        return false;
    }
    StmtList trueBranch;
    if (!parseStatementSequence(trueBranch)) return false;
    ifStmt->setTrueBranch(trueBranch);
    if (curToken().is(tok::kw_ELSE)) {
        advance();
        StmtList falseBranch;
        if (!parseStatementSequence(falseBranch)) return false;
        ifStmt->setFalseBranch(falseBranch);
    }

    if (!consume(tok::kw_END)) {
        return false;
    }
    return true;
}
bool tinylang::Parser::parseWhileStatement(StmtList& stmts)
{
    if (!consume(tok::kw_WHILE)) {
        return false;
    }
    Expr * p = nullptr; //dummy
    if (!parseExpression(p)) return false;

    if (!consume(tok::kw_DO)) {
        return false;
    }

    if (!parseStatementSequence(stmts)) return false;

    if (!consume(tok::kw_END)) {
        return false;
    }
    return true;
}
bool tinylang::Parser::parseReturnStatement(StmtList& stmts)
{
   consume(tok::kw_RETURN);
   
   Expr * p = nullptr; 
   if(!parseExpression(p)) return false;
   stmts.push_back(new ReturnStatement(p));
   return true;
}

bool tinylang::Parser::parseFormalParameter(FormalParamList& params)
{
    IdentList ids;
    bool IsVar = false;
    if (curToken().is(tok::kw_VAR)) {
        IsVar = true;
        advance();
    }
    if (!parseIdentList(ids)) return false;

    if (!consume(tok::colon)) return false;

    Decl* D = nullptr;
    SMLoc lstLoc;
    if (!parseQualident(D, lstLoc)) return false;
    return Semantic.actOnFormalParameter(params, ids, D, IsVar, lstLoc);
}
bool tinylang::Parser::parseFormalParameterList(FormalParamList& params)
{
    if (curToken().isOneOf(tok::kw_VAR, tok::identifier))
    {
        if (!parseFormalParameter(params)) return false;
    }

    while(curToken().is(tok::semi)) {
        advance();
        if (!parseFormalParameter(params)) return false;
    }
    return true;
}


bool tinylang::Parser::parseFormalParameters(ProcudureDeclaration*& procDecl)
{
    if(!expect(tok::l_paren)) return false;
    advance();

    FormalParamList params;
    if (!parseFormalParameterList(params)) return false;

    if (!consume(tok::r_paren)) return false;

    if (!expect(tok::colon)) return false;

    advance();

    Decl* type = nullptr;
    SMLoc lstLoc;
    if (!parseQualident(type, lstLoc)) return false;
    if (TypeDeclaration* typ = dyn_cast<TypeDeclaration>(type)) {
        procDecl->setReturnType(typ);
    } else {
        diags.report(curToken().getLocation(), diag::err_unkonw_type);
    }
    procDecl->addFormalParams(params);
    return true;
}

bool tinylang::Parser::parseActualParams()
{
    while (curToken().is(tok::l_paren)) {
        advance();

        if (!curToken().is(tok::r_paren)) {
            ExprList exprs;
            if (!parseExpressionList(exprs)) return false;
        }

        if (!consume(tok::kw_THEN)) return false;
        StmtList stmts;
        if (!parseStatementSequence(stmts)) return false;

        if(!curToken().is(tok::kw_ELSE)) {
            if (!parseStatementSequence(stmts)) return false;
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

tinylang::OperatorInfo tinylang::Parser::generateOp()
{
    tinylang::OperatorInfo op(curToken().getLocation(),
                        curToken().getKind());
    return op;
}