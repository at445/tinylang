#include "Parser.h"

AST *Parser::parseCalc() {
    if (Tok.is(Token::KW_with)) {
        llvm::SmallVector<llvm::StringRef, 8> Vars;
        advance();
        if(!expect(Token::ident)) return nullptr;
        Vars.push_back(Tok.getText());
        advance();

        while(Tok.getKind() == Token::comma) {
            advance();
            Vars.push_back(Tok.getText());
            advance();
        }
        if(!expect(Token::colon)) return nullptr;
        advance();

        return new WithDecl(std::move(Vars), parseExpr());
    }
    return parseExpr();
}

Expr *Parser::parseExpr() {
    auto lhs = parseTerm();
    if (lhs == nullptr) return nullptr;

    while (Tok.isOneOf(Token::plus, Token::minus))
    {
        BinaryOp::Operator opr = (Tok.is(Token::plus)) ? BinaryOp::Plus : BinaryOp::Minus;
        advance();
        auto rhs = parseTerm();
        lhs = new BinaryOp(opr, lhs, rhs);
    }
    return lhs;
}

Expr *Parser::parseTerm() {
    auto lhs = parseFactor();
    if (lhs == nullptr) return nullptr;

    while (Tok.isOneOf(Token::star, Token::slash))
    {
        BinaryOp::Operator opr = (Tok.is(Token::star)) ? BinaryOp::Mul : BinaryOp::Div;
        advance();
        auto rhs = parseFactor();
        lhs = new BinaryOp(opr, lhs, rhs);
    }
    return lhs;
}

Expr *Parser::parseFactor() {
    Expr * ret = nullptr;
    if (Tok.is(Token::ident)) {
        ret = new Factor(Factor::Ident, Tok.getText());
        advance();
    } else if (Tok.is(Token::number)) {
        ret = new Factor(Factor::Number, Tok.getText());
        advance();
    } else if (Tok.is(Token::l_paren)) {
        advance();
        ret = parseExpr();
        if (!consume(Token::r_paren)) return nullptr;
    } else {
        return nullptr;
    }
    return ret;
}

AST *Parser::parse(){
    auto ast = parseCalc();
    expect(Token::eoi);
    if(hasError()) return nullptr;
    return ast;
}