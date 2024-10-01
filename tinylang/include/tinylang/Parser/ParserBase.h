#ifndef TINYLANG_PARSER_BASE_H
#define TINYLANG_PARSER_BASE_H

#include "tinylang/Basic/LLVM.h"
#include "tinylang/Basic/TokenKinds.h"
#include "tinylang/Lexer/Token.h"
#include "tinylang/Lexer/Lexer.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/DenseMap.h"

namespace tinylang {
    class ParserBase {
    public:
        ParserBase(Lexer &lexer, DiagnosticsEngine &diag) 
            : curIdx(0), lex(lexer), diags(diag){
            sync(1);
        }
    protected:
        tok::TokenKind LookAheadTyp(int offset = 1) {
             return LookAhead(offset).getKind(); 
        }

        bool consume(tok::TokenKind ExpectedTok) {
            if (!expect(ExpectedTok)) {
                return false;
            }
            advance();
            return true;
        }

        inline void advance() {
            if (isParserProcess()) {
                llvm::outs() << lookahead[curIdx];
            }
            curIdx++;
            if (isParserProcess() && curIdx == lookahead.size()) {
                curIdx = 0;
                lookahead.clear();
                clearMemo();
            }
            sync(1);
        }

        inline bool expect(tok::TokenKind ExpectedTok) {
            if (curToken().getKind() != ExpectedTok) {
                errorReport(ExpectedTok, curToken().getKind());
                return false;
            }
            return true;
        }


        // 从缓冲区中获取当前解析或者分析token的第i个后面的token
        const Token& LookAhead(int offset = 1) { 
            sync(offset); 
            return lookahead[curIdx+offset-1]; 
        }

        inline void speculateMark() {
            markers.push_back(curIdx);
        }

        inline void speculateRollBack() {
            auto prev = markers.back();
            markers.pop_back();
            seek(prev);
        }

        inline void seek(int idx) {
            curIdx = idx;
        }
        inline const Token& curToken() {
            return lookahead[curIdx];
        }

        bool isPositionSpeculated(llvm::DenseMap<int, int>& memoization, int idx) {
            auto iter = memoization.find(idx);
            return (iter != memoization.end() && iter->getSecond() != FAILED);
        }

        void memoize(llvm::DenseMap<int, int>& memoization, int startIdx, bool failed) {
            auto stopIdx = failed ? FAILED : curIdx;
            memoization[startIdx] = stopIdx;
        }

        virtual void clearMemo(void) = 0;

    private:
        // Make sure we have i tokens from current position p
        // prefretch several token to lookahead member from lexer if it is not enough
        void sync(int i) {
            int fetchNumber = curIdx + i - lookahead.size();
            for (int i = 0; i<fetchNumber; ++i) {
                Token tok;
                lex.next(tok);
                lookahead.push_back(std::move(tok));
            }
        }

        // Whether this is a speculating process
        inline bool isSpeculatingProcess() { 
            return !markers.empty(); 
        }

        // Whether this is a parser process
        inline bool isParserProcess() { 
            return markers.empty(); 
        }
        void errorReport(tok::TokenKind ExpectedTok, tok::TokenKind actualTok) {
            auto ExpctSpel = tok::getSpelling(ExpectedTok);
            auto expectStr = (ExpctSpel == nullptr) ? 
                                tok::getTokenName(ExpectedTok) : ExpctSpel;
            
            auto ActualSpelling = tok::getSpelling(actualTok);
            auto actualStr = (ActualSpelling == nullptr) ? 
                                tok::getTokenName(actualTok) : ActualSpelling;

            diags.report(curToken().getLocation(), 
                            diag::err_expected, expectStr, actualStr);
        }
    private:
        size_t curIdx;
        SmallVector<Token, 10> lookahead;

        // Structure for speculation, stack of speculate branch
        // key: Position among buffer
        SmallVector<int, 10> markers;

        Lexer& lex;
    protected:
        DiagnosticsEngine &diags;
        
    };
}
#endif