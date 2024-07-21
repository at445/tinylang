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
        ParserBase(Lexer &lexer) 
            :lex(lexer), curIdx(0){
            sync(1);
        }
    protected:
        tok::TokenKind LookAheadTyp(int offset = 1) {
             return LookAhead(offset).getKind(); 
        }

        // return: this error happened during parsering
        bool consume(tok::TokenKind ExpectedTok) {
            if (LookAheadTyp() == ExpectedTok) {
                consume();
                return false;
            }
            return true;
        }

        void consume() {
            curIdx++;
            if (isParserProcess() && curIdx == lookahead.size()) {
                curIdx = 0;
                lookahead.clear();
                clearMemo();
            }
            sync(1);
        }


        // 从缓冲区中获取当前解析或者分析token的第i个后面的token
        const Token& LookAhead(int offset) { 
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
            auto fetchNumber = curIdx + i - lookahead.size();
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
    private:
        int curIdx;
        SmallVector<Token, 10> lookahead;

        // Structure for speculation, stack of speculate branch
        // key: Position among buffer
        SmallVector<int, 10> markers;

        Lexer& lex;
        
    };
}
#endif