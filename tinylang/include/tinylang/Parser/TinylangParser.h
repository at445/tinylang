#ifndef TINYLANG_PARSER_H
#define TINYLANG_PARSER_H

#include "tinylang/Parser/ParserBase.h"

namespace tinylang {
    class Paser : public ParserBase {
        void clearMemo(void) override {
            memoization.clear();
        }
    private:
        // Structure for speculation
        // key: start Position among buffer
        // value: end Position among buffer 
        //          means how far the speculation has reached
        //          if this is -1, means speculate process is usless
        llvm::DenseMap<int, int> memoization;

        bool parseCompilationUnit(void);
    };
}
#endif
