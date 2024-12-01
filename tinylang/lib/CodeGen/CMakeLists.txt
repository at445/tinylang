set(LLVM_LINK_COMPONENTS support)

add_tinylang_library(tinylangCodeGen
  CGModule.cpp
  CGProcedure.cpp
  
  LINK_LIBS
  tinylangBasic
)