set(LLVM_LINK_COMPONENTS support)

add_tinylang_library(tinylangParser
  TinylangParser.cpp

  LINK_LIBS
  tinylangBasic
  )