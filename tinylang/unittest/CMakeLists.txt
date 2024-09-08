project(gtest_test)
add_definitions("-fno-rtti")

enable_testing()

add_executable(${PROJECT_NAME} 
                main.cpp
                DiagnosticTest.cpp
                LexerTest.cpp
                ParseTest.cpp
                )
target_link_libraries(${PROJECT_NAME}
    PRIVATE
    gmock 
    gtest 
    pthread
    tinylangBasic
    tinylangLexer
    tinylangParser
    tinylangSema
    ${LLVM_COMMON_LIBS}
    )

add_test(NAME ${PROJECT_NAME}
         COMMAND ${PROJECT_NAME})