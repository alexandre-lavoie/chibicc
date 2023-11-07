cmake_minimum_required(VERSION 3.0)
project(chibicc)

set(SRCS
    codegen.c
    hashmap.c
    main.c
    parse.c
    preprocess.c
    strings.c
    tokenize.c
    type.c
    unicode.c
)

add_executable(dcpu16-linux-cc
    ${SRCS}
    dcpu16/codegen.c
)

target_include_directories(dcpu16-linux-cc PRIVATE . dcpu16)
target_compile_definitions(dcpu16-linux-cc PRIVATE DCPU16=1)
