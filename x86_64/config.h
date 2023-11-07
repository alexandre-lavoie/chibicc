#ifndef CONFIG_H
#define CONFIG_H

/**
 * Sizes
 */

#define NUM_BYTE_AT_ADDRESS 1
#define STACK_ALIGN 16
#define FRAME_OFFSET 16
#define FRAME_ALIGN 8

#define TY_BOOL_SIZE 1
#define TY_BOOL_ALIGN TY_BOOL_SIZE

#define TY_CHAR_SIZE 1
#define TY_CHAR_ALIGN TY_CHAR_SIZE

#define TY_SHORT_SIZE 2
#define TY_SHORT_ALIGN TY_SHORT_SIZE

#define TY_INT_SIZE 4
#define TY_INT_ALIGN TY_INT_SIZE

#define TY_LONG_SIZE 8
#define TY_LONG_ALIGN TY_LONG_SIZE

#define TY_FLOAT_SIZE 4
#define TY_FLOAT_ALIGN TY_FLOAT_SIZE

#define TY_DOUBLE_SIZE 8
#define TY_DOUBLE_ALIGN TY_DOUBLE_SIZE

#define TY_LDOUBLE_SIZE 16
#define TY_LDOUBLE_ALIGN TY_LDOUBLE_SIZE

#define TY_PTR_SIZE 8
#define TY_PTR_ALIGN TY_PTR_SIZE

/**
 * Registers
 */

#define GP_REF "%"

#define GP_MAX 6
#define FP_MAX 8

#define GP_RET "rda"

#define GP_0 GP_RET
#define GP_1 "rdi"

#endif /* CONFIG_H */
