#ifndef CONFIG_H
#define CONFIG_H

/**
 * Sizes
 */

#define NUM_BYTE_AT_ADDRESS 2
#define STACK_ALIGN 1
#define FRAME_OFFSET 2
#define FRAME_ALIGN 1

#define TY_BOOL_SIZE 1
#define TY_BOOL_ALIGN 1

#define TY_CHAR_SIZE 1
#define TY_CHAR_ALIGN 1

#define TY_SHORT_SIZE 2
#define TY_SHORT_ALIGN 1

#define TY_INT_SIZE 2
#define TY_INT_ALIGN 1

#define TY_LONG_SIZE 2
#define TY_LONG_ALIGN 1

#define TY_FLOAT_SIZE 2
#define TY_FLOAT_ALIGN 1

#define TY_DOUBLE_SIZE 2
#define TY_DOUBLE_ALIGN 1

#define TY_LDOUBLE_SIZE 2
#define TY_LDOUBLE_ALIGN 1

#define TY_PTR_SIZE 2
#define TY_PTR_ALIGN 1

/**
 * Registers
 */

#define GP_REF ""

#define GP_MAX 0
#define FP_MAX 0

#define GP_RET "A"

#define GP_0 GP_RET
#define GP_1 "B"

#endif /* CONFIG_H */
