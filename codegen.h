#pragma once

#include "chibicc.h"

/**
 * Externs
 */

extern int depth;
extern Obj *current_fn;

/**
 * Common
 */

void gen_stmt(const Node *node);
void gen_expr(const Node *node);

__attribute__((format(printf, 1, 2)))
void println(char *fmt, ...);

int count(void);

bool has_flonum1(const Type *ty);
bool has_flonum2(const Type *ty);

int push_args(const Node *node);

/**
 * Stack
 */

void push(void);
void pushf(void);
void pushld(void);

void pop(const char *arg);
void popf(int reg);

/**
 * Jump
 */

void emit_jump_label(const char *label);
void emit_jump_reg(void);
void emit_jump_zero(const Type *ty, const char *label);
void emit_jump_nzero(const Type *ty, const char *label);

/**
 * Function
 */

void emit_prologue(const Obj *fn);
void emit_epilogue(const Obj *fn);
void emit_va_area(int gp, int fp, int off);

void emit_funcall(const Node *node);

void copy_ret_buffer(const Obj *var);

void store_fp(int r, int offset, int sz);
void store_gp(int r, int offset, int sz);

/**
 * Struct 
 */

void push_struct(const Type *ty);
void copy_struct_reg(void);
void copy_struct_mem(void);

/**
 * Negate
 */

void emit_neg(void);
void emit_negf(void);
void emit_negd(void);
void emit_negld(void);

/**
 * Load
 */

void load8(bool usig);
void load16(bool usig);
void load32(void);
void load64(void);

void loadf(void);
void loadd(void);
void loadld(void);

/**
 * Store
 */

void store8(void);
void store16(void);
void store32(void);
void store64(void);

void storef(void);
void stored(void);
void storeld(void);

void stores(int sz);

/**
 * Immediate
 */

void emit_num64(int64_t val);

void emit_numf(long double fval);
void emit_numd(long double fval);
void emit_numld(long double fval);

/**
 * Addition
 */

void emit_add(void);
void emit_addl(void);
void emit_addf(void);
void emit_addd(void);
void emit_addld(void);

/**
 * Subtraction
 */

void emit_sub(void);
void emit_subl(void);
void emit_subf(void);
void emit_subd(void);
void emit_subld(void);

/**
 * Multiplication
 */

void emit_mul(void);
void emit_mull(void);
void emit_mulf(void);
void emit_muld(void);
void emit_mulld(void);

/**
 * Division
 */

void emit_div(bool sig, bool is_8);
void emit_divl(bool sig, bool is_8);
void emit_divf(void);
void emit_divd(void);
void emit_divld(void);

/**
 * Equal
 */

void emit_eq(void);
void emit_eql(void);
void emit_eqf(void);
void emit_eqd(void);
void emit_eqld(void);

/**
 * Not Equal
 */

void emit_neq(void);
void emit_neql(void);
void emit_neqf(void);
void emit_neqd(void);
void emit_neqld(void);

/**
 * Less Than
 */

void emit_lt(bool usig);
void emit_ltl(bool usig);
void emit_ltf(void);
void emit_ltd(void);
void emit_ltld(void);

/**
 * Less Than Equal
 */

void emit_le(bool usig);
void emit_lel(bool usig);
void emit_lef(void);
void emit_led(void);
void emit_leld(void);

/**
 * Modulo
 */

void emit_mod(bool sig, bool is_8);
void emit_modl(bool sig, bool is_8);

/**
 * Not
 */

void emit_not(void);

void emit_lnot(const Type *ty);

/**
 * And
 */

void emit_and(void);
void emit_andl(void);
/**
 * Or
 */

void emit_or(void);
void emit_orl(void);
/**
 * Xor
 */

void emit_xor(void);
void emit_xorl(void);

/**
 * Shift Left
 */

void emit_shl(void);
void emit_shll(void);

/**
 * Shift Right
 */

void emit_shr(bool usig);
void emit_shrl(bool usig);

/**
 * Bitfield
 */

void emit_member_bitfield(const Member *mem);
void emit_assign_bitfield(const Node *node);

/**
 * Threading
 */

void emit_exch(int sz);
void emit_cas(const Node *node);

/**
 * Flow
 */

void emit_switch_cases(const Node *node);

/**
 * Memory
 */

void emit_memzero(unsigned char size, int offset);

void emit_label_val(const char *label);

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
void gen_addr(const Node *node);

void cast(const Type *from, const Type *to);

void emit_align_fun(void);

void emit_ret_fun(int offset);
