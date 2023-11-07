#include "chibicc.h"
#include "../codegen.h"

const static char *regs[] = { "A", "B" };

/**
 * Stack
 */

void push(void) {
  println("  SET PUSH, A");
  depth++;
}

void pushf(void) {
  // TODO: Push float
  push();
}

void pushld(void) {
  // TODO: Push long double
  push();
}

void pop(const char *arg) {
  println("  SET %s, POP", arg);
  depth--;
}

void popf(int reg) {
  // TODO: Pop float
  println("  SET %s, POP", regs[reg]);
  depth--;
}

/**
 * Jump
 */

void emit_jump_label(const char *label) {
  println("  SET PC, %s", label);
}

void emit_jump_reg(void) {
  println("  SET PC, A");
}

void emit_jump_zero(const Type *ty, const char *label) {
  // TODO: Type

  println("  IFE A, 0");
  println("  SET PC, %s", label);
}

void emit_jump_nzero(const Type *ty, const char *label) {
  // TODO: Type

  println("  IFN A, 0");
  println("  SET PC, %s", label);
}

/**
 * Function
 */

void emit_prologue(const Obj *fn) {
  println("  SET PUSH, X");
  println("  SET X, SP");
  println("  SUB SP, %d", fn->stack_size);
  println("  SET [X+%d], SP", fn->alloca_bottom->offset);
}

void emit_epilogue(const Obj *fn) {
  println(".L.return.%s:", fn->name);
  println("  SET SP, X");
  println("  SET X, POP");
  println("  SET PC, POP");
}

void emit_va_area(int gp, int fp, int off) {
  // TODO
  unreachable();
}

void emit_funcall(const Node *node) {
  int stack_args = push_args(node);
  gen_expr(node->lhs);

  int gp = 0, fp = 0;
  if (node->ret_buffer && node->ty->size > 16) pop(regs[gp++]);

  depth -= stack_args;

  println("  JSR A");
}

void copy_ret_buffer(const Obj *var) {
  // TODO
  unreachable();
}

void store_fp(int r, int offset, int sz) {
  // TODO
  unreachable();
}

void store_gp(int r, int offset, int sz) {
  // TODO
  unreachable();
}

/**
 * Struct 
 */

void push_struct(const Type *ty) {
  depth += ty->size / NUM_BYTE_AT_ADDRESS;
  
  for (int i = 0; i < ty->size / NUM_BYTE_AT_ADDRESS; i++) {
    println("  SET PUSH, [A+%d]", i);
  }
}

void copy_struct_reg(void) {
  // TODO
  unreachable();
}

void copy_struct_mem(void) {
  // TODO
  unreachable();
}

/**
 * Negate
 */

void emit_neg(void) {
  // TODO: Confirm logic
  println("  MLI A, -1");
}

void emit_negf(void) {
  // TODO
  emit_neg();
}

void emit_negd(void) {
  // TODO
  emit_neg();
}

void emit_negld(void) {
  // TODO
  emit_neg();
}

/**
 * Load
 */

void load8(bool usig) {
  // TODO
  load16(usig);
}

void load16(bool usig) {
  println("  SET A, [A]");
}

void load32(void) {
  // TODO
  load16(false);
}

void load64(void) {
  // TODO
  load16(false);
}

void loadf(void) {
  // TODO
  load16(false);
}

void loadd(void) {
  // TODO
  load16(false);
}

void loadld(void) {
  // TODO
  load16(false);
}

/**
 * Store
 */

void store8(void) {
  // TODO
  store16();
}

void store16(void) {
  println("  SET [B], A");
}

void store32(void) {
  // TODO
  store16();
}

void store64(void) {
  // TODO
  store16();
}

void storef(void) {
  // TODO
  store16();
}

void stored(void) {
  // TODO
  store16();
}

void storeld(void) {
  // TODO
  store16();
}

void stores(int sz) {
  for (int i = 0; i < sz / NUM_BYTE_AT_ADDRESS; i++) {
    println("  SET [B+%i], [A+%i]", i);
  }
}

/**
 * Immediate
 */

void emit_num64(int64_t val) {
  println("  SET A, %d", val);
}

void emit_numf(long double fval) {
  // TODO
  println("  SET A, %d", (int)fval);
}

void emit_numd(long double fval) {
  // TODO
  println("  SET A, %d", (int)fval);
}

void emit_numld(long double fval) {
  // TODO
  println("  SET A, %d", (int)fval);
}

/**
 * Addition
 */

void emit_add(void) {
  println("  ADD A, B");
}

void emit_addl(void) {
  // TODO
  emit_add();
}

void emit_addf(void) {
  // TODO
  emit_add();
}

void emit_addd(void) {
  // TODO
  emit_add();
}

void emit_addld(void) {
  // TODO
  emit_add();
}

/**
 * Subtraction
 */

void emit_sub(void) {
  println("  SUB A, B");
}

void emit_subl(void) {
  // TODO
  emit_sub();
}

void emit_subf(void) {
  // TODO
  emit_sub();
}

void emit_subd(void) {
  // TODO
  emit_sub();
}

void emit_subld(void) {
  // TODO
  emit_sub();
}

/**
 * Multiplication
 */

void emit_mul(void) {
  println("  MLI A, B");
}

void emit_mull(void) {
  // TODO
  emit_mul();
}

void emit_mulf(void) {
  // TODO
  emit_mul();
}

void emit_muld(void) {
  // TODO
  emit_mul();
}

void emit_mulld(void) {
  // TODO
  emit_mul();
}

/**
 * Division
 */

void emit_div(bool usig, bool is_8) {
  println(usig ? "  DIV A, B" : "  DVI A, B");
}

void emit_divl(bool usig, bool is_8) {
  // TODO
  emit_div(usig, is_8);
}

void emit_divf(void) {
  // TODO
  emit_div(false, false);
}

void emit_divd(void) {
  // TODO
  emit_div(false, false);
}

void emit_divld(void) {
  // TODO
  emit_div(false, false);
}

/**
 * Equal
 */

void emit_eq(void) {
  println("  SET C, 0");
  println("  IFE A, B");
  println("  SET C, 1");
  println("  SET A, C");
}

void emit_eql(void) {
  // TODO
  emit_eq();
}

void emit_eqf(void) {
  // TODO
  emit_eq();
}

void emit_eqd(void) {
  // TODO
  emit_eq();
}

void emit_eqld(void) {
  // TODO
  emit_eq();
}

/**
 * Not Equal
 */

void emit_neq(void) {
  println("  SET C, 0");
  println("  IFN A, B");
  println("  SET C, 1");
  println("  SET A, C");
}

void emit_neql(void) {
  // TODO
  emit_neq();
}

void emit_neqf(void) {
  // TODO
  emit_neq();
}

void emit_neqd(void) {
  // TODO
  emit_neq();
}

void emit_neqld(void) {
  // TODO
  emit_neq();
}

/**
 * Less Than
 */

void emit_lt(bool usig) {
  println("  SET C, 0");
  println(usig ? "  IFU A, B" : "  IFL A, B");
  println("  SET C, 1");
  println("  SET A, C");
}

void emit_ltl(bool usig) {
  // TODO
  emit_lt(usig);
}

void emit_ltf(void) {
  // TODO
  emit_lt(false);
}

void emit_ltd(void) {
  // TODO
  emit_lt(false);
}

void emit_ltld(void) {
  // TODO
  emit_lt(false);
}

/**
 * Less Than Equal
 */

void emit_le(bool usig) {
  println("  SET C, 0");
  println(usig ? "  IFU A, B" : "  IFL A, B");
  println("  SET C, 1");
  println("  IFE A, B");
  println("  SET C, 1");
  println("  SET A, C");
}

void emit_lel(bool usig) {
  // TODO
  emit_le(usig);
}

void emit_lef(void) {
  // TODO
  emit_le(false);
}

void emit_led(void) {
  // TODO
  emit_le(false);
}

void emit_leld(void) {
  // TODO
  emit_le(false);
}

/**
 * Modulo
 */

void emit_mod(bool usig, bool is_8) {
  println(usig ? "  MOD A, B" : "  MDI A, B");
}

void emit_modl(bool usig, bool is_8) {
  // TODO
  emit_mod(usig, is_8);
}

/**
 * Not
 */

void emit_not(void) {
  // TODO: Check logic
  println("  XOR A, 0xFFFF");
}

void emit_lnot(const Type *ty) {
  // TODO
  emit_not();
}

/**
 * And
 */

void emit_and(void) {
  println("  AND A, B");
}

void emit_andl(void) {
  // TODO
  emit_and();
}

/**
 * Or
 */

void emit_or(void) {
  println("  BOR A, B");
}

void emit_orl(void) {
  // TODO
  emit_or();
}

/**
 * Xor
 */

void emit_xor(void) {
  println("  XOR A, B");
}

void emit_xorl(void) {
  // TODO
  emit_xor();
}

/**
 * Shift Left
 */

void emit_shl(void) {
  println("  SHL A, B");
}

void emit_shll(void) {
  // TODO
  emit_shl();
}

/**
 * Shift Right
 */

void emit_shr(bool usig) {
  println(usig ? "  SHR A, B" : "  ASR A, B");
}

void emit_shrl(bool usig) {
  // TODO
  emit_shr(usig);
}

/**
 * Bitfield
 */

void emit_member_bitfield(const Member *mem) {
  // TODO
  unreachable();
}

void emit_assign_bitfield(const Node *node) {
  // TODO
  unreachable();
}

/**
 * Threading
 */

void emit_exch(int sz) {
  // TODO
  unreachable();
}

void emit_cas(const Node *node) {
  // TODO
  unreachable();
}

/**
 * Flow
 */

void emit_switch_cases(const Node *node) {
  for (Node *n = node->case_next; n; n = n->case_next) {
    if (n->begin == n->end) {
      println("  IFE A, %ld", n->begin);
      println("  SET PC, %s", n->label);
      continue;
    }

    // TODO: Check
    println("  SET B, A");
    println("  SUB B, %ld", n->begin);
    println("  IFL B, %ld", n->end - n->begin);
    println("  SET PC, %s", n->label);
    println("  IFE B, %ld", n->end - n->begin);
    println("  SET PC, %s", n->label);
  }
}

/**
 * Memory
 */

void emit_memzero(unsigned char size, int offset) {
  if (size == 0) return;

  int c = count();

  println(".memzero.%d.start:", c);
  println("  SET Y, X");
  println("  ADD Y, %d", offset);

  println("  SET Z, X");
  println("  ADD Z, %d", offset - MAX(size / NUM_BYTE_AT_ADDRESS, 1));

  println(".memzero.%d.loop:", c);
  println("  SET [Y], 0");
  println("  SUB Y, 1");
  println("  IFN Y, Z");
  println("  SET PC, .memzero.%d.loop", c);
  println(".memzero.%d.end:", c);
}

void emit_label_val(const char *label) {
  println("  SET A, %s", label);
}

void gen_addr(const Node *node) {
  switch(node->kind) {
  case ND_VAR:
    if (node->var->is_local) {
      println("  SET A, X");
      println("  ADD A, %d", node->var->offset);
      return;
    } else if (node->var->is_static) {
      println("  SET A, %s", node->var->name);
      return;
    } else if (node->ty->kind == TY_FUNC) {
      if (node->var->is_definition)
        println("  SET A, %s", node->var->name);
      else
        error_tok(node->tok, "TODO GOTPCREL");
      return;
    }

    break;
  case ND_MEMBER:
    gen_addr(node->lhs);
    println("  ADD A, %d", node->member->offset);
    return;
  case ND_DEREF:
    gen_expr(node->lhs);
    return;
  }

  printf("gen_addr: %d\n", node->kind);

  error_tok(node->tok, "invalid lvalue");
}

void cast(const Type *from, const Type *to) {
  // TODO
}

void emit_ret_fun(int offset) {
  println("  SET B, X");
  println("  ADD B, %d", offset);
  println("  SET A, [B]");
}
