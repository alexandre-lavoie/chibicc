#include "chibicc.h"
#include "../codegen.h"

static char *argreg8[] = {"%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"};
static char *argreg16[] = {"%di", "%si", "%dx", "%cx", "%r8w", "%r9w"};
static char *argreg32[] = {"%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"};
static char *argreg64[] = {"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"};

static char *reg_dx(int sz) {
  switch (sz) {
  case 1: return "%dl";
  case 2: return "%dx";
  case 4: return "%edx";
  case 8: return "%rdx";
  }
  unreachable();
}

static char *reg_ax(int sz) {
  switch (sz) {
  case 1: return "%al";
  case 2: return "%ax";
  case 4: return "%eax";
  case 8: return "%rax";
  }
  unreachable();
}

static void cmp_zero(const Type *ty) {
  switch (ty->kind) {
  case TY_FLOAT:
    println("  xorps %%xmm1, %%xmm1");
    println("  ucomiss %%xmm1, %%xmm0");
    return;
  case TY_DOUBLE:
    println("  xorpd %%xmm1, %%xmm1");
    println("  ucomisd %%xmm1, %%xmm0");
    return;
  case TY_LDOUBLE:
    println("  fldz");
    println("  fucomip");
    println("  fstp %%st(0)");
    return;
  }

  if (is_integer(ty) && ty->size <= 4)
    println("  cmp $0, %%eax");
  else
    println("  cmp $0, %%rax");
}

static void builtin_alloca(void) {
  // Align size to 16 bytes.
  println("  add $15, %%rdi");
  println("  and $0xfffffff0, %%edi");

  // Shift the temporary area by %rdi.
  println("  mov %d(%%rbp), %%rcx", current_fn->alloca_bottom->offset);
  println("  sub %%rsp, %%rcx");
  println("  mov %%rsp, %%rax");
  println("  sub %%rdi, %%rsp");
  println("  mov %%rsp, %%rdx");
  println("1:");
  println("  cmp $0, %%rcx");
  println("  je 2f");
  println("  mov (%%rax), %%r8b");
  println("  mov %%r8b, (%%rdx)");
  println("  inc %%rdx");
  println("  inc %%rax");
  println("  dec %%rcx");
  println("  jmp 1b");
  println("2:");

  // Move alloca_bottom pointer.
  println("  mov %d(%%rbp), %%rax", current_fn->alloca_bottom->offset);
  println("  sub %%rdi, %%rax");
  println("  mov %%rax, %d(%%rbp)", current_fn->alloca_bottom->offset);
}

/**
 * Stack
 */

void push(void) {
  println("  push %%" GP_0);
  depth++;
}

void pushf(void) {
  println("  sub $8, %%rsp");
  println("  movsd %%xmm0, (%%rsp)");
  depth++;
}

void pushld(void) {
  println("  sub $16, %%rsp");
  println("  fstpt (%%rsp)");
  depth += 2;
}

void pop(const char *arg) {
  println("  pop %s", arg);
  depth--;
}

void popf(int reg) {
  println("  movsd (%%rsp), %%xmm%d", reg);
  println("  add $8, %%rsp");
  depth--;
}

/**
 * Jump
 */

void emit_jump_label(const char *label) {
  println("  jmp %s", label);
}

void emit_jump_reg(void) {
  println("  jmp *%%rax");
}

void emit_jump_zero(const Type *ty, const char *label) {
  cmp_zero(ty);
  println("  je %d", label);
}

void emit_jump_nzero(const Type *ty, const char *label) {
  cmp_zero(ty);
  println("  jne %d", label);
}

/**
 * Function
 */

void emit_prologue(const Obj *fn) {
  println("  push %%rbp");
  println("  mov %%rsp, %%rbp");
  println("  sub $%d, %%rsp", fn->stack_size);
  println("  mov %%rsp, %d(%%rbp)", fn->alloca_bottom->offset);
}

void emit_epilogue(const Obj *fn) {
  println(".L.return.%s:", fn->name);
  println("  mov %%rbp, %%rsp");
  println("  pop %%rbp");
  println("  ret");
}

void emit_va_area(int gp, int fp, int off) {
  // va_elem
  println("  movl $%d, %d(%%rbp)", gp * 8, off);          // gp_offset
  println("  movl $%d, %d(%%rbp)", fp * 8 + 48, off + 4); // fp_offset
  println("  movq %%rbp, %d(%%rbp)", off + 8);            // overflow_arg_area
  println("  addq $16, %d(%%rbp)", off + 8);
  println("  movq %%rbp, %d(%%rbp)", off + 16);           // reg_save_area
  println("  addq $%d, %d(%%rbp)", off + 24, off + 16);

  // __reg_save_area__
  println("  movq %%rdi, %d(%%rbp)", off + 24);
  println("  movq %%rsi, %d(%%rbp)", off + 32);
  println("  movq %%rdx, %d(%%rbp)", off + 40);
  println("  movq %%rcx, %d(%%rbp)", off + 48);
  println("  movq %%r8, %d(%%rbp)", off + 56);
  println("  movq %%r9, %d(%%rbp)", off + 64);
  println("  movsd %%xmm0, %d(%%rbp)", off + 72);
  println("  movsd %%xmm1, %d(%%rbp)", off + 80);
  println("  movsd %%xmm2, %d(%%rbp)", off + 88);
  println("  movsd %%xmm3, %d(%%rbp)", off + 96);
  println("  movsd %%xmm4, %d(%%rbp)", off + 104);
  println("  movsd %%xmm5, %d(%%rbp)", off + 112);
  println("  movsd %%xmm6, %d(%%rbp)", off + 120);
  println("  movsd %%xmm7, %d(%%rbp)", off + 128);
}

void emit_funcall(const Node *node) {
  if (node->lhs->kind == ND_VAR && !strcmp(node->lhs->var->name, "alloca")) {
    gen_expr(node->args);
    println("  mov %%rax, %%rdi");
    builtin_alloca();
    return;
  }

  int stack_args = push_args(node);
  gen_expr(node->lhs);

  int gp = 0, fp = 0;

  // If the return type is a large struct/union, the caller passes
  // a pointer to a buffer as if it were the first argument.
  if (node->ret_buffer && node->ty->size > 16)
    pop(argreg64[gp++]);

  for (Node *arg = node->args; arg; arg = arg->next) {
    Type *ty = arg->ty;

    switch (ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
      if (ty->size > 16)
        continue;

      bool fp1 = has_flonum1(ty);
      bool fp2 = has_flonum2(ty);

      if (fp + fp1 + fp2 < FP_MAX && gp + !fp1 + !fp2 < GP_MAX) {
        if (fp1)
          popf(fp++);
        else
          pop(argreg64[gp++]);

        if (ty->size > 8) {
          if (fp2)
            popf(fp++);
          else
            pop(argreg64[gp++]);
        }
      }
      break;
    case TY_FLOAT:
    case TY_DOUBLE:
      if (fp < FP_MAX)
        popf(fp++);
      break;
    case TY_LDOUBLE:
      break;
    default:
      if (gp < GP_MAX)
        pop(argreg64[gp++]);
    }
  }

  println("  mov %%rax, %%r10");
  println("  mov $%d, %%rax", fp);
  println("  call *%%r10");
  println("  add $%d, %%rsp", stack_args * 8);

  depth -= stack_args;

  // It looks like the most significant 48 or 56 bits in RAX may
  // contain garbage if a function return type is short or bool/char,
  // respectively. We clear the upper bits here.
  switch (node->ty->kind) {
  case TY_BOOL:
    println("  movzx %%al, %%eax");
    return;
  case TY_CHAR:
    if (node->ty->is_unsigned)
      println("  movzbl %%al, %%eax");
    else
      println("  movsbl %%al, %%eax");
    return;
  case TY_SHORT:
    if (node->ty->is_unsigned)
      println("  movzwl %%ax, %%eax");
    else
      println("  movswl %%ax, %%eax");
    return;
  }

  // If the return type is a small struct, a value is returned
  // using up to two registers.
  if (node->ret_buffer && node->ty->size <= 16) {
    copy_ret_buffer(node->ret_buffer);
    println("  lea %d(%%rbp), %%rax", node->ret_buffer->offset);
  }
}

void copy_ret_buffer(const Obj *var) {
  Type *ty = var->ty;
  int gp = 0, fp = 0;

  if (has_flonum1(ty)) {
    assert(ty->size == 4 || 8 <= ty->size);
    if (ty->size == 4)
      println("  movss %%xmm0, %d(%%rbp)", var->offset);
    else
      println("  movsd %%xmm0, %d(%%rbp)", var->offset);
    fp++;
  } else {
    for (int i = 0; i < MIN(8, ty->size); i++) {
      println("  mov %%al, %d(%%rbp)", var->offset + i);
      println("  shr $8, %%rax");
    }
    gp++;
  }

  if (ty->size > 8) {
    if (has_flonum2(ty)) {
      assert(ty->size == 12 || ty->size == 16);
      if (ty->size == 12)
        println("  movss %%xmm%d, %d(%%rbp)", fp, var->offset + 8);
      else
        println("  movsd %%xmm%d, %d(%%rbp)", fp, var->offset + 8);
    } else {
      char *reg1 = (gp == 0) ? "%al" : "%dl";
      char *reg2 = (gp == 0) ? "%rax" : "%rdx";
      for (int i = 8; i < MIN(16, ty->size); i++) {
        println("  mov %s, %d(%%rbp)", reg1, var->offset + i);
        println("  shr $8, %s", reg2);
      }
    }
  }
}

void store_fp(int r, int offset, int sz) {
  switch (sz) {
  case 4:
    println("  movss %%xmm%d, %d(%%rbp)", r, offset);
    return;
  case 8:
    println("  movsd %%xmm%d, %d(%%rbp)", r, offset);
    return;
  }
  unreachable();
}

void store_gp(int r, int offset, int sz) {
  switch (sz) {
  case 1:
    println("  mov %s, %d(%%rbp)", argreg8[r], offset);
    return;
  case 2:
    println("  mov %s, %d(%%rbp)", argreg16[r], offset);
    return;
  case 4:
    println("  mov %s, %d(%%rbp)", argreg32[r], offset);
    return;
  case 8:
    println("  mov %s, %d(%%rbp)", argreg64[r], offset);
    return;
  default:
    for (int i = 0; i < sz; i++) {
      println("  mov %s, %d(%%rbp)", argreg8[r], offset + i);
      println("  shr $8, %s", argreg64[r]);
    }
    return;
  }
}

/**
 * Struct 
 */

void push_struct(const Type *ty) {
  int sz = align_to(ty->size, 8);
  println("  sub $%d, %%rsp", sz);
  depth += sz / 8;

  for (int i = 0; i < ty->size; i++) {
    println("  mov %d(%%rax), %%r10b", i);
    println("  mov %%r10b, %d(%%rsp)", i);
  }
}

void copy_struct_reg(void) {
  Type *ty = current_fn->ty->return_ty;
  int gp = 0, fp = 0;

  println("  mov %%rax, %%rdi");

  if (has_flonum(ty, 0, 8, 0)) {
    assert(ty->size == 4 || 8 <= ty->size);
    if (ty->size == 4)
      println("  movss (%%rdi), %%xmm0");
    else
      println("  movsd (%%rdi), %%xmm0");
    fp++;
  } else {
    println("  mov $0, %%rax");
    for (int i = MIN(8, ty->size) - 1; i >= 0; i--) {
      println("  shl $8, %%rax");
      println("  mov %d(%%rdi), %%al", i);
    }
    gp++;
  }

  if (ty->size > 8) {
    if (has_flonum(ty, 8, 16, 0)) {
      assert(ty->size == 12 || ty->size == 16);
      if (ty->size == 4)
        println("  movss 8(%%rdi), %%xmm%d", fp);
      else
        println("  movsd 8(%%rdi), %%xmm%d", fp);
    } else {
      char *reg1 = (gp == 0) ? "%al" : "%dl";
      char *reg2 = (gp == 0) ? "%rax" : "%rdx";
      println("  mov $0, %s", reg2);
      for (int i = MIN(16, ty->size) - 1; i >= 8; i--) {
        println("  shl $8, %s", reg2);
        println("  mov %d(%%rdi), %s", i, reg1);
      }
    }
  }
}

void copy_struct_mem(void) {
  Type *ty = current_fn->ty->return_ty;
  Obj *var = current_fn->params;

  println("  mov %d(%%rbp), %%rdi", var->offset);

  for (int i = 0; i < ty->size; i++) {
    println("  mov %d(%%rax), %%dl", i);
    println("  mov %%dl, %d(%%rdi)", i);
  }
}

/**
 * Negate
 */

void emit_neg(void) {
  println("  neg %%rax");
}

void emit_negf(void) {
  println("  mov $1, %%rax");
  println("  shl $31, %%rax");
  println("  movq %%rax, %%xmm1");
  println("  xorps %%xmm1, %%xmm0");
}

void emit_negd(void) {
  println("  mov $1, %%rax");
  println("  shl $63, %%rax");
  println("  movq %%rax, %%xmm1");
  println("  xorpd %%xmm1, %%xmm0");
}

void emit_negld(void) {
  println("  fchs");
}

/**
 * Load
 */

void load8(bool usig) {
  println("  %sbl (%%rax), %%eax", usig ? "movz" : "movs");
}

void load16(bool usig) {
  println("  %swl (%%rax), %%eax", usig ? "movz" : "movs");
}

void load32(void) {
  println("  movsxd (%%rax), %%rax");
}

void load64(void) {
  println("  mov (%%rax), %%rax");
}

void loadf(void) {
  println("  movss (%%rax), %%xmm0");
}

void loadd(void) {
  println("  movsd (%%rax), %%xmm0");
}

void loadld(void) {
  println("  fldt (%%rax)");
}

/**
 * Store
 */

void store8(void) {
  println("  mov %%al, (%%rdi)");
}

void store16(void) {
  println("  mov %%ax, (%%rdi)");
}

void store32(void) {
  println("  mov %%eax, (%%rdi)");
}

void store64(void) {
  println("  mov %%rax, (%%rdi)");
}

void storef(void) {
  println("  movss %%xmm0, (%%rdi)");
}

void stored(void) {
  println("  movsd %%xmm0, (%%rdi)");
}

void storeld(void) {
  println("  fstpt (%%rdi)");
}

void stores(int sz) {
  for (int i = 0; i < sz; i++) {
    println("  mov %d(%%rax), %%r8b", i);
    println("  mov %%r8b, %d(%%rdi)", i);
  }
}

/**
 * Immediate
 */

void emit_num64(int64_t val) {
  println("  mov $%ld, %%rax", val);
}

void emit_numf(long double fval) {
  union { float f32; uint32_t u32; } u = { fval };
  println("  mov $%u, %%eax  # float %Lf", u.u32, fval);
  println("  movq %%rax, %%xmm0");
}

void emit_numd(long double fval) {
  union { double f64; uint64_t u64; } u = { fval };
  println("  mov $%lu, %%rax  # double %Lf", u.u64, fval);
  println("  movq %%rax, %%xmm0"); 
}

void emit_numld(long double fval) {
  union { long double f80; uint64_t u64[2]; } u;
  memset(&u, 0, sizeof(u));
  u.f80 = fval;
  println("  mov $%lu, %%rax  # long double %Lf", u.u64[0], fval);
  println("  mov %%rax, -16(%%rsp)");
  println("  mov $%lu, %%rax", u.u64[1]);
  println("  mov %%rax, -8(%%rsp)");
  println("  fldt -16(%%rsp)");
}

/**
 * Addition
 */

void emit_add(void) {
  println("  add %edi, %eax");
}

void emit_addl(void) {
  println("  add %rdi, %rax");
}

void emit_addf(void) {
  println("  addss %%xmm1, %%xmm0");
}

void emit_addd(void) {
  println("  addsd %%xmm1, %%xmm0");
}

void emit_addld(void) {
  println("  faddp");
}

/**
 * Subtraction
 */

void emit_sub(void) {
  println("  sub %edi, %eax");
}

void emit_subl(void) {
  println("  sub %rdi, %rax");
}

void emit_subf(void) {
  println("  subss %%xmm1, %%xmm0");
}

void emit_subd(void) {
  println("  subsd %%xmm1, %%xmm0");
}

void emit_subld(void) {
  println("  fsubrp");
}

/**
 * Multiplication
 */

void emit_mul(void) {
  println("  imul %edi, %eax");
}

void emit_mull(void) {
  println("  imul %rdi, %rax");
}

void emit_mulf(void) {
  println("  mulss %%xmm1, %%xmm0");
}

void emit_muld(void) {
  println("  mulsd %%xmm1, %%xmm0");
}

void emit_mulld(void) {
  println("  fmulp");
}

/**
 * Division
 */

void emit_div(bool usig, bool is_8) {
  if (usig) {
    println("  mov $0, %edx");
    println("  div %edi");
  } else {
    if (is_8)
      println("  cqo");
    else
      println("  cdq");
    println("  idiv %edi");
  }
}

void emit_divl(bool usig, bool is_8) {
  if (usig) {
    println("  mov $0, %rdx");
    println("  div %rdi");
  } else {
    if (is_8)
      println("  cqo");
    else
      println("  cdq");
    println("  idiv %rdi");
  }
}

void emit_divf(void) {
  println("  divss %%xmm1, %%xmm0");
}

void emit_divd(void) {
  println("  divsd %%xmm1, %%xmm0");
}

void emit_divld() {
  println("  fdivrp");
}

/**
 * Equal
 */

void emit_eq(void) {
  println("  cmp %edi, %eax");

  println("  sete %%al");

  println("  movzb %%al, %%rax");
}

void emit_eql(void) {
  println("  cmp %rdi, %rax");

  println("  sete %%al");

  println("  movzb %%al, %%rax");
}

void emit_eqf(void) {
  println("  ucomiss %%xmm0, %%xmm1");

  println("  sete %%al");
  println("  setnp %%dl");
  println("  and %%dl, %%al");

  println("  and $1, %%al");
  println("  movzb %%al, %%rax");
}

void emit_eqd(void){
  println("  ucomisd %%xmm0, %%xmm1");

  println("  sete %%al");
  println("  setnp %%dl");
  println("  and %%dl, %%al");

  println("  and $1, %%al");
  println("  movzb %%al, %%rax");
}

void emit_eqld(void) {
  println("  fcomip");
  println("  fstp %%st(0)");

  println("  sete %%al");

  println("  movzb %%al, %%rax");
}

/**
 * Not Equal
 */

void emit_neq(void) {
  println("  cmp %edi, %eax");

  println("  setne %%al");

  println("  movzb %%al, %%rax");
}

void emit_neql(void) {
  println("  cmp %rdi, %rax");

  println("  setne %%al");

  println("  movzb %%al, %%rax");
}

void emit_neqf(void) {
  println("  ucomiss %%xmm0, %%xmm1");

  println("  setne %%al");
  println("  setp %%dl");
  println("  or %%dl, %%al");

  println("  and $1, %%al");
  println("  movzb %%al, %%rax");
}

void emit_neqd(void) {
  println("  ucomisd %%xmm0, %%xmm1");

  println("  setne %%al");
  println("  setp %%dl");
  println("  or %%dl, %%al");

  println("  and $1, %%al");
  println("  movzb %%al, %%rax");
}

void emit_neqld(void) {
  println("  fcomip");
  println("  fstp %%st(0)");

  println("  setne %%al");

  println("  movzb %%al, %%rax");
}

/**
 * Less Than
 */

void emit_lt(bool usig) {
  println("  cmp %edi, %eax");

  if (usig) println("  setb %%al");
  else println("  setl %%al");

  println("  movzb %%al, %%rax");
}

void emit_ltl(bool usig) {
  println("  cmp %rdi, %rax");

  if (usig) println("  setb %%al");
  else println("  setl %%al");

  println("  movzb %%al, %%rax");
}

void emit_ltf(void) {
  println("  ucomiss %%xmm0, %%xmm1");

  println("  seta %%al");

  println("  and $1, %%al");
  println("  movzb %%al, %%rax");
}

void emit_ltd(void) {
  println("  ucomisd %%xmm0, %%xmm1");

  println("  seta %%al");

  println("  and $1, %%al");
  println("  movzb %%al, %%rax");
}

void emit_ltld(void) {
  println("  fcomip");
  println("  fstp %%st(0)");

  println("  seta %%al");

  println("  movzb %%al, %%rax");
}

/**
 * Less Than Equal
 */

void emit_le(bool usig) {
  println("  cmp %edi, %eax");

  if (usig) println("  setbe %%al");
  else println("  setle %%al");

  println("  movzb %%al, %%rax");
}

void emit_lel(bool usig) {
  println("  cmp %rdi, %rax");

  if (usig) println("  setbe %%al");
  else println("  setle %%al");

  println("  movzb %%al, %%rax");
}

void emit_lef(void) {
  println("  ucomiss %%xmm0, %%xmm1");

  println("  setae %%al");

  println("  and $1, %%al");
  println("  movzb %%al, %%rax");
}

void emit_led(void) {
  println("  ucomisd %%xmm0, %%xmm1");

  println("  setae %%al");

  println("  and $1, %%al");
  println("  movzb %%al, %%rax");
}

void emit_leld(void) {
  println("  fcomip");
  println("  fstp %%st(0)");

  println("  setae %%al");

  println("  movzb %%al, %%rax");
}

/**
 * Modulo
 */

void emit_mod(bool usig, bool is_8) {
  emit_div(usig, is_8);

  println("  mov %%rdx, %%rax");
}

void emit_modl(bool usig, bool is_8) {
  emit_divl(usig, is_8);

  println("  mov %%rdx, %%rax");
}

/**
 * Not
 */

void emit_not(void) {
  println("  not %%rax");
}

void emit_lnot(const Type *ty) {
  cmp_zero(ty);
  println("  sete %%al");
  println("  movzx %%al, %%rax");
}

/**
 * And
 */

void emit_and(void) {
  println("  and %edi, %eax");
}

void emit_andl(void) {
  println("  and %rdi, %rax");
}

/**
 * Or
 */

void emit_or(void) {
  println("  or %edi, %eax");
}

void emit_orl(void) {
  println("  or %rdi, %rax");
}

/**
 * Xor
 */

void emit_xor(void) {
  println("  xor %edi, %eax");
}

void emit_xorl(void) {
  println("  xor %rdi, %rax");
}

/**
 * Shift Left
 */

void emit_shl(void) {
  println("  mov %%rdi, %%rcx");
  println("  shl %%cl, %eax");
}

void emit_shll(void) {
  println("  mov %%rdi, %%rcx");
  println("  shl %%cl, %rax");
}

/**
 * Shift Right
 */

void emit_shr(bool usig) {
  println("  mov %%rdi, %%rcx");
  if (usig)
    println("  shr %%cl, %eax");
  else
    println("  sar %%cl, %eax");
}

void emit_shrl(bool usig) {
  println("  mov %%rdi, %%rcx");
  if (usig)
    println("  shr %%cl, %rax");
  else
    println("  sar %%cl, %rax");
}

/**
 * Bitfield
 */

void emit_member_bitfield(const Member *mem) {
  println("  shl $%d, %%rax", 64 - mem->bit_width - mem->bit_offset);
  if (mem->ty->is_unsigned)
    println("  shr $%d, %%rax", 64 - mem->bit_width);
  else
    println("  sar $%d, %%rax", 64 - mem->bit_width);
}

void emit_assign_bitfield(const Node *node) {
  println("  mov %%rax, %%r8");

  // If the lhs is a bitfield, we need to read the current value
  // from memory and merge it with a new value.
  Member *mem = node->lhs->member;
  println("  mov %%rax, %%rdi");
  println("  and $%ld, %%rdi", (1L << mem->bit_width) - 1);
  println("  shl $%d, %%rdi", mem->bit_offset);

  println("  mov (%%rsp), %%rax");
  load(mem->ty);

  long mask = ((1L << mem->bit_width) - 1) << mem->bit_offset;
  println("  mov $%ld, %%r9", ~mask);
  println("  and %%r9, %%rax");
  println("  or %%rdi, %%rax");
  store(node->ty);
  println("  mov %%r8, %%rax");
}

/**
 * Threading
 */

void emit_exch(int sz) {
  println("  xchg %s, (%%rdi)", reg_ax(sz));
}

void emit_cas(const Node *node) {
  gen_expr(node->cas_addr);
  push();
  gen_expr(node->cas_new);
  push();
  gen_expr(node->cas_old);
  println("  mov %%rax, %%r8");
  load(node->cas_old->ty->base);
  pop("%rdx"); // new
  pop("%rdi"); // addr

  int sz = node->cas_addr->ty->base->size;
  println("  lock cmpxchg %s, (%%rdi)", reg_dx(sz));
  println("  sete %%cl");
  println("  je 1f");
  println("  mov %s, (%%r8)", reg_ax(sz));
  println("1:");
  println("  movzbl %%cl, %%eax");
}

/**
 * Flow
 */

void emit_switch_cases(const Node *node) {
  char *ax = (node->cond->ty->size == 8) ? "%rax" : "%eax";
  char *di = (node->cond->ty->size == 8) ? "%rdi" : "%edi";

  for (Node *n = node->case_next; n; n = n->case_next) {
    if (n->begin == n->end) {
      println("  cmp $%ld, %s", n->begin, ax);
      println("  je %s", n->label);
      continue;
    }

    // [GNU] Case ranges
    println("  mov %s, %s", ax, di);
    println("  sub $%ld, %s", n->begin, di);
    println("  cmp $%ld, %s", n->end - n->begin, di);
    println("  jbe %s", n->label);
  }
}

/**
 * Memory
 */

void emit_memzero(unsigned char size, int offset) {
  // `rep stosb` is equivalent to `memset(%rdi, %al, %rcx)`.
  println("  mov $%d, %%rcx", size);
  println("  lea %d(%%rbp), %%rdi", offset);
  println("  mov $0, %%al");
  println("  rep stosb");
}

void emit_label_val(const char *label) {
  println("  lea %s(%%rip), %%rax", label);
}

void gen_addr(const Node *node) {
  switch (node->kind) {
  case ND_VAR:
    // Variable-length array, which is always local.
    if (node->var->ty->kind == TY_VLA) {
      println("  mov %d(%%rbp), %%rax", node->var->offset);
      return;
    }

    // Local variable
    if (node->var->is_local) {
      println("  lea %d(%%rbp), %%rax", node->var->offset);
      return;
    }

    if (opt_fpic) {
      // Thread-local variable
      if (node->var->is_tls) {
        println("  data16 lea %s@tlsgd(%%rip), %%rdi", node->var->name);
        println("  .value 0x6666");
        println("  rex64");
        println("  call __tls_get_addr@PLT");
        return;
      }

      // Function or global variable
      println("  mov %s@GOTPCREL(%%rip), %%rax", node->var->name);
      return;
    }

    // Thread-local variable
    if (node->var->is_tls) {
      println("  mov %%fs:0, %%rax");
      println("  add $%s@tpoff, %%rax", node->var->name);
      return;
    }

    // Here, we generate an absolute address of a function or a global
    // variable. Even though they exist at a certain address at runtime,
    // their addresses are not known at link-time for the following
    // two reasons.
    //
    //  - Address randomization: Executables are loaded to memory as a
    //    whole but it is not known what address they are loaded to.
    //    Therefore, at link-time, relative address in the same
    //    exectuable (i.e. the distance between two functions in the
    //    same executable) is known, but the absolute address is not
    //    known.
    //
    //  - Dynamic linking: Dynamic shared objects (DSOs) or .so files
    //    are loaded to memory alongside an executable at runtime and
    //    linked by the runtime loader in memory. We know nothing
    //    about addresses of global stuff that may be defined by DSOs
    //    until the runtime relocation is complete.
    //
    // In order to deal with the former case, we use RIP-relative
    // addressing, denoted by `(%rip)`. For the latter, we obtain an
    // address of a stuff that may be in a shared object file from the
    // Global Offset Table using `@GOTPCREL(%rip)` notation.

    // Function
    if (node->ty->kind == TY_FUNC) {
      if (node->var->is_definition)
        println("  lea %s(%%rip), %%rax", node->var->name);
      else
        println("  mov %s@GOTPCREL(%%rip), %%rax", node->var->name);
      return;
    }

    // Global variable
    println("  lea %s(%%rip), %%rax", node->var->name);
    return;
  case ND_DEREF:
    gen_expr(node->lhs);
    return;
  case ND_COMMA:
    gen_expr(node->lhs);
    gen_addr(node->rhs);
    return;
  case ND_MEMBER:
    gen_addr(node->lhs);
    println("  add $%d, %%rax", node->member->offset);
    return;
  case ND_FUNCALL:
    if (node->ret_buffer) {
      gen_expr(node);
      return;
    }
    break;
  case ND_ASSIGN:
  case ND_COND:
    if (node->ty->kind == TY_STRUCT || node->ty->kind == TY_UNION) {
      gen_expr(node);
      return;
    }
    break;
  case ND_VLA_PTR:
    println("  lea %d(%%rbp), %%rax", node->var->offset);
    return;
  }

  error_tok(node->tok, "not an lvalue");
}

// The table for type casts
static char i32i8[] = "movsbl %al, %eax";
static char i32u8[] = "movzbl %al, %eax";
static char i32i16[] = "movswl %ax, %eax";
static char i32u16[] = "movzwl %ax, %eax";
static char i32f32[] = "cvtsi2ssl %eax, %xmm0";
static char i32i64[] = "movsxd %eax, %rax";
static char i32f64[] = "cvtsi2sdl %eax, %xmm0";
static char i32f80[] = "mov %eax, -4(%rsp); fildl -4(%rsp)";

static char u32f32[] = "mov %eax, %eax; cvtsi2ssq %rax, %xmm0";
static char u32i64[] = "mov %eax, %eax";
static char u32f64[] = "mov %eax, %eax; cvtsi2sdq %rax, %xmm0";
static char u32f80[] = "mov %eax, %eax; mov %rax, -8(%rsp); fildll -8(%rsp)";

static char i64f32[] = "cvtsi2ssq %rax, %xmm0";
static char i64f64[] = "cvtsi2sdq %rax, %xmm0";
static char i64f80[] = "movq %rax, -8(%rsp); fildll -8(%rsp)";

static char u64f32[] = "cvtsi2ssq %rax, %xmm0";
static char u64f64[] =
  "test %rax,%rax; js 1f; pxor %xmm0,%xmm0; cvtsi2sd %rax,%xmm0; jmp 2f; "
  "1: mov %rax,%rdi; and $1,%eax; pxor %xmm0,%xmm0; shr %rdi; "
  "or %rax,%rdi; cvtsi2sd %rdi,%xmm0; addsd %xmm0,%xmm0; 2:";
static char u64f80[] =
  "mov %rax, -8(%rsp); fildq -8(%rsp); test %rax, %rax; jns 1f;"
  "mov $1602224128, %eax; mov %eax, -4(%rsp); fadds -4(%rsp); 1:";

static char f32i8[] = "cvttss2sil %xmm0, %eax; movsbl %al, %eax";
static char f32u8[] = "cvttss2sil %xmm0, %eax; movzbl %al, %eax";
static char f32i16[] = "cvttss2sil %xmm0, %eax; movswl %ax, %eax";
static char f32u16[] = "cvttss2sil %xmm0, %eax; movzwl %ax, %eax";
static char f32i32[] = "cvttss2sil %xmm0, %eax";
static char f32u32[] = "cvttss2siq %xmm0, %rax";
static char f32i64[] = "cvttss2siq %xmm0, %rax";
static char f32u64[] = "cvttss2siq %xmm0, %rax";
static char f32f64[] = "cvtss2sd %xmm0, %xmm0";
static char f32f80[] = "movss %xmm0, -4(%rsp); flds -4(%rsp)";

static char f64i8[] = "cvttsd2sil %xmm0, %eax; movsbl %al, %eax";
static char f64u8[] = "cvttsd2sil %xmm0, %eax; movzbl %al, %eax";
static char f64i16[] = "cvttsd2sil %xmm0, %eax; movswl %ax, %eax";
static char f64u16[] = "cvttsd2sil %xmm0, %eax; movzwl %ax, %eax";
static char f64i32[] = "cvttsd2sil %xmm0, %eax";
static char f64u32[] = "cvttsd2siq %xmm0, %rax";
static char f64i64[] = "cvttsd2siq %xmm0, %rax";
static char f64u64[] = "cvttsd2siq %xmm0, %rax";
static char f64f32[] = "cvtsd2ss %xmm0, %xmm0";
static char f64f80[] = "movsd %xmm0, -8(%rsp); fldl -8(%rsp)";

#define FROM_F80_1                                           \
  "fnstcw -10(%rsp); movzwl -10(%rsp), %eax; or $12, %ah; " \
  "mov %ax, -12(%rsp); fldcw -12(%rsp); "

#define FROM_F80_2 " -24(%rsp); fldcw -10(%rsp); "

static char f80i8[] = FROM_F80_1 "fistps" FROM_F80_2 "movsbl -24(%rsp), %eax";
static char f80u8[] = FROM_F80_1 "fistps" FROM_F80_2 "movzbl -24(%rsp), %eax";
static char f80i16[] = FROM_F80_1 "fistps" FROM_F80_2 "movzbl -24(%rsp), %eax";
static char f80u16[] = FROM_F80_1 "fistpl" FROM_F80_2 "movswl -24(%rsp), %eax";
static char f80i32[] = FROM_F80_1 "fistpl" FROM_F80_2 "mov -24(%rsp), %eax";
static char f80u32[] = FROM_F80_1 "fistpl" FROM_F80_2 "mov -24(%rsp), %eax";
static char f80i64[] = FROM_F80_1 "fistpq" FROM_F80_2 "mov -24(%rsp), %rax";
static char f80u64[] = FROM_F80_1 "fistpq" FROM_F80_2 "mov -24(%rsp), %rax";
static char f80f32[] = "fstps -8(%rsp); movss -8(%rsp), %xmm0";
static char f80f64[] = "fstpl -8(%rsp); movsd -8(%rsp), %xmm0";

static char *cast_table[][11] = {
  // i8   i16     i32     i64     u8     u16     u32     u64     f32     f64     f80
  {NULL,  NULL,   NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80}, // i8
  {i32i8, NULL,   NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80}, // i16
  {i32i8, i32i16, NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80}, // i32
  {i32i8, i32i16, NULL,   NULL,   i32u8, i32u16, NULL,   NULL,   i64f32, i64f64, i64f80}, // i64

  {i32i8, NULL,   NULL,   i32i64, NULL,  NULL,   NULL,   i32i64, i32f32, i32f64, i32f80}, // u8
  {i32i8, i32i16, NULL,   i32i64, i32u8, NULL,   NULL,   i32i64, i32f32, i32f64, i32f80}, // u16
  {i32i8, i32i16, NULL,   u32i64, i32u8, i32u16, NULL,   u32i64, u32f32, u32f64, u32f80}, // u32
  {i32i8, i32i16, NULL,   NULL,   i32u8, i32u16, NULL,   NULL,   u64f32, u64f64, u64f80}, // u64

  {f32i8, f32i16, f32i32, f32i64, f32u8, f32u16, f32u32, f32u64, NULL,   f32f64, f32f80}, // f32
  {f64i8, f64i16, f64i32, f64i64, f64u8, f64u16, f64u32, f64u64, f64f32, NULL,   f64f80}, // f64
  {f80i8, f80i16, f80i32, f80i64, f80u8, f80u16, f80u32, f80u64, f80f32, f80f64, NULL},   // f80
};

void cast(const Type *from, const Type *to) {
  if (to->kind == TY_VOID)
    return;

  if (to->kind == TY_BOOL) {
    cmp_zero(from);
    println("  setne %%al");
    println("  movzx %%al, %%eax");
    return;
  }

  int t1 = getTypeId(from);
  int t2 = getTypeId(to);
  if (cast_table[t1][t2])
    println("  %s", cast_table[t1][t2]);
}

void emit_ret_fun(int offset) {
  println("  lea %d(%%rbp), %%rax", offset);
}
