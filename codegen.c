#include "chibicc.h"
#include "codegen.h"

int depth = 0;
Obj *current_fn = 0;

static FILE *output_file = 0;
static int counter = 1;

__attribute__((format(printf, 1, 2)))
void println(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(output_file, fmt, ap);
  va_end(ap);
  fprintf(output_file, "\n");
}

int count(void) {
  return counter++;
}

// Round up `n` to the nearest multiple of `align`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
int align_to(int n, int align) {
  return (n + align - 1) / align * align;
}

enum { I8, I16, I32, I64, U8, U16, U32, U64, F32, F64, F80 };

int getTypeId(const Type *ty) {
  switch (ty->kind) {
  case TY_CHAR:
    return ty->is_unsigned ? U8 : I8;
  case TY_SHORT:
    return ty->is_unsigned ? U16 : I16;
  case TY_INT:
    return ty->is_unsigned ? U32 : I32;
  case TY_LONG:
    return ty->is_unsigned ? U64 : I64;
  case TY_FLOAT:
    return F32;
  case TY_DOUBLE:
    return F64;
  case TY_LDOUBLE:
    return F80;
  }
  return U64;
}

// Load a value from where %rax is pointing to.
void load(Type *ty) {
  switch (ty->kind) {
  case TY_ARRAY:
  case TY_STRUCT:
  case TY_UNION:
  case TY_FUNC:
  case TY_VLA:
    // If it is an array, do not attempt to load a value to the
    // register because in general we can't load an entire array to a
    // register. As a result, the result of an evaluation of an array
    // becomes not the array itself but the address of the array.
    // This is where "array is automatically converted to a pointer to
    // the first element of the array in C" occurs.
    return;
  case TY_FLOAT:
    return loadf();
  case TY_DOUBLE:
    return loadd();
  case TY_LDOUBLE:
    return loadld();
  default: {
    // When we load a char or a short value to a register, we always
    // extend them to the size of int, so we can assume the lower half of
    // a register always contains a valid value. The upper half of a
    // register for char, short and int may contain garbage. When we load
    // a long value to a register, it simply occupies the entire register.
    switch(ty->size) {
    case 1:
      return load8(ty->is_unsigned);
    case 2:
      return load16(ty->is_unsigned);
    case 4:
      return load32();
    default:
      return load64();
    }
  }
  }
}

// Store %rax to an address that the stack top is pointing to.
void store(Type *ty) {
  pop(GP_REF GP_1);

  switch (ty->kind) {
  case TY_STRUCT:
  case TY_UNION:
    return stores(ty->size);
  case TY_FLOAT:
    return storef();
  case TY_DOUBLE:
    return stored();
  case TY_LDOUBLE:
    return storeld();
  default:
    switch(ty->size) {
    case 1:
      return store8();
    case 2:
      return store16();
    case 4:
      return store32();
    default:
      return store64();
    }
  }
}

void emit_num(const Node *node) {
  switch (node->ty->kind) {
  case TY_FLOAT: {
    return emit_numf(node->fval);
  }
  case TY_DOUBLE: {
    return emit_numd(node->fval);
  }
  case TY_LDOUBLE: {
    return emit_numld(node->fval);
  }
  default: {
    return emit_num64(node->val);
  }
  }
}

// Structs or unions equal or smaller than 16 bytes are passed
// using up to two registers.
//
// If the first 8 bytes contains only floating-point type members,
// they are passed in an XMM register. Otherwise, they are passed
// in a general-purpose register.
//
// If a struct/union is larger than 8 bytes, the same rule is
// applied to the the next 8 byte chunk.
//
// This function returns true if `ty` has only floating-point
// members in its byte range [lo, hi).
bool has_flonum(const Type *ty, int lo, int hi, int offset) {
  if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
    for (Member *mem = ty->members; mem; mem = mem->next)
      if (!has_flonum(mem->ty, lo, hi, offset + mem->offset))
        return false;
    return true;
  }

  if (ty->kind == TY_ARRAY) {
    for (int i = 0; i < ty->array_len; i++)
      if (!has_flonum(ty->base, lo, hi, offset + ty->base->size * i))
        return false;
    return true;
  }

  return offset < lo || hi <= offset || ty->kind == TY_FLOAT || ty->kind == TY_DOUBLE;
}

bool has_flonum1(const Type *ty) {
  return has_flonum(ty, 0, 8, 0);
}

bool has_flonum2(const Type *ty) {
  return has_flonum(ty, 8, 16, 0);
}

static void push_args2(const Node *args, bool first_pass) {
  if (!args)
    return;

  push_args2(args->next, first_pass);

  if ((first_pass && !args->pass_by_stack) || (!first_pass && args->pass_by_stack))
    return;

  gen_expr(args);

  switch (args->ty->kind) {
  case TY_STRUCT:
  case TY_UNION:
    push_struct(args->ty);
    break;
  case TY_FLOAT:
  case TY_DOUBLE:
    pushf();
    break;
  case TY_LDOUBLE:
    pushld();
    break;
  default:
    push();
    break;
  }
}

// Load function call arguments. Arguments are already evaluated and
// stored to the stack as local variables. What we need to do in this
// function is to load them to registers or push them to the stack.
int push_args(const Node *node) {
  int gp = 0, fp = 0;
  int prev_depth = depth;

  // If the return type is a large struct/union, the caller passes
  // a pointer to a buffer as if it were the first argument.
  if (node->ret_buffer && node->ty->size > 16)
    gp++;

  // Load as many arguments to the registers as possible.
  for (Node *arg = node->args; arg; arg = arg->next) {
    Type *ty = arg->ty;

    switch (ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
      if (ty->size > 16) {
        arg->pass_by_stack = true;
      } else {
        bool fp1 = has_flonum1(ty);
        bool fp2 = has_flonum2(ty);

        if (fp + fp1 + fp2 < FP_MAX && gp + !fp1 + !fp2 < GP_MAX) {
          fp = fp + fp1 + fp2;
          gp = gp + !fp1 + !fp2;
        } else {
          arg->pass_by_stack = true;
        }
      }
      break;
    case TY_FLOAT:
    case TY_DOUBLE:
      if (fp++ >= FP_MAX) {
        arg->pass_by_stack = true;
      }
      break;
    case TY_LDOUBLE:
      arg->pass_by_stack = true;
      break;
    default:
      if (gp++ >= GP_MAX) {
        arg->pass_by_stack = true;
      }
    }
  }

  push_args2(node->args, true);
  push_args2(node->args, false);

  // If the return type is a large struct/union, the caller passes
  // a pointer to a buffer as if it were the first argument.
  if (node->ret_buffer && node->ty->size > 16) {
    emit_ret_fun(node->ret_buffer->offset);
    push();
  }

  return depth - prev_depth;
}

// Generate code for a given node.
void gen_expr(const Node *node) {
  char buffer[128];

  #if 0
  println("  .loc %d %d", node->tok->file->file_no, node->tok->line_no);
  #endif

  switch (node->kind) {
  case ND_NULL_EXPR:
    return;
  case ND_NUM: {
    return emit_num(node);
  }
  case ND_NEG:
    gen_expr(node->lhs);

    switch (node->ty->kind) {
    case TY_FLOAT:
      return emit_negf();
    case TY_DOUBLE:
      return emit_negd();
    case TY_LDOUBLE:
      return emit_negld();
    default:
      return emit_neg();
    }
  case ND_VAR:
    gen_addr(node);
    load(node->ty);
    return;
  case ND_MEMBER: {
    gen_addr(node);
    load(node->ty);

    Member *mem = node->member;
    if (mem->is_bitfield) emit_member_bitfield(mem);

    return;
  }
  case ND_DEREF:
    gen_expr(node->lhs);
    load(node->ty);
    return;
  case ND_ADDR:
    gen_addr(node->lhs);
    return;
  case ND_ASSIGN:
    gen_addr(node->lhs);
    push();
    gen_expr(node->rhs);

    if (node->lhs->kind == ND_MEMBER && node->lhs->member->is_bitfield) emit_assign_bitfield(node);

    store(node->ty);
    return;
  case ND_STMT_EXPR:
    for (const Node *n = node->body; n; n = n->next)
      gen_stmt(n);
    return;
  case ND_COMMA:
    gen_expr(node->lhs);
    gen_expr(node->rhs);
    return;
  case ND_CAST:
    gen_expr(node->lhs);
    cast(node->lhs->ty, node->ty);
    return;
  case ND_MEMZERO:
    return emit_memzero(node->var->ty->size, node->var->offset);
  case ND_COND: {
    int c = count();
    gen_expr(node->cond);
    snprintf(buffer, 128, ".L.else.%d", c);
    emit_jump_zero(node->cond->ty, buffer);    
    gen_expr(node->then);
    snprintf(buffer, 128, ".L.end.%d", c);
    emit_jump_label(buffer);
    println(".L.else.%d:", c);
    gen_expr(node->els);
    println(".L.end.%d:", c);
    return;
  }
  case ND_NOT:
    gen_expr(node->lhs);

    return emit_lnot(node->lhs->ty);
  case ND_BITNOT:
    gen_expr(node->lhs);

    return emit_not();
  case ND_LOGAND: {
    int c = count();
    gen_expr(node->lhs);
    snprintf(buffer, 128, ".L.false.%d", c);
    emit_jump_zero(node->lhs->ty, buffer);
    gen_expr(node->rhs);
    emit_jump_zero(node->rhs->ty, buffer);
    emit_num64(1);
    snprintf(buffer, 128, ".L.end.%d", c);
    emit_jump_label(buffer);
    println(".L.false.%d:", c);
    emit_num64(0);
    println(".L.end.%d:", c);

    return;
  }
  case ND_LOGOR: {
    int c = count();
    gen_expr(node->lhs);
    snprintf(buffer, 128, ".L.true.%d", c);
    emit_jump_nzero(node->lhs->ty, buffer);
    gen_expr(node->rhs);
    emit_jump_nzero(node->rhs->ty, buffer);
    emit_num64(0);
    snprintf(buffer, 128, ".L.end.%d", c);
    emit_jump_label(buffer);
    println(".L.true.%d:", c);
    emit_num64(1);
    println(".L.end.%d:", c);

    return;
  }
  case ND_FUNCALL: {
    return emit_funcall(node);
  }
  case ND_LABEL_VAL:
    return emit_label_val(node->unique_label);
  case ND_CAS: {
    return emit_cas(node);
  }
  case ND_EXCH: {
    gen_expr(node->lhs);
    push();
    gen_expr(node->rhs);
    pop(GP_REF GP_1);

    return emit_exch(node->lhs->ty->base->size);
  }
  default: {
    switch (node->lhs->ty->kind) {
    case TY_FLOAT: {
      gen_expr(node->rhs);
      pushf();
      gen_expr(node->lhs);
      popf(1);

      switch (node->kind) {
      case ND_ADD:
        return emit_addf();
      case ND_SUB:
        return emit_subf();
      case ND_MUL:
        return emit_mulf();
      case ND_DIV:
        return emit_divf();
      case ND_EQ:
        return emit_eqf();
      case ND_NE:
        return emit_neqf();
      case ND_LT:
        return emit_ltf();
      case ND_LE:
        return emit_lef();
      }

      break;
    }
    case TY_DOUBLE: {
      gen_expr(node->rhs);
      pushf();
      gen_expr(node->lhs);
      popf(1);

      switch (node->kind) {
      case ND_ADD:
        return emit_addd();
      case ND_SUB:
        return emit_subd();
      case ND_MUL:
        return emit_muld();
      case ND_DIV:
        return emit_divd();
      case ND_EQ:
        return emit_eqd();
      case ND_NE:
        return emit_neqd();
      case ND_LT:
        return emit_ltd();
      case ND_LE:
        return emit_led();
      }

      break;
    }
    case TY_LDOUBLE: {
      gen_expr(node->lhs);
      gen_expr(node->rhs);

      switch (node->kind) {
      case ND_ADD:
        return emit_addld();
      case ND_SUB:
        return emit_subld();
      case ND_MUL:
        return emit_mulld();
      case ND_DIV:
        return emit_divld();
      case ND_EQ:
        return emit_eqld();
      case ND_NE:
        return emit_neqld();
      case ND_LT:
        return emit_ltld();
      case ND_LE:
        return emit_leld();
      }

      break;
    }
    default: {
      gen_expr(node->rhs);
      push();
      gen_expr(node->lhs);
      pop(GP_REF GP_1);

      bool is_long = node->lhs->ty->kind == TY_LONG || node->lhs->ty->base;

      switch (node->kind) {
      case ND_ADD:
        return is_long ? emit_addl() : emit_add();
      case ND_SUB:
        return is_long ? emit_subl() : emit_sub();
      case ND_MUL:
        return is_long ? emit_mull() : emit_mul();
      case ND_DIV:
        return is_long ? emit_divl(node->ty->is_unsigned, node->lhs->ty->size == 8) : emit_div(node->ty->is_unsigned, node->lhs->ty->size == 8);
      case ND_MOD:
        return is_long ? emit_modl(node->ty->is_unsigned, node->lhs->ty->size == 8) : emit_mod(node->ty->is_unsigned, node->lhs->ty->size == 8);
      case ND_BITAND:
        return is_long ? emit_andl() : emit_and();
      case ND_BITOR:
        return is_long ? emit_orl() : emit_or();
      case ND_BITXOR:
        return is_long ? emit_xorl() : emit_xor();
      case ND_EQ:
        return is_long ? emit_eq() : emit_eql();
      case ND_NE:
        return is_long ? emit_neq() : emit_neql();
      case ND_LT:
        return is_long ? emit_lt(node->lhs->ty->is_unsigned) : emit_ltl(node->lhs->ty->is_unsigned);
      case ND_LE:
        return is_long ? emit_le(node->lhs->ty->is_unsigned) : emit_lel(node->lhs->ty->is_unsigned);
      case ND_SHL:
        return is_long ? emit_shl() : emit_shll();
      case ND_SHR:
        return is_long ? emit_shr(node->lhs->ty->is_unsigned) : emit_shrl(node->lhs->ty->is_unsigned);
      }

      break;
    }
    }
  }
  }

  error_tok(node->tok, "invalid expression");
}

void gen_stmt(const Node *node) {
  char buffer[128];

  #if 0
  println("  .loc %d %d", node->tok->file->file_no, node->tok->line_no);
  #endif

  switch (node->kind) {
  case ND_IF: {
    int c = count();
    gen_expr(node->cond);
    snprintf(buffer, 128, ".L.else.%d", c);
    emit_jump_zero(node->cond->ty, buffer);
    gen_stmt(node->then);
    snprintf(buffer, 128, ".L.end.%d", c);
    emit_jump_label(buffer);
    println(".L.else.%d:", c);
    if (node->els)
      gen_stmt(node->els);
    println(".L.end.%d:", c);
    return;
  }
  case ND_FOR: {
    int c = count();
    if (node->init)
      gen_stmt(node->init);
    println(".L.begin.%d:", c);
    if (node->cond) {
      gen_expr(node->cond);
      emit_jump_zero(node->cond->ty, node->brk_label);
    }
    gen_stmt(node->then);
    println("%s:", node->cont_label);
    if (node->inc)
      gen_expr(node->inc);

    snprintf(buffer, 128, ".L.begin.%d", c);
    emit_jump_label(buffer);

    println("%s:", node->brk_label);
    return;
  }
  case ND_DO: {
    int c = count();
    println(".L.begin.%d:", c);
    gen_stmt(node->then);
    println("%s:", node->cont_label);
    gen_expr(node->cond);
    snprintf(buffer, 128, ".L.begin.%d", c);
    emit_jump_nzero(node->cond->ty, buffer);
    println("%s:", node->brk_label);
    return;
  }
  case ND_SWITCH:
    gen_expr(node->cond);

    emit_switch_cases(node);

    if (node->default_case) emit_jump_label(node->default_case->label);

    emit_jump_label(node->brk_label);
    gen_stmt(node->then);
    println("%s:", node->brk_label);
    return;
  case ND_CASE:
    println("%s:", node->label);
    gen_stmt(node->lhs);
    return;
  case ND_BLOCK:
    for (Node *n = node->body; n; n = n->next)
      gen_stmt(n);
    return;
  case ND_GOTO:
    emit_jump_label(node->unique_label);
    return;
  case ND_GOTO_EXPR:
    gen_expr(node->lhs);
    emit_jump_reg();
    return;
  case ND_LABEL:
    println("%s:", node->unique_label);
    gen_stmt(node->lhs);
    return;
  case ND_RETURN: {
    if (node->lhs) {
      gen_expr(node->lhs);
      Type *ty = node->lhs->ty;

      switch (ty->kind) {
      case TY_STRUCT:
      case TY_UNION:
        if (ty->size <= 16)
          copy_struct_reg();
        else
          copy_struct_mem();
        break;
      }
    }

    snprintf(buffer, 128, ".L.return.%s", current_fn->name);
    emit_jump_label(buffer);

    return;
  } case ND_EXPR_STMT:
    gen_expr(node->lhs);
    return;
  case ND_ASM:
    println("  %s", node->asm_str);
    return;
  }

  error_tok(node->tok, "invalid statement");
}

// Assign offsets to local variables.
void assign_lvar_offsets(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function)
      continue;

    // If a function has many parameters, some parameters are
    // inevitably passed by stack rather than by register.
    // The first passed-by-stack parameter resides at RBP+16.
    int top = FRAME_OFFSET;
    int bottom = 0;

    int gp = 0, fp = 0;

    // Assign offsets to pass-by-stack parameters.
    for (Obj *var = fn->params; var; var = var->next) {
      const Type *ty = var->ty;

      switch (ty->kind) {
      case TY_STRUCT:
      case TY_UNION:
        if (ty->size <= 16) {
          bool fp1 = has_flonum(ty, 0, 8, 0);
          bool fp2 = has_flonum(ty, 8, 16, 8);
          if (fp + fp1 + fp2 < FP_MAX && gp + !fp1 + !fp2 < GP_MAX) {
            fp = fp + fp1 + fp2;
            gp = gp + !fp1 + !fp2;
            continue;
          }
        }
        break;
      case TY_FLOAT:
      case TY_DOUBLE:
        if (fp++ < FP_MAX)
          continue;
        break;
      case TY_LDOUBLE:
        break;
      default:
        if (gp++ < GP_MAX)
          continue;
      }

      top = align_to(top, FRAME_ALIGN);
      var->offset = top;
      top += var->ty->size / NUM_BYTE_AT_ADDRESS;
    }

    // Assign offsets to pass-by-register parameters and local variables.
    for (Obj *var = fn->locals; var; var = var->next) {
      if (var->offset)
        continue;

      // AMD64 System V ABI has a special alignment rule for an array of
      // length at least 16 bytes. We need to align such array to at least
      // 16-byte boundaries. See p.14 of
      // https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-draft.pdf.
      int align = (var->ty->kind == TY_ARRAY && var->ty->size >= 16)
        ? MAX(16, var->align) : var->align;

      bottom += var->ty->size / NUM_BYTE_AT_ADDRESS;
      bottom = align_to(bottom, align);
      var->offset = -bottom;
    }

    fn->stack_size = align_to(bottom, STACK_ALIGN);
  }
}

void emit_data(Obj *prog) {
  for (Obj *var = prog; var; var = var->next) {
    if (var->is_function || !var->is_definition)
      continue;

    if (var->is_static)
      println("  .local %s", var->name);
    else
      println("  .globl %s", var->name);

    int align = (var->ty->kind == TY_ARRAY && var->ty->size >= 16)
      ? MAX(16, var->align) : var->align;

    // Common symbol
    if (opt_fcommon && var->is_tentative) {
      println("  .comm %s, %d, %d", var->name, var->ty->size, align);
      continue;
    }

    // .data or .tdata
    if (var->init_data) {
      if (var->is_tls)
        println("  .section .tdata,\"awT\",@progbits");
      else
        println("  .data");

      println("  .type %s, @object", var->name);
      println("  .size %s, %d", var->name, var->ty->size);
      println("  .align %d", align);
      println("%s:", var->name);

      int pos = 0;
      Relocation *rel = var->rel;
      while (pos < var->ty->size) {
        if (rel && rel->offset == pos) {
          println("  .quad %s%+ld", *rel->label, rel->addend);
          rel = rel->next;
          pos += 8;
        } else {
          println("  .byte %d", var->init_data[pos++]);
        }
      }
      continue;
    }

    // .bss or .tbss
    if (var->is_tls)
      println("  .section .tbss,\"awT\",@nobits");
    else
      println("  .bss");

    println("  .align %d", align);
    println("%s:", var->name);
    println("  .zero %d", var->ty->size);
  }
}

void emit_text(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition)
      continue;

    // No code is emitted for "static inline" functions
    // if no one is referencing them.
    if (!fn->is_live)
      continue;

    if (fn->is_static)
      println("  .local %s", fn->name);
    else
      println("  .globl %s", fn->name);

    println("  .text");
    println("  .type %s, @function", fn->name);
    println("%s:", fn->name);
    current_fn = fn;

    // Prologue
    emit_prologue(fn);

    // Save arg registers if function is variadic
    if (fn->va_area) {
      int gp = 0, fp = 0;
      for (Obj *var = fn->params; var; var = var->next) {
        if (is_flonum(var->ty))
          fp++;
        else
          gp++;
      }

      emit_va_area(gp, fp, fn->va_area->offset);
    }

    // Save passed-by-register arguments to the stack
    int gp = 0, fp = 0;
    for (Obj *var = fn->params; var; var = var->next) {
      if (var->offset > 0)
        continue;

      Type *ty = var->ty;

      switch (ty->kind) {
      case TY_STRUCT:
      case TY_UNION:
        assert(ty->size <= 16);
        if (has_flonum(ty, 0, 8, 0))
          store_fp(fp++, var->offset, MIN(8, ty->size));
        else
          store_gp(gp++, var->offset, MIN(8, ty->size));

        if (ty->size > 8) {
          if (has_flonum(ty, 8, 16, 0))
            store_fp(fp++, var->offset + 8, ty->size - 8);
          else
            store_gp(gp++, var->offset + 8, ty->size - 8);
        }
        break;
      case TY_FLOAT:
      case TY_DOUBLE:
        store_fp(fp++, var->offset, ty->size);
        break;
      default:
        store_gp(gp++, var->offset, ty->size);
      }
    }

    // Emit code
    gen_stmt(fn->body);
    assert(depth == 0);

    // [https://www.sigbus.info/n1570#5.1.2.2.3p1] The C spec defines
    // a special rule for the main function. Reaching the end of the
    // main function is equivalent to returning 0, even though the
    // behavior is undefined for the other functions.
    if (strcmp(fn->name, "main") == 0) emit_num64(0);

    // Epilogue
    emit_epilogue(fn);
  }
}

void codegen(Obj *prog, FILE *out) {
  output_file = out;

  File **files = get_input_files();
  for (int i = 0; files[i]; i++)
    println("  .file %d \"%s\"", files[i]->file_no, files[i]->name);

  assign_lvar_offsets(prog);
  emit_data(prog);
  emit_text(prog);
}
