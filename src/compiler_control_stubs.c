#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <caml/custom.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

/* KernelA.HH:2124-2134 and 2179-2214, pinned c26482bb. This is a
   field-storage prefix, not CmpCtrlNew or the complete CCmpCtrl ABI. Pointer
   slots remain zero; no native table, queue or lexical-buffer owner is implied. */
struct compiler_lex_hash_context {
  uint64_t next, old_flags, hash_mask;
  uint64_t local_var_lst, fun, hash_table_lst;
  uint64_t define_hash_table, local_hash_table, glbl_hash_table;
};

struct compiler_abs_counts {
  uint16_t abs_addres, c_addres;
  uint32_t externs;
};

struct compiler_control_prefix {
  uint64_t next, last, token, flags, cur_i64;
  double cur_f64;
  uint64_t cur_str, cur_str_len, class_dol_offset;
  uint64_t dollar_buf, dollar_cnt, cur_help_idx;
  uint64_t last_U16, min_line, max_line, last_line_num, lock_cnt;
  uint64_t char_bmp_alpha_numeric;
  struct compiler_lex_hash_context htc;
  uint64_t hash_entry;
  struct compiler_abs_counts abs_cnts;
  uint64_t asm_undef_hash, local_var_entry, lb_leave, cur_buf_ptr;
  uint64_t lex_include_stk, lex_prs_stk, fun_lex_file;
  uint64_t next_stream_blk, last_stream_blk, aot;
  uint64_t pass, opts, pass_trace, saved_pass_trace, error_cnt, warning_cnt;
};

_Static_assert(sizeof(void *) == 8, "compiler control storage requires 64 bits");
_Static_assert(sizeof(struct compiler_lex_hash_context) == 72,
               "CLexHashTableContext size differs");
_Static_assert(sizeof(struct compiler_abs_counts) == 8,
               "CAbsCntsI64 size differs");
_Static_assert(offsetof(struct compiler_control_prefix, flags) == 24,
               "CCmpCtrl flags offset differs");
_Static_assert(offsetof(struct compiler_control_prefix, htc) == 144,
               "CCmpCtrl htc offset differs");
_Static_assert(offsetof(struct compiler_control_prefix, hash_entry) == 216,
               "CCmpCtrl hash_entry offset differs");
_Static_assert(offsetof(struct compiler_control_prefix, abs_cnts) == 224,
               "CCmpCtrl abs_cnts offset differs");
_Static_assert(offsetof(struct compiler_control_prefix, aot) == 304,
               "CCmpCtrl aot offset differs");
_Static_assert(offsetof(struct compiler_control_prefix, opts) == 320,
               "CCmpCtrl opts offset differs");
_Static_assert(offsetof(struct compiler_control_prefix, error_cnt) == 344,
               "CCmpCtrl error_cnt offset differs");
_Static_assert(offsetof(struct compiler_control_prefix, warning_cnt) == 352,
               "CCmpCtrl warning_cnt offset differs");
_Static_assert(sizeof(struct compiler_control_prefix) == 360,
               "CCmpCtrl field prefix size differs");

static void compiler_control_finalize(value handle) {
  free(*(struct compiler_control_prefix **)Data_custom_val(handle));
  *(struct compiler_control_prefix **)Data_custom_val(handle) = NULL;
}

static struct custom_operations compiler_control_operations = {
  "holyc.compiler.control-fields",
  compiler_control_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct compiler_control_prefix *compiler_control_original(value handle) {
  if (!Is_block(handle) || Tag_val(handle) != Custom_tag ||
      Custom_ops_val(handle) != &compiler_control_operations)
    caml_invalid_argument("foreign compiler control storage");
  struct compiler_control_prefix *control =
      *(struct compiler_control_prefix **)Data_custom_val(handle);
  if (control == NULL)
    caml_invalid_argument("expired compiler control storage");
  return control;
}

static unsigned compiler_control_bit(value index) {
  intnat bit = Long_val(index);
  if (bit < 0 || bit >= 64)
    caml_invalid_argument("compiler control bit is outside its option field");
  return (unsigned)bit;
}

static int compiler_control_get(struct compiler_control_prefix *control,
                                unsigned bit) {
  return (control->opts >> bit) & 1;
}

static int compiler_control_set(struct compiler_control_prefix *control,
                                unsigned bit, int enabled) {
  uint64_t mask = UINT64_C(1) << bit;
  int previous = compiler_control_get(control, bit);
  if (enabled) control->opts |= mask;
  else control->opts &= ~mask;
  return previous;
}

/* PrsStmt.HC:159,166,1114; KernelA.HH:2156. */
static int compiler_control_has_return(struct compiler_control_prefix *control) {
  return (control->flags & UINT64_C(0x400000)) != 0;
}
static void compiler_control_set_has_return(
    struct compiler_control_prefix *control, int enabled) {
  if (enabled) control->flags |= UINT64_C(0x400000);
  else control->flags &= ~UINT64_C(0x400000);
}

CAMLprim value holyc_compiler_control_has_return(value handle) {
  CAMLparam1(handle);
  CAMLreturn(Val_bool(compiler_control_has_return(compiler_control_original(handle))));
}
CAMLprim value holyc_compiler_control_set_has_return(value handle, value enabled) {
  CAMLparam2(handle, enabled);
  compiler_control_set_has_return(compiler_control_original(handle), Bool_val(enabled));
  CAMLreturn(Val_unit);
}

static void compiler_control_increment_warning(
    struct compiler_control_prefix *control) {
  /* CExcept.HC:78,107. Unsigned arithmetic preserves the I64 bit pattern,
     including wraparound, without signed-overflow undefined behavior. */
  control->warning_cnt++;
}

CAMLprim value holyc_compiler_control_create(value options) {
  CAMLparam1(options);
  CAMLlocal1(handle);
  handle = caml_alloc_custom(&compiler_control_operations,
                            sizeof(struct compiler_control_prefix *),
                            sizeof(struct compiler_control_prefix), 1024 * 1024);
  *(struct compiler_control_prefix **)Data_custom_val(handle) = NULL;
  struct compiler_control_prefix *control = calloc(1, sizeof(*control));
  if (control == NULL) caml_raise_out_of_memory();
  *(struct compiler_control_prefix **)Data_custom_val(handle) = control;
  control->opts = (uint64_t)Int64_val(options);
  CAMLreturn(handle);
}

CAMLprim value holyc_compiler_control_options(value handle) {
  CAMLparam1(handle);
  CAMLreturn(caml_copy_int64((int64_t)compiler_control_original(handle)->opts));
}

CAMLprim value holyc_compiler_control_get_option(value handle, value index) {
  CAMLparam2(handle, index);
  struct compiler_control_prefix *control = compiler_control_original(handle);
  unsigned bit = compiler_control_bit(index);
  CAMLreturn(Val_bool(compiler_control_get(control, bit)));
}

CAMLprim value holyc_compiler_control_set_option(value handle, value index,
                                               value enabled) {
  CAMLparam3(handle, index, enabled);
  struct compiler_control_prefix *control = compiler_control_original(handle);
  unsigned bit = compiler_control_bit(index);
  CAMLreturn(Val_bool(compiler_control_set(control, bit, Bool_val(enabled))));
}

CAMLprim value holyc_compiler_control_warnings(value handle) {
  CAMLparam1(handle);
  CAMLreturn(caml_copy_int64(
      (int64_t)compiler_control_original(handle)->warning_cnt));
}

CAMLprim value holyc_compiler_control_increment_warning(value handle) {
  CAMLparam1(handle);
  compiler_control_increment_warning(compiler_control_original(handle));
  CAMLreturn(Val_unit);
}

/* CExcept.HC:91. Only the original LexExcept producer counts this error;
   grammar limits, unsupported execution and ownership faults remain separate. */
static void compiler_control_increment_error(
    struct compiler_control_prefix *control) {
  control->error_cnt++;
}

CAMLprim value holyc_compiler_control_errors(value handle) {
  CAMLparam1(handle);
  CAMLreturn(caml_copy_int64(
      (int64_t)compiler_control_original(handle)->error_cnt));
}

CAMLprim value holyc_compiler_control_increment_error(value handle) {
  CAMLparam1(handle);
  compiler_control_increment_error(compiler_control_original(handle));
  CAMLreturn(Val_unit);
}

#if (defined(__x86_64__) || defined(_M_X64)) && defined(__GNUC__)
/* KUtils.HC:88-103 and CMisc.HC:1-10: BT reads the carry, and BTS/BTR
   returns the old bit. Literal offsets keep the oracle independent of C
   member access and offsetof. Indices stay inside the single 64-bit field. */
static int compiler_option_instruction_get(unsigned char *bytes, uint64_t bit) {
  unsigned char previous;
  __asm__ volatile("btq %2,320(%1); setc %0"
                   : "=q"(previous) : "r"(bytes), "r"(bit) : "cc", "memory");
  return previous;
}

static int compiler_option_instruction_set(unsigned char *bytes, uint64_t bit,
                                           int enabled) {
  unsigned char previous;
  if (enabled)
    __asm__ volatile("btsq %2,320(%1); setc %0"
                     : "=q"(previous) : "r"(bytes), "r"(bit) : "cc", "memory");
  else
    __asm__ volatile("btrq %2,320(%1); setc %0"
                     : "=q"(previous) : "r"(bytes), "r"(bit) : "cc", "memory");
  return previous;
}
#endif

CAMLprim value holyc_compiler_control_verify_storage(value unit) {
  CAMLparam1(unit);
#if (defined(__x86_64__) || defined(_M_X64)) && defined(__GNUC__)
  const uint64_t seeds[] = {0, 1, UINT64_C(0x90000), UINT64_C(0xaaaaaaaaaaaaaaaa),
                           UINT64_C(0x5555555555555555), UINT64_C(0x7fffffffffffffff),
                           UINT64_C(0x8000000000000000), UINT64_MAX};
  for (size_t seed = 0; seed < sizeof(seeds) / sizeof(seeds[0]); seed++) {
    for (unsigned bit = 0; bit < 64; bit++) {
      for (int enabled = 0; enabled <= 1; enabled++) {
        struct compiler_control_prefix production;
        unsigned char reference[360];
        /* Nonzero guards also detect overwrites of neighboring fields. */
        memset(&production, 0xa5, sizeof(production));
        memset(reference, 0xa5, sizeof(reference));
        production.opts = seeds[seed];
        production.warning_cnt = seeds[seed];
        production.error_cnt = seeds[seed];
        memcpy(reference + 320, &seeds[seed], 8);
        memcpy(reference + 352, &seeds[seed], 8);
        memcpy(reference + 344, &seeds[seed], 8);
        if (compiler_control_get(&production, bit) !=
            compiler_option_instruction_get(reference, bit)) CAMLreturn(Val_false);
        for (int repeat = 0; repeat < 2; repeat++) {
          if (compiler_control_set(&production, bit, enabled) !=
              compiler_option_instruction_set(reference, bit, enabled))
            CAMLreturn(Val_false);
          if (memcmp(&production, reference, 360) != 0) CAMLreturn(Val_false);
        }
        compiler_control_increment_warning(&production);
        __asm__ volatile("incq 352(%0)" : : "r"(reference) : "cc", "memory");
        if (memcmp(&production, reference, 360) != 0) CAMLreturn(Val_false);
        compiler_control_increment_error(&production);
        __asm__ volatile("incq 344(%0)" : : "r"(reference) : "cc", "memory");
        if (memcmp(&production, reference, 360) != 0) CAMLreturn(Val_false);
      }
    }
  }
  for (size_t seed = 0; seed < sizeof(seeds) / sizeof(seeds[0]); seed++) {
    for (int enabled = 0; enabled <= 1; enabled++) {
      struct compiler_control_prefix production;
      unsigned char reference[360], previous;
      memset(&production, 0x5a, 360);
      memset(reference, 0x5a, 360);
      production.flags = seeds[seed];
      memcpy(reference + 24, &seeds[seed], 8);
      __asm__ volatile("btq $22,24(%1); setc %0"
                       : "=q"(previous) : "r"(reference) : "cc", "memory");
      if (compiler_control_has_return(&production) != previous) CAMLreturn(Val_false);
      for (int repeat = 0; repeat < 2; repeat++) {
        compiler_control_set_has_return(&production, enabled);
        if (enabled)
          __asm__ volatile("btsq $22,24(%0)" : : "r"(reference) : "cc", "memory");
        else
          __asm__ volatile("btrq $22,24(%0)" : : "r"(reference) : "cc", "memory");
        if (memcmp(&production, reference, 360) != 0) CAMLreturn(Val_false);
      }
    }
  }
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}
