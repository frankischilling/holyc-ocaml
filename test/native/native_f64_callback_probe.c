/* Test-only CW probe. Never restore a full FP image around compiler-generated
   C/OCaml frames. Full x87 register/tag tests live in the standalone thunk test. */
#include <stdint.h>
#include <caml/mlvalues.h>

#if defined(__x86_64__) && (defined(__GNUC__) || defined(__clang__)) && \
    (defined(__linux__) || defined(_WIN32))
#define HOLYC_F64_CW_PROBE 1
#else
#define HOLYC_F64_CW_PROBE 0
#endif

CAMLprim value holyc_test_f64_cw_available(value unit)
{
  (void)unit;
  return Val_bool(HOLYC_F64_CW_PROBE);
}

CAMLprim value holyc_test_f64_cw_get(value unit)
{
  (void)unit;
#if HOLYC_F64_CW_PROBE
  uint16_t control;
  __asm__ volatile("fnstcw %0" : "=m"(control) : : "memory");
  return Val_int(control);
#else
  return Val_int(0);
#endif
}

CAMLprim value holyc_test_f64_cw_set(value control)
{
#if HOLYC_F64_CW_PROBE
  uint16_t word = (uint16_t)Int_val(control);
  __asm__ volatile("fldcw %0" : : "m"(word) : "memory");
#else
  (void)control;
#endif
  return Val_unit;
}
