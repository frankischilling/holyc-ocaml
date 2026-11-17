/* Architectural tests for the instructions used by the pinned TempleOS
   comparison templates. These raw-word fixtures are not source execution
   captures and do not authorize arbitrary HolyC F64 payload construction. */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#if defined(__x86_64__) && (defined(__GNUC__) || defined(__clang__)) && \
    (defined(_WIN32) || defined(__linux__))
struct fp_image { _Alignas(16) unsigned char bytes[512]; };

/* Return B, AE, A, BE in low-to-high bits after comparing left:right.
   FLD m64 preserves the numeric value but can quiet a signaling NaN. Both
   operands are removed by the FCOMIP/FFREE/FINCSTP sequence in Templates.HC. */
static unsigned predicates(uint64_t left, uint64_t right)
{
  unsigned char b, ae, a, be;
  __asm__ volatile (
    "fldl %5\n\tfldl %4\n\tfcomip %%st(1),%%st\n\t"
    "setb %0\n\tsetae %1\n\tseta %2\n\tsetbe %3\n\t"
    "ffree %%st(0)\n\tfincstp"
    : "=qm"(b), "=qm"(ae), "=qm"(a), "=qm"(be)
    : "m"(left), "m"(right) : "cc", "st", "st(1)");
  return b | ae << 1 | a << 2 | be << 3;
}

static unsigned word_equal(uint64_t left, uint64_t right)
{
  unsigned char equal;
  __asm__ volatile("cmpq %2,%1\n\tsete %0"
    : "=qm"(equal) : "r"(left), "r"(right) : "cc");
  return equal;
}

struct row { uint64_t left, right; unsigned forward, reverse, equal; };
static int run(void)
{
  static const struct row rows[] = {
    {0x3ff0000000000000ULL,0x4000000000000000ULL,9,6,0},
    {0x4000000000000000ULL,0x4000000000000000ULL,10,10,1},
    {0,0x8000000000000000ULL,10,10,0},
    {0x7ff0000000000000ULL,0x3ff0000000000000ULL,6,9,0},
    {0xfff0000000000000ULL,0x3ff0000000000000ULL,9,6,0},
    {0x7ff0000000000000ULL,0x7ff0000000000000ULL,10,10,1},
    {0x7ff8000000000042ULL,0x3ff0000000000000ULL,9,9,0},
    {0x7ff0000000000001ULL,0x3ff0000000000000ULL,9,9,0},
    {0xfff8000000000042ULL,0x3ff0000000000000ULL,9,9,0},
    {0x7ff8000000000042ULL,0x7ff8000000000042ULL,9,9,1},
    {0x7ff8000000000042ULL,0x7ff8000000000043ULL,9,9,0},
    {0x7ff0000000000001ULL,0x7ff0000000000001ULL,9,9,1},
  };
  struct fp_image saved, after;
  __asm__ volatile("fxsave64 %0\n\tfninit" : "=m"(saved));
  unsigned failures=0;
  for (unsigned round=0;round<32;++round)
    for (unsigned i=0;i<sizeof(rows)/sizeof(rows[0]);++i) {
      const struct row *r=&rows[i];
      if (predicates(r->left,r->right)!=r->forward ||
          predicates(r->right,r->left)!=r->reverse ||
          word_equal(r->left,r->right)!=r->equal) ++failures;
    }
  __asm__ volatile("fxsave64 %0\n\tfxrstor64 %1" : "=m"(after) : "m"(saved));
  /* TOP is zero and abridged tag word is empty after every pair of operands. */
  if ((after.bytes[3]&0x38)!=0 || after.bytes[4]!=0 ||
      (after.bytes[2]&1)==0 || after.bytes[0]!=0x7f || after.bytes[1]!=3) ++failures;
  if (failures) { fprintf(stderr,"raw FCOMIP predicates/stack: %u failures\n",failures); abort(); }
  puts("raw comparison instructions: zero/NaN word equality, FCOMIP predicates and balanced stack passed");
  return 0;
}
#else
static int run(void) { puts("raw FCOMIP predicates: unsupported host, skipped"); return 0; }
#endif
#include <caml/mlvalues.h>
CAMLprim value holyc_test_native_float_comparison(value unit)
{
  (void)unit;
  (void)run();
  return Val_unit;
}
