// Compiler/CompilerA.HH and Kernel/KernelB.HH at the pinned TempleOS revision.
#define IC_TO_I64 0x1B
#define IC_TO_F64 0x1C
_intern IC_TO_F64 F64 ToF64(I64 value);
_intern IC_TO_I64 I64 ToI64(F64 value);

F64 Base(F64 value=ToF64(40))
{
  return value;
}

F64 Target(F64 value=ToF64(99))
{
  return value;
}

F64 (*Selected)(F64 value=ToF64(ToI64(2.75)))=&Target;

Base()+Selected();
