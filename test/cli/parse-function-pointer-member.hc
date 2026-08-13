// Sources: Compiler/PrsVar.HC:PrsType/PrsVarLst and Kernel/KernelA.HH:281-282.
class CallbackBox
{
  U8 *(*convert)(I64 value=1,U0 (*done)(I64 result));
};
