#include "T:/Kernel/KernelA.HH"
I64 NVGlobal;
I64 NVFunction(I64 x) {return x+NVGlobal;}
NVGlobal=17;
