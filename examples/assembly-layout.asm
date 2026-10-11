// Forward instruction fields follow Compiler/Asm.HC and AsmResolve.HC.
ORG 0x1003;
LIST;
Entry::
  JMP Tail;
NOLIST;
  DU8 0xAA;
  ALIGN 8,0x90;
LIST;
Tail:
  MOV RAX,[RBX+After];
After:
  RET;
