U0 Inline()
{
  IMPORT Target;
  DU8 "A",0,2 DUP(0);
  LIST USE32
  PUSHFD CLI;
  MOV RAX,U64 8[RBP]
  CALL Target;
  NOLIST USE64
  BINFILE "fixture.bin";
  if (1) {PAUSE}
  POPFD BPT;
}
