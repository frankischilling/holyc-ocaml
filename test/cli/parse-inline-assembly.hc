U0 Inline()
{
  PUSHFD CLI;
  MOV RAX,U64 8[RBP]
  CALL Target;
  if (1) {PAUSE}
  POPFD BPT;
}
