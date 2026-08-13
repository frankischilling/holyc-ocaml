asm {
ROOT:
@@loop: MOV RAX,I64 [RBP+8]
EXPORTED::
DU8 "A",0;
USE64 MOV RSP,I64 [RBP]
JZ @@loop
}

U0 Nested()
{
  asm {
    LIST
    CALL &Target
  }
}
