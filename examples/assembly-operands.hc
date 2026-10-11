asm {
ASSEMBLY_OPERANDS::
    USE16
    MOV AX,0xffff
    REP_MOVSB
    USE32
    MOV EAX,0xffffffff
    USE64
    MOV RAX,U64 24[RBP+RCX*8]
  MOV RDI,U64 FS:TaskHashTable[RAX]
    LOCK
    BTS U64 [RBX],0
    FLD U64 [RDX]
    FMUL ST0,ST0
    FADDP ST1,ST0
    FXCH ST7
@@done:
    JZ @@done
    IMUL2 RAX,10
    RET1 8
}

U0 DirectAssemblyOperands()
{
    MOV RAX,
        U64 8[RBP]
    ADD RAX,2
    BPT;
}
