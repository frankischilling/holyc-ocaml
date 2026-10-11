#define DST RAX
#define SLOT U64 24[RBP+RCX*8]
ENTRY::
    USE16
    REP_MOVSB
    MOV AX,0xffff
    USE32
    REP_MOVSB
    MOV EAX,0xffffffff
    USE64
    MOV
        DST,
        SLOT
  MOV RDI,U64 FS:TaskHashTable[RAX]
@@again:
    LOCK
    BTS U64 [RBX],0
    JZ @@again
    FLD U64 [RDX]
    FMUL ST0,ST0
    FADDP ST1,ST0
    FXCH ST7
    IMUL2 RAX,10
    BPT
    RET1 8
