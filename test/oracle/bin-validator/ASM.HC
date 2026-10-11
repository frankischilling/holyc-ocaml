asm {
USE64
IMPORT NVImport;
NVEntry:: MOV RAX,NVImport; RET;
NVDatum:: DU32 NVEntry;
}
