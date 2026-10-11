USE64
IMPORT BMODCounter;
entry::
MOV RAX,BMODCounter;
MOV U64 [RAX],17;
RET;
second:
MOV RAX,BMODCounter;
ADD U64 [RAX],4;
RET;
ALIGN 16,0;
data:: DU32 entry;
