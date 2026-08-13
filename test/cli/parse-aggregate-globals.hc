// Sources: Compiler/PrsStmt.HC:PrsGlblVarLst and Compiler/PrsVar.HC:PrsGlblInit.
#define COUNT 2
class Entry
{
  I64 value;
  U8 *name;
} entries[COUNT]={{1,"one"},{2,"two"},},*active=CAlloc(sizeof(Entry));
union Bits { I64 signed_value; U64 unsigned_value; } bits={0};
I64 standalone=3;
