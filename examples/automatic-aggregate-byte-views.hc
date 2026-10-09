extern U0 Print(U8 *fmt,...);

class Packed
{
  U8 text[3];
  U16 value;
};

I64 F()
{
  Packed object;
  U8 *bytes=(&object)(U8*);
  bytes[0]=65;
  bytes[1]=66;
  bytes[2]=0;
  Print("%s",bytes);

  // PrsVarLst packs value at byte three, without automatic member padding.
  U16 *value=(bytes+3)(U16*);
  *value=40;
  *value+=2;
  return *value;
}
F();
