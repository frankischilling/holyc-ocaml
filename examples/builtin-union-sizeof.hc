extern U0 Print(U8 *format,...);

U64 values[3];
class Packet { U32 code; U64 words[2]; };

I64 Measure(U64 parameter)
{
  U64 local[4];
  static I32 cached;
  I64 wide=sizeof(U64.u8);
  I64 middle=sizeof(U64.u32.u8);
  I64 small=sizeof(U64.u32.u16.u8);
  I64 field=sizeof(Packet.words.u16.u8);
  I64 stored=sizeof(values.u32.u8);
  Print("%d %d %d %d %d\n",wide,middle,small,field,stored);
  return wide+middle+small+field+stored+sizeof(local.u8)+
    sizeof(cached.u16.u8)+sizeof(parameter.i16.i8);
}

Measure(0);
