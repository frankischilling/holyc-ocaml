I64 NextByte()
{
  static U8 Bytes[2][3]={"AB","CD"};
  return ++Bytes[1][0];
}
NextByte();
U8 Later[2]={20,22};
NextByte();
