extern U0 Print(U8 *fmt,...);

class Packet {
  U8 tag;
  U16 value;
};

union Storage {
  U8 bytes[7];
  I64 word;
};

I64 Measure(Packet argument)
{
  Packet items[2][3];
  Storage storage;
  I64 (*callback)();
  I64 parameter_bytes=sizeof(argument);
  I64 array_bytes=sizeof(items);
  I64 union_bytes=sizeof(storage);
  I64 callback_bytes=sizeof(callback);
  I64 starred_bytes=sizeof(items *);
  Print("%d %d %d %d %d\n",parameter_bytes,array_bytes,union_bytes,
        callback_bytes,starred_bytes);
  return parameter_bytes+array_bytes+union_bytes+callback_bytes+starred_bytes;
}

Measure(0);
