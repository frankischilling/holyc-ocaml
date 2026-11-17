extern U0 Print(U8 *fmt,...);

F64 Value()
{
  Print("A");
  return 1.5;
}

class Box
{
  U8 tag;
  F64 values[2];
  I64 tail;
};

I64 Observe()
{
  static Box box={7,{Value(),-0.0},42};
  // Packed F64 values start at byte 1. Read their exact stored bytes.
  I64 result=box.tag+(&box)(U8*)[7]+(&box)(U8*)[16]+box.tail;
  (&box)(U8*)[7]=0;
  return result;
}

// Static preparation prints A once. Calls retain the changed byte.
Observe()+Observe()-560;
