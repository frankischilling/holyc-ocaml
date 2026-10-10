extern U0 Print(U8 *fmt,...);

I64 LocalObjects()
{
  class Entry {
    U16 value;
    U8 marks[2];
  };
  Entry entries[2];
  entries[0].value=20;
  entries[1].value=21;
  Entry *selected=&entries[1];
  selected->marks[0]=1;
  selected->value+=selected->marks[0];
  I64 result=entries[0].value+selected->value;
  Print("%d",result);
  return result;
}

LocalObjects();
