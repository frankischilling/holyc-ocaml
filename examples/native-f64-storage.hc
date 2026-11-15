class Box
{
  U8 tag;
  F64 values[2];
};

F64 Sum(F64 *pointer)
{
  F64 left=*pointer;
  F64 right=*(pointer+1);
  return left+right;
}

F64 Example()
{
  Box box;
  box.tag=7;
  box.values[0]=1.5;
  box.values[1]=2.25;
  return Sum(&box.values[0]);
}

Example();
