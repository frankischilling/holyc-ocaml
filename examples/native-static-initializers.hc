I8 G=20;
I64 Next(I64 step=1)
{
  static I8 value=20;
  return value+=step;
}
Next();
G+Next();
