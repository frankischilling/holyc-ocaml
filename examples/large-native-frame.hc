I64 LargeFrame()
{
  I64 values[1024];
  values[0]=20;
  values[1023]=22;
  return values[0]+values[1023];
}

LargeFrame();
