I64 CompactFrame()
{
  I64 values[5000];
  values[0]=20;
  values[4999]=22;
  return values[0]+values[4999];
}

CompactFrame();
