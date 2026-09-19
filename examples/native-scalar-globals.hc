I8 Small;
I64 Total;

I64 Add(I64 n=21)
{
  Total+=n;
  Small++;
  return Total;
}

Small=0;
Total=0;
Add();
Add();
Total+Small-2;
