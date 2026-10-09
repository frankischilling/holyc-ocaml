F64 (*Words)()[2]={0xFFFFFFFFFFFFFFFF,0x8000000000000000};

I64 Check(I64 (*cb)()=17)
{
  if (Words[0]==-1 && Words[1]==0x8000000000000000 && cb==17)
    return 42;
  return 0;
}

Check();
