class Reading {
  U8 tag;
  F64 whole;
  I8 fraction;
  F64 zero;
  U8 tail;
};

Reading sample={65,40,-254.75,-0.0,66};

I64 Check()
{
  static F64 saved[2]={1,2.25};
  static U16 count=65578.75;
  if (sample.tag!=65 || sample.tail!=66 || sizeof(Reading)!=19)
    return -1;
  if ((&sample.whole)(U64*)[0]!=0x4044000000000000 || sample.fraction!=2)
    return -2;
  if ((&sample.zero)(U64*)[0]!=0x8000000000000000)
    return -3;
  if ((&saved[0])(U64*)[0]!=0x3ff0000000000000 ||
      (&saved[1])(U64*)[0]!=0x4002000000000000)
    return -4;
  return count;
}

Check();
Check();
