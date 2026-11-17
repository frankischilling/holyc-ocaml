I64 Preparations=0;

F64 Offset()
{
  Preparations++;
  return 3.75;
}

F64 Metadata()
{
  Preparations++;
  return 2.75;
}

class Sample {
  U8 head;
  $$=Offset();
  $$=$$-0x400e000000000000+16;
  I64 tail value Metadata();
};

sizeof(Sample)+Preparations+16;
