U0 BOut(U8 *text)
{
  I64 i;
  for (i=0;i<StrLen(text);i++) OutU8(0xE9,text[i]);
}
U0 BCap(I64 pass,U8 *id,I64 value)
{
  U8 text[256];
  StrPrint(text,"BN|%d|%s|%X\n",pass,id,value);
  BOut(text);
}
U0 BWrite(U8 *filename,U8 *hex)
{
  I64 i,count=StrLen(hex)/2,a,b;
  U8 *bytes=MAlloc(count);
  for (i=0;i<count;i++) {
    a=hex[2*i];b=hex[2*i+1];
    if (a>='a') a=a-'a'+10; else a-='0';
    if (b>='a') b=b-'a'+10; else b-='0';
    bytes[i]=a*16+b;
  }
  FileWrite(filename,bytes,count);
  Free(bytes);
}

U0 NCapture(U8 *id,U8 *filename)
{
  I64 i,size;
  U8 *p=FileRead(filename,&size),text[32];
  BOut("NATIVE|");BOut(id);BOut("|");
  for (i=0;i<size;i++) {StrPrint(text,"%02X",p[i]);BOut(text);}
  BOut("\n");Free(p);
}
BWrite("B:/ASM.HC","61736d207b0a55534536340a494d504f5254204e56496d706f72743b0a4e56456e7472793a3a204d4f56205241582c4e56496d706f72743b205245543b0a4e56446174756d3a3a2044553332204e56456e7472793b0a7d0a");
BCap(0,"ASM-errors",Cmp("B:/ASM.HC",,"B:/ASM.BIN"));
NCapture("ASM","B:/ASM.BIN");
BWrite("B:/HOLYC.HC","23696e636c7564652022543a2f4b65726e656c2f4b65726e656c412e4848220a493634204e56476c6f62616c3b0a493634204e5646756e6374696f6e28493634207829207b72657475726e20782b4e56476c6f62616c3b7d0a4e56476c6f62616c3d31373b0a");
BCap(0,"HOLYC-errors",Cmp("B:/HOLYC.HC",,"B:/HOLYC.BIN"));
NCapture("HOLYC","B:/HOLYC.BIN");
BOut("NATIVEDONE\n");
