
I64 BMODCounter;
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
I64 BField(U8 *p,I64 offset,I64 width)
{
  I64 i,value=0;
  for (i=0;i<width;i++) value|=p[offset+i]<<(8*i);
  return value;
}
I64 BExport(U8 *name)
{
  CHashExport *entry=HashFind(name,Fs->hash_table,HTT_EXPORT_SYS_SYM);
  if (!entry) return -1;
  return entry->val;
}
I64 BPending(U8 *name)
{
  return !!HashSingleTableFind(name,Fs->hash_table,HTT_IMPORT_SYS_SYM);
}
I64 BZero(U8 *p,I64 size)
{
  I64 i;
  for (i=0;i<size;i++) if (p[i]) return FALSE;
  return TRUE;
}
U0 BRemove(U8 *name)
{
  CHash *entry;
  while (entry=HashSingleTableFind(name,Fs->hash_table,
      HTT_EXPORT_SYS_SYM|HTT_IMPORT_SYS_SYM)) HashRemDel(entry,Fs->hash_table);
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
U0 BSource(U8 *name)
{
  I64 i,size;
  U8 text[4],*bytes=FileRead(name,&size);
  BOut("SRC|");BOut(name);BOut("|");
  for (i=0;i<size;i++) {StrPrint(text,"%02X",bytes[i]);BOut(text);}
  BOut("\n");Free(bytes);
}
U0 BRun()
{
  I64 pass,base,code,data;
  CBinFile *header,*deferred;
  U8 *p,*d;
  // Original ISO sources, expanded by the original FileRead implementation.
  BSource("T:/Kernel/KLoad.HC.Z");
  BSource("T:/Kernel/KernelA.HH.Z");
  BSource("T:/Compiler/CMain.HC.Z");
  BSource("T:/Compiler/AsmResolve.HC.Z");
  for (pass=0;pass<2;pass++) {
    header=Load("B:/ALL.BIN");p=header(U8 *)+32;base=p;
    BCap(pass,"signature",header->bin_signature);
    BCap(pass,"alignment",p&15);
    BCap(pass,"rel8",BField(p,0,1));BCap(pass,"imm8",BField(p,1,1));
    BCap(pass,"rel16",BField(p,2,2));BCap(pass,"imm16",BField(p,4,2));
    BCap(pass,"rel32",BField(p,6,4));BCap(pass,"imm32",BField(p,10,4));
    BCap(pass,"rel64",BField(p,14,8));BCap(pass,"imm64",BField(p,22,8));
    BCap(pass,"zero-width",BField(p,125,2));
    BCap(pass,"abs-duplicate",(BField(p,30,4)-2*base)&0xFFFFFFFF);
    BCap(pass,"abs-single",(BField(p,34,4)-base)&0xFFFFFFFF);
    code=BExport("BMRCode");data=BExport("BMRData");
    BCap(pass,"code-duplicate",(BField(p,38,4)-2*code)&0xFFFFFFFF);
    BCap(pass,"code-single",(BField(p,42,4)-code)&0xFFFFFFFF);
    BCap(pass,"data-duplicate",BField(p,46,8)-2*data);
    BCap(pass,"data-single",BField(p,54,8)-data);
    BCap(pass,"zero-code-patch",(BField(p,62,4)-BExport("BMRZeroCode"))&0xFFFFFFFF);
    BCap(pass,"zero-data-patch",BField(p,66,8)-BExport("BMRZeroData"));
    BCap(pass,"zero-code-bytes",BZero(BExport("BMRZeroCode"),16));
    BCap(pass,"zero-data-bytes",BZero(BExport("BMRZeroData"),16));
    BCap(pass,"rel32-export",BExport("BMRRel32")-base);
    BCap(pass,"rel64-export",BExport("BMRRel64")-base);
    BCap(pass,"imm32-export",BExport("BMRImm32"));
    BCap(pass,"imm64-export",BExport("BMRImm64"));
    header=Load("B:/ALL.BIN",LDF_JUST_LOAD|LDF_NO_ABSS);p=header(U8 *)+32;
    BCap(pass,"no-abs",BField(p,30,8));
    BMODCounter=0;header=Load("B:/CODE.BIN",LDF_JUST_LOAD);
    BCap(pass,"just-load",BMODCounter);
    BMODCounter=0;header=Load("B:/CODE.BIN");p=header(U8 *)+32;
    BCap(pass,"main-order",BMODCounter);
    BCap(pass,"source-abs",(BField(p,32,4)-p)&0xFFFFFFFF);
    BCap(pass,"source-export",BExport("data")-p);
    deferred=Load("B:/WAIT.BIN",LDF_JUST_LOAD|LDF_SILENT);d=deferred(U8 *)+32;
    BCap(pass,"unresolved-byte",BField(d,0,1));
    BCap(pass,"pending",BPending("BMRLater"));
    Load("B:/PROV.BIN",LDF_JUST_LOAD);
    BCap(pass,"deferred-byte",BField(d,0,1));
    BCap(pass,"deferred-word",BField(d,1,4));
    BCap(pass,"resolved",BPending("BMRLater"));
    BCap(pass,"heap-pending",BPending("BMRLaterHeap"));
    BCap(pass,"heap-unpatched",BField(d,8,8));
    Load("B:/RES.BIN",LDF_JUST_LOAD);
    BCap(pass,"heap-resolved",BPending("BMRLaterHeap"));
    BCap(pass,"heap-final-word",BField(d,8,8));
    BRemove("BMRCode");BRemove("BMRZeroCode");BRemove("BMRData");BRemove("BMRZeroData");
    BRemove("BMRRel32");BRemove("BMRRel64");BRemove("BMRImm32");BRemove("BMRImm64");
    BRemove("BMRLater");BRemove("BMRLaterHeap");BRemove("entry");BRemove("data");
  }
  BOut("BINDONE\n");
}

BWrite("B:/CODE.BIN","eb1e0400544f5342ffffffffffffff7f44000000000000009000000000000000b800000000c70011000000c3b80000000048830004c30000000000000000000000000000140100000000200000001000000000656e7472790010200000006461746100190000000000190c00000000190c000000000901000000424d4f44436f756e74657200090d0000000000000000");
BWrite("B:/ALL.BIN","eb1e0400544f5342ffffffffffffff7fa000000000000000d00100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c31403000000001e000000220000001e0000001503000000424d52436f64650010000000260000002a000000260000001601000000424d525a65726f436f646500100000003e0000001703000000424d52446174610010000000000000002e000000360000002e0000001801000000424d525a65726f44617461001000000000000000420000001060000000424d5252656c3332001178563412424d52496d6d3332001268000000424d5252656c36340013efcdab89424d52496d6d363400197f00000000197f00000000027d000000424d5252656c333200037e000000000400000000000602000000000806000000000a0e000000424d5252656c3634000501000000424d52496d6d333200070400000000090a000000000b16000000424d52496d6d36340000000000000000000000");
BWrite("B:/WAIT.BIN","eb1e0000544f5342ffffffffffffff7f38000000000000006000000000000000cccccccccccccccccccccccccccccccccccccccccccccccc0500000000424d524c61746572000901000000000b08000000424d524c6174657248656170000000");
BWrite("B:/PROV.BIN","eb1e0000544f5342ffffffffffffff7f200000000000000050000000000000001144332211424d524c61746572001800000000424d524c61746572486561700010000000000000000000000000000000");
BWrite("B:/RES.BIN","eb1e0000544f5342ffffffffffffff7f200000000000000040000000000000001355000000424d524c6174657248656170000000000000000000000000000000");
BRun;
