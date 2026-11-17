U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 ECAddresses(){U8 a[0],b[0];U8 *p=a;return (p==a)*40+(a==b)*2;}
POValue("LOCAL_ADDRESSES",ECAddresses());
I64 ECCalls=0;
I64 ECSize(){ECCalls++;return 0;}
I64 ECValues[0][ECSize()]={};
I64 ECUnused(){static I64 a[ECSize()]={};return sizeof(a);}
POValue("DIMENSION_CALLS",ECCalls);
POValue("STATIC_SUM",ECUnused()+ECUnused());
POValue("CALLS_AFTER",ECCalls);
class ECBox{U8 empty[];U8 value;};ECBox ECObject={"AB",42};
POValue("FOLLOWING_BYTE",ECObject.value);
POValue("PACKED_BYTES",sizeof(ECBox));
I64 ECFractional[0.5]={},ECNegativeFractional[-0.5]={};
POValue("FRACTIONAL_BYTES",sizeof(ECFractional));
POValue("NEGATIVE_FRACTIONAL_BYTES",sizeof(ECNegativeFractional));
POOut("POINTER_DONE\n");
