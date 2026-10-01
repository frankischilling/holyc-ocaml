#define IC_TOUPPER 0x1e
public _intern IC_TOUPPER I64 ToUpper(U8 ch);
extern U0 Print(U8 *fmt,...);

I64 Convert(U8 *text)
{
  I64 i=0;
  while (text[i]) {
    text[i]=ToUpper(text[i]);
    i++;
  }
  return i;
}

U8 Text[4]="aZ!";
Print("%s:%d",Text,Convert(Text));
ToUpper('z')-48;
