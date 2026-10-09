extern U0 Print(U8 *fmt,...);
U0 (*p)(U8 *fmt,...)=&Print;
I64 Run(U0 (*q)(U8 *fmt,...)=p)
{
  q("%s%c","A",'B');
  return 42;
}
U0 Print(U8 *fmt,...) {}
p=0;
Run();
