I64 Read(U8 *p) { return p[0] + p[1]; }
I64 F()
{
    U8 *s="*";
    return Read(s);
}
F();
