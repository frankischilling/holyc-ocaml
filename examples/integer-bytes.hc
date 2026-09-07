I64 Sum(U8 *p) { return p[0] + p[1]; }
I64 F()
{
    U8 a[3];
    a[0] = 40;
    a[1] = 2;
    a[2] = 0;
    return Sum(a);
}
F();
