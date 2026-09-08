extern U0 Print(U8 *fmt,...);

U8 Total=40;
U8 Text[3]="32";

U0 Bump(U8 *p) {
    (*p)++;
}

I64 Result() {
    static U8 Calls=0;
    Calls++;
    U8 local=255;
    ++local;
    I64 full=(local+=256);
    Bump(Text);
    Total+=Calls;
    ++Total;
    Print("%s",Text);
    return Total+full-256+local;
}

Result();
