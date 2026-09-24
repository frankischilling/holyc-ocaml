#define IC_STRLEN 0x84
public _intern IC_STRLEN I64 StrLen(U8 *s);

U8 Text[5]={65,66,67,68,0};

I64 MeasuredTail()
{
    Text[2]=0;
    if (StrLen(&Text[1])<24)
        return 42;
    return 0;
}

MeasuredTail();
