I64 counter=0;

I64 CaseValue(I64 value)
{
    counter=counter*10+value;
    return value;
}

I64 Pick(I64 value)
{
    switch (value) {
        case CaseValue(5)...CaseValue(3): return 0;
        case: return counter-11;
        default: return -1;
    }
}

Pick(6)+Pick(6)-42;
