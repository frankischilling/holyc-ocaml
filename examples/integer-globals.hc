I64 Total;

I64 AddTo(I64 n)
{
    Total = Total + n;
    return Total;
}

Total = 0;
AddTo(20);
AddTo(22);
Total;
