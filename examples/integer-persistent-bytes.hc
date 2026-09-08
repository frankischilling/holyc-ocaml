U8 Total=0;

U0 Save(U8 *p,I64 value)
{
    *p=value;
}

I64 Next()
{
    static U8 count=40;
    count=count+1;
    Save(&Total,count);
    return Total;
}

Next();
Next();
