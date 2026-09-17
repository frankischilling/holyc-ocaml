I64 Forward(I64 n)
{
    I64 value = 1;
    goto done;
    value = 99;
done:
    return value + n;
}

I64 Backward(I64 n)
{
    I64 sum = 0;
again:
    sum += n;
    n--;
    if (n)
        goto again;
    return sum * 2;
}

I64 SameA()
{
same:
    return 20;
}

I64 SameB()
{
same:
    return 22;
}

U0 Labels()
{
    goto done;
first:
second:
    return;
done:
    goto tail;
tail:
}

I64 Nested()
{
    I64 n = 0;
    while (1) {
        if (n == 2)
            break;
        n++;
    }
    goto done;
    n = 99;
done:
    return n + 40;
}

I64 ForOrder()
{
    I64 n = 0;
    for (n = 0; n < 2; goto again) {
        n++;
again:
        if (n == 1)
            n++;
        else
            goto done;
    }
done:
    return n + 40;
}

I64 Recur(I64 n)
{
    I64 saved = n;
    if (n)
        goto recurse;
    return 0;
recurse:
    return saved + Recur(n - 1);
}

I8 Defaulted(I8 n = 298)
{
    goto done;
    n = 0;
done:
    return n;
}

U0 Visit(U8 n)
{
again:
    if (!n)
        goto done;
    n--;
    goto again;
done:
    return;
}

I64 Check()
{
    Labels();
    Visit(2);
    return (Forward(41) == 42)
         + (Backward(6) == 42)
         + (SameA() + SameB() == 42)
         + (Nested() == 42)
         + (ForOrder() == 42)
         + (Recur(6) * 2 == 42)
         + (Defaulted() == 42)
         + 35;
}

Check();
