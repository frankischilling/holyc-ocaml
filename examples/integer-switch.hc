I64 Pick(I64 value)
{
    switch (value) {
    case 1:
        return 42;
    default:
        return 0;
    }
    return 0;
}

I64 Range(I64 value)
{
    switch (value) {
    case 5...3:
        return 42;
    default:
        return 0;
    }
    return 0;
}

I64 Implicit(I64 value)
{
    switch (value) {
    case:
        return 1;
    case:
        return 42;
    default:
        return 0;
    }
    return 0;
}

I64 Flow()
{
    I64 out = 0;
    switch (1) {
    case 1:
        out = 1;
    default:
        out += 40;
    case 2:
        out += 1;
        break;
    }
    return out;
}

I64 Nested()
{
    I64 out = 0;
    while (1) {
        switch (0) {
        case 0:
            switch (1) {
            case 1:
                out = 40;
                break;
            default:
                out = 99;
            }
            out += 2;
            break;
        default:
            out = 99;
        }
        break;
    }
    return out;
}

I64 SelectorOnce()
{
    I64 value = 4;
    switch (++value) {
    case 5:
        value += 37;
        break;
    default:
        value = 0;
    }
    return value;
}

I64 Check()
{
    return (Pick(1) == 42)
         + (Range(4) == 42)
         + (Implicit(1) == 42)
         + (Flow() == 42)
         + (Nested() == 42)
         + (SelectorOnce() == 42)
         + 36;
}

Check();
