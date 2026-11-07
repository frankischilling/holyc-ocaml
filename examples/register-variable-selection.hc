// Narrow values kept in registers can retain a wider compound-update result.
I64 WidePair(I8 x,I8 y,I64 increment)
{
    x+=increment;
    y+=increment;
    return x+y;
}

// noreg forces the updated values back through narrow stack storage.
I64 StackPair(I8 noreg x,I8 noreg y,I64 increment)
{
    x+=increment;
    y+=increment;
    return x+y;
}

WidePair(127,127,1)-StackPair(127,127,1);
