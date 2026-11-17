// The sole automatic candidate in F uses the first free general slot, RSI.
// Child reserves RSI even though its explicit parameter is unused, so y uses
// RDI. The nested call must preserve F's original x.
I64 Child(I64 reg RSI reserved,I64 y)
{
  y*=3;
  return y+1;
}

I64 F(I64 x)
{
  return x+Child(0,x+2)+x;
}

F(7);
