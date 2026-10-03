I64 Quotient(I64 x) { return x/2; }
I64 Remainder(I64 x) { return x%2; }
I64 Masked() { I64 x=-7; x%=64; return x; }
I64 Compare(I64 x) { return (x/1(U64))<0; }
38+(Quotient(-7)==-4)+(Remainder(-7)==-1)+(Masked()==57)+Compare(-7);
