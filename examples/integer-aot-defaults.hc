extern I64 Unused(I64 n=20+22);
I64 Saved(U8 n=sizeof U8+276){return n;};
I64 Twice(){return Saved()+Saved();};
Twice();
