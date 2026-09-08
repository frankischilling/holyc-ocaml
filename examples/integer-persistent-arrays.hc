extern U0 Print(U8 *fmt,...);

U8 Message[3]="42";
I64 Totals[2][2]={{19,20},{0,0}};

U0 Bump(I64 *row) {
    row[0]=row[0]+1;
    row[1]=row[1]+1;
}

I64 Result() {
    static U8 Extra[2]={0,1};
    Bump(Totals[0]);
    return Totals[0][0]+Totals[0][1]+Extra[1];
}

Print("%s",Message);
Result();
