U64 Word(){return 7;}
U64 Variable(U64 x){return (-x)>>1;}
U64 Called(){return (-Word())>>1;}
U64 Casted(){return (-7(U64))>>1;}
I64 Compared(){return ((-Word())>>1)<0;}
38+(Variable(7)==0xFFFFFFFFFFFFFFFC)+
(Called()==0x7FFFFFFFFFFFFFFC)+
(Casted()==0x7FFFFFFFFFFFFFFC)+
(Compared()==0);
