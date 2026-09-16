// F's parameter keeps the C selected before the nested directive shadows it.
class C {};
I64 F(C *p)#exe {class C {};}{return 0;}
42;
