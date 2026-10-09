I64 Original(){return 42;}
I64 Replacement(){return 17;}
I64 (*selected)()=&Original;
I64 Call(I64 (*target)()=selected){return target();}
selected=&Replacement;
Call();
