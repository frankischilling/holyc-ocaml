I64 Original(){return 42;}
I64 (*saved)()=&Original;
I64 (*copies)()[2]={0,saved};
I64 Original(){return 17;}
I64 Call(I64 (*target)()){return target();}
Call(copies[1]);
