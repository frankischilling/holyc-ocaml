I64 (*words)()[2][2]={{0,34},{50,0}};
I64 (*saved)()=words[0][1];
++saved;
words[1][0]--;
I64 Read(I64 (*value)()){return value;}
Read(saved);
