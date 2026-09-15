#exe {I64 N=38;extern I64 Joined(I64 a=++N);}
#exe {extern I64 Joined(I64 b=++N);}
#exe {I64 Joined(I64 value=++N){return value+1;}}
#exe {StreamPrint("%d;",Joined()+Joined()-N-1);}
