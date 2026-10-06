extern I64 Answer(I64 value=41);
I64 Old(){return Answer();}
I64 Answer(I64 value){return value+1;}
I64 Answer(I64 value){return 100;}
Old();
