class Sample {
  U8 tag;
  F64 values[2];
  I16 whole;
  U8 tail;
};

F64 Total=1.5;
F64 Grid[2][2]={{1.0,2.0},{3.0,4.0}};
Sample Record={65,{1.25,2.5},0,66};

F64 Read(Sample *sample) {
  return sample->values[0]+sample->values[1];
}

F64 Tick(F64 increment) {
  static F64 saved[2]={0.5,1.0};
  Total+=increment;
  Grid[1][0]-=0.5;
  Record.values[0]*=2.0;
  Record.values[1]/=2.0;
  saved[0]+=0.25;
  Record.whole=Read(&Record);
  return saved[0]+saved[1];
}

I64 Check() {
  F64 first=Tick(1.0);
  F64 second=Tick(1.0);
  if(first!=1.75)return -1;
  if(second!=2.0)return -2;
  if(Total!=3.5)return -3;
  if(Grid[1][0]!=2.0)return -4;
  if(Record.values[0]!=5.0)return -5;
  if(Record.values[1]!=0.625)return -6;
  if(Record.whole!=5)return -7;
  F64 widened=Record.whole;
  if(widened!=5.0)return -8;
  I64 rounds=0;
  while(Total<4.5) {
    Total+=0.5;
    rounds++;
  }
  I64 index;
  for(index=0;index<2;index++)
    Grid[0][index]+=1.0;
  do {
    Total-=0.5;
  } while(Total>3.5);
  if(rounds!=2)return -9;
  if(Grid[0][0]!=2.0)return -10;
  if(Grid[0][1]!=3.0)return -11;
  if(Record.tag!=65 || Record.tail!=66 || sizeof(Sample)!=20)return -12;
  return 42;
}

Check();
