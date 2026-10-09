class Pair{I64 a;I64 b;};
I64 Counter=0;
I64 Seed(){return ++Counter+41;}
I64 Consume(Pair (*word)()){return word;}
I64 Remember()
{
  static Pair (*words)()[2]={0,42};
  static I64 (*saved)(Pair (*word)()=Seed())=&Consume;
  if(sizeof(words)!=16)return 0;
  if(words[1]!=42)return 0;
  return saved();
}
class Pair{U8 different;};
Counter=99;
Remember();
Remember();
