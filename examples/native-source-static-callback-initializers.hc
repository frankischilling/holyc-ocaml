I64 Counter=40;
I64 Seed(){return ++Counter;}
I64 Answer(I64 value){return value+1;}
I64 Remember()
{
  static I64 (*saved)(I64 value=Seed())[3]={&Answer,saved[0],saved[0]()};
  return saved[1]();
}
Counter=100;
I64 Answer(I64 value){return 17;}
Remember();
