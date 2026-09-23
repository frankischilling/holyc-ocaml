I16 Numbers[2][2]={{40,0},{0,0}};
U8 Digits[3]="41";

I64 Sum(I16 *row)
{
  static U8 Addend[2]={1,1};
  U8 *scratch="00";
  scratch[0]=Digits[0];
  scratch[1]=Digits[1]+Addend[1];
  row[0]=scratch[1]-'0';
  return Numbers[0][0]+row[0];
}

Sum(Numbers[1]);
