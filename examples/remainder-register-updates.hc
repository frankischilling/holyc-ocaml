I64 RegisterRemainder(I8 value,I64 step,I64 divisor)
{
  value+=step;
  value%=divisor;
  return value;
}

RegisterRemainder(127,2,227)-87;
