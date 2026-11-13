I64 ReadGrid(I64 row,I64 column)
{
  I64 values[2][3];
  values[(0)][1-1]=20;
  values[1][2]=22;

  // Keep the original 24-byte row stride and 8-byte element stride.
  // Grouping and +0 follow the source optimizer's phases.
  return values[0][0]+values[(row)][column+0];
}

ReadGrid(1,2); // 42
