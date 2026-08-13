extern class CDC;

U0 Configure()
{
  U0 reg R15 (*callback)(CDC *dc);
  callback;
}
