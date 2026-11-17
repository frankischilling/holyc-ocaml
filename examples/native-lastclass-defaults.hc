// The previous supplied I64 value has forwarded class I64i.
// The ordinary middle default leaves that class selected for both names.
I64 Inspect(I64 value,U8 *name=lastclass,I64 ordinary=11,
            U8 *again=lastclass)
{
  return name[3]+again[3]+ordinary;
}

Inspect(7,,,);
