I64 answer=42;

I64 CheckVoidDeclarations() {
  U0 first,second;
  return answer+sizeof(first)+sizeof(second);
}

CheckVoidDeclarations();
