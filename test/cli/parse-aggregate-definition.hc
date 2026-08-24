public class Node
{
  Node *next;
  U8 bytes[2];
  union { I64 signed_value; U64 unsigned_value; };
}
union Payload { I64 number; U8 bytes[8]; };
Node *head;
