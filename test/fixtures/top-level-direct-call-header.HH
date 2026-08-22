I64 IncludedCall(I64 value);
I64 (*IncludedCallback)(I64 value);
I64 (*IncludedCallbacks)(I64 value)[2];
class IncludedBox {I64 (*Member)(I64 value);};
IncludedBox IncludedObject;
