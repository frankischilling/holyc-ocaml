// Source-derived TempleOS normalization contrasts, not runtime captures.
I64 RawBits(I64 n) { return n*1.0; }
I64 RoundedInteger(I64 n) { return n/1.0; }
(RawBits(9007199254740993)==0x4340000000000000) &&
(RoundedInteger(9007199254740993)==9007199254740992);
