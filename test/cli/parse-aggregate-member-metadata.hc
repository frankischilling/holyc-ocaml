class MetadataBox
{
  I64 age print_str "%2" "d" dft_val 38;
  I64 first tag 1, second tag 2 tag 3;
  U0 (*callback)(I64 value) doc "handler";
  U8 *names[2] public 1;
  union { F64 percentile format "%5.2f"; };
};
