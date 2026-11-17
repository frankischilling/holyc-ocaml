module Encoder = X86_64_encoder

let emit_modulo ~emit ~mark_loop ~repeat_on_carry ~scratch ~left ~right ~result
    ~truncate_result =
  let memory = Encoder.X87_stack scratch in
  let st0 = Encoder.x87_register ~index:0 |> Result.get_ok in
  (* BackFA.HC:242-264 and Templates.HC:23-37 consume two materialized
     binary64 words. Neither the input nor the result can retain an x87 link. *)
  emit (Encoder.Store_stack (scratch, right));
  emit (Encoder.Fld64 memory);
  emit (Encoder.Store_stack (scratch, left));
  emit (Encoder.Fld64 memory);
  (* The result home is free after both inputs have reached the x87 stack.
     Preserve the caller's RAX even when it owns a live unrelated value. *)
  if result <> Encoder.Rax then emit (Encoder.Mov (result, Encoder.Rax));
  mark_loop ();
  emit Encoder.Fprem;
  emit Encoder.Fstsw_ax;
  (* SHR moves the original status bit 10 into carry. Higher RAX bits cannot
     affect that bit, so no second general register or scratch word is needed. *)
  emit (Encoder.Shift_immediate (Encoder.Shr, Encoder.Rax, 11L));
  repeat_on_carry ();
  emit (Encoder.Fstp64 memory);
  emit (Encoder.Ffree st0);
  emit Encoder.Fincstp;
  if truncate_result then (
    emit (Encoder.Fld64 memory);
    emit (Encoder.Fisttp64 memory));
  if result <> Encoder.Rax then emit (Encoder.Mov (Encoder.Rax, result));
  emit (Encoder.Load_stack (result, scratch))
