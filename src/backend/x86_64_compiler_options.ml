module E = X86_64_encoder

type t = {
  index_stage : int;
  value_stage : int option;
  result_stage : int;
  scratch_stage : int;
}

let scratch_slots = 4

let emit (emitter : 'label X86_64_print_format.emitter) (call : t) =
  let out = emitter.instruction in
  let slot index = emitter.slot (call.scratch_stage + index) in
  out (E.Load_context (E.Rax, 184));
  out (E.Test E.Rax);
  emitter.branch Equal (emitter.fault 30);
  out (E.Store_stack (slot 0, E.Rax));
  out (E.Store_stack (slot 1, E.R11));
  out (E.Store_stack (slot 2, E.R9));
  out (E.Load_context (E.Rax, 16));
  out (E.Binary (E.Sub, E.Rax, E.R10));
  out (E.Store_context (24, E.Rax));
  out (E.Load_stack (E.Rdx, emitter.slot call.index_stage));
  (match call.value_stage with
  | None -> out (E.Mov_imm64 (E.R8, 0L))
  | Some stage ->
      out (E.Load_stack (E.R8, emitter.slot stage));
      out (E.Mov_imm64 (E.Rax, 0xffL));
      out (E.Binary (E.And, E.R8, E.Rax));
      out (E.Test E.R8);
      out (E.Setcc (E.NE, E.R8));
      out (E.Movzx8 (E.R8, E.R8));
      out (E.Mov_imm64 (E.Rax, 1L));
      out (E.Binary (E.Add, E.R8, E.Rax)));
  out (E.Compiler_option_arguments emitter.status_abi);
  out (E.Call_stack (slot 0));
  out (E.Store_stack (slot 3, E.Rax));
  out (E.Load_stack (E.R11, slot 1));
  out (E.Load_stack (E.R9, slot 2));
  out (E.Load_context (E.R10, 16));
  out (E.Load_context (E.Rcx, 24));
  out (E.Binary (E.Sub, E.R10, E.Rcx));
  out (E.Load_context (E.Rcx, 0));
  out (E.Test E.Rcx);
  emitter.branch Not_equal (emitter.fault 30);
  out (E.Load_stack (E.Rax, slot 3));
  out (E.Store_stack (emitter.slot call.result_stage, E.Rax))
