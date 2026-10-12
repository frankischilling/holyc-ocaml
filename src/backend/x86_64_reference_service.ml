module E = X86_64_encoder

type argument =
  | Register of E.register
  | Immediate of int64
  | Frame_address of int
  | Arena_address of int

let scratch_slots = 9

type 'label emitter = {
  abi : E.status_abi;
  emit : E.instruction -> unit;
  fresh : unit -> 'label;
  mark : 'label -> unit;
  equal : 'label -> unit;
  abort : unit -> unit;
  slot : int -> E.stack_slot;
  invoke : return_slot:E.stack_slot -> rax_slot:E.stack_slot -> unit;
}

let registers = [ E.Rax; E.Rcx; E.Rdx; E.R8; E.R9; E.R10; E.R11 ]

let emit emitter ~stage ~operation ~arguments ~site =
  let out = emitter.emit in
  let slot index = emitter.slot (stage + index) in
  out (E.Store_stack (slot 0, E.Rax));
  out (E.Store_context_imm (208, operation));
  List.iteri
    (fun index -> function
      | Register register -> out (E.Store_context (216 + (index * 8), register))
      | _ -> ())
    arguments;
  List.iteri
    (fun index -> function
      | Register _ -> ()
      | Immediate value ->
          out (E.Mov_imm64 (E.Rax, value));
          out (E.Store_context (216 + (index * 8), E.Rax))
      | Frame_address offset ->
          out
            (E.Address_frame
               (E.Rax, E.scalar_frame_slot ~offset |> Result.get_ok));
          out (E.Store_context (216 + (index * 8), E.Rax))
      | Arena_address offset ->
          out (E.Address_arena (E.Rax, E.arena_slot ~offset |> Result.get_ok));
          out (E.Store_context (216 + (index * 8), E.Rax)))
    arguments;
  emitter.invoke ~return_slot:(slot 8) ~rax_slot:(slot 0);
  match site with
  | None -> ()
  | Some site ->
      let success = emitter.fresh () in
      out (E.Load_stack (E.Rax, slot 7));
      out (E.Test E.Rax);
      out (E.Load_stack (E.Rax, slot 0));
      emitter.equal success;
      if site >= 0 then out (E.Store_context_imm (8, site));
      emitter.abort ();
      emitter.mark success

let emit_shared ~abi ~emit:out ~slot ~stage =
  let slot index = slot (stage + index) in
  List.iteri
    (fun index register -> out (E.Store_stack (slot index, register)))
    registers;
  out (E.Load_context (E.Rax, 192));
  out (E.Store_stack (slot 7, E.Rax));
  out (E.Source_arguments abi);
  out (E.Call_stack (slot 7));
  out (E.Load_stack (E.R11, slot 6));
  out (E.Store_stack (slot 7, E.Rax));
  List.iteri
    (fun index register -> out (E.Load_stack (register, slot index)))
    registers;
  out (E.Jump_stack (slot 8))
