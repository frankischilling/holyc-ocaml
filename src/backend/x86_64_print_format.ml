module E = X86_64_encoder

type argument_kind =
  | Word
  | Unsigned_byte_pointer
  | Signed_byte_pointer
  | Other_pointer

type t = {
  format_stage : int;
  arguments_stage : int;
  argument_kinds : argument_kind array;
  scratch_stage : int;
  activation_bytes : int;
}

type branch = Always | Equal | Not_equal | Below | Less | Overflow

type 'label emitter = {
  instruction : E.instruction -> unit;
  fresh : unit -> 'label;
  mark : 'label -> unit;
  branch : branch -> 'label -> unit;
  fault : int -> 'label;
  slot : int -> E.stack_slot;
}

let fixed_scratch_slots = 16

let scratch_slots count =
  if count < 0 || count > max_int - fixed_scratch_slots then
    invalid_arg "native Print argument count exceeds its scratch representation";
  fixed_scratch_slots + count

let kind_tag = function
  | Word -> 0L
  | Unsigned_byte_pointer -> 1L
  | Signed_byte_pointer -> 2L
  | Other_pointer -> 3L

let emit emitter call =
  let count = Array.length call.argument_kinds in
  let scratch_count = scratch_slots count in
  if
    call.format_stage < 0 || call.arguments_stage < 0 || call.scratch_stage < 0
    || count > (max_int / 8) - 2
    || call.format_stage > max_int - 2
    || call.arguments_stage <> call.format_stage + 2
    || call.arguments_stage > max_int - count
    || call.scratch_stage <> call.arguments_stage + count
    || call.scratch_stage > max_int - scratch_count
    || call.activation_bytes <> (count + 2) * 8
  then invalid_arg "native Print has inconsistent staged call storage";
  let out = emitter.instruction in
  let jump = emitter.branch in
  let mark = emitter.mark in
  let fresh = emitter.fresh in
  let fault = emitter.fault in
  let stage offset = emitter.slot (call.scratch_stage + offset) in
  (* Relative format offset, argument cursor, draft length, active string
     reference/offset/kind, packed or decimal word, packed visit count, decimal
     length, sign, current byte, spare, then three qwords of digit storage. *)
  let load register offset = out (E.Load_stack (register, stage offset)) in
  let store offset register = out (E.Store_stack (stage offset, register)) in
  let constant offset value =
    out (E.Mov_imm64 (E.Rax, value));
    store offset E.Rax
  in
  let increment offset =
    load E.Rcx offset;
    out (E.Mov_imm64 (E.R8, 1L));
    out (E.Binary (E.Add, E.Rcx, E.R8));
    jump Overflow (fault 9);
    store offset E.Rcx
  in
  let charge () =
    out (E.Load_context (E.Rcx, 96));
    out (E.Test E.Rcx);
    jump Equal (fault 12);
    out (E.Dec E.Rcx);
    out (E.Store_context (96, E.Rcx))
  in
  let append () =
    (* The byte is saved before work/capacity checks clobber scratch registers. *)
    store 10 E.Rax;
    charge ();
    load E.Rax 2;
    out (E.Load_context (E.Rcx, 88));
    out (E.Cmp (E.Rax, E.Rcx));
    let available = fresh () in
    jump Below available;
    jump Always (fault 11);
    mark available;
    out (E.Load_context (E.Rdx, 80));
    out (E.Load_context (E.Rcx, 104));
    out (E.Binary (E.Add, E.Rdx, E.Rcx));
    out (E.Binary (E.Add, E.Rdx, E.Rax));
    load E.R8 10;
    out (E.Store_indirect_narrow (E.Rdx, E.Frame8, E.R8));
    out (E.Mov_imm64 (E.Rcx, 1L));
    out (E.Binary (E.Add, E.Rax, E.Rcx));
    store 2 E.Rax
  in
  let read_byte () =
    (* Work is consumed even when pointer kind, bounds or initialization fail. *)
    charge ();
    load E.Rax 5;
    out (E.Cmp_imm8 (E.Rax, 3));
    jump Equal (fault 15);
    load E.Rdx 3;
    out (E.Load_indirect (E.Rax, E.Rdx, 16));
    out (E.Test E.Rax);
    jump Less (fault 10);
    load E.Rcx 4;
    out (E.Test E.Rcx);
    jump Less (fault 10);
    out (E.Binary (E.Add, E.Rax, E.Rcx));
    jump Overflow (fault 9);
    out (E.Load_indirect (E.R8, E.Rdx, 24));
    out (E.Cmp (E.Rax, E.R8));
    let in_bounds = fresh () in
    jump Below in_bounds;
    jump Always (fault 10);
    mark in_bounds;
    out (E.Load_indirect (E.R8, E.Rdx, 8));
    let initialized = fresh () in
    out (E.Test E.R8);
    jump Equal initialized;
    out (E.Mov (E.Rcx, E.Rax));
    for _ = 1 to 3 do
      out (E.Binary (E.Add, E.Rcx, E.Rcx))
    done;
    out (E.Binary (E.Sub, E.R8, E.Rcx));
    out (E.Load_indirect_narrow (E.Rcx, E.R8, E.Frame8, E.Zero_extend));
    out (E.Test E.Rcx);
    jump Equal (fault 7);
    mark initialized;
    load E.Rcx 5;
    out (E.Cmp_imm8 (E.Rcx, 2));
    jump Equal (fault 16);
    out (E.Load_indirect (E.Rdx, E.Rdx, 0));
    out (E.Binary (E.Add, E.Rdx, E.Rax));
    out (E.Load_indirect_narrow (E.Rax, E.Rdx, E.Frame8, E.Zero_extend))
  in
  let read_format () =
    out (E.Load_stack (E.Rax, emitter.slot call.format_stage));
    store 3 E.Rax;
    load E.Rax 0;
    store 4 E.Rax;
    constant 5 1L;
    read_byte ();
    store 10 E.Rax;
    increment 0;
    load E.Rax 10
  in
  let take_argument ~pointer =
    if count = 0 then jump Always (fault 14)
    else (
      load E.Rax 1;
      out (E.Mov_imm64 (E.Rcx, Int64.of_int count));
      out (E.Cmp (E.Rax, E.Rcx));
      let present = fresh () in
      jump Below present;
      jump Always (fault 14);
      mark present;
      out (E.Mov (E.Rcx, E.Rax));
      for _ = 1 to 3 do
        out (E.Binary (E.Add, E.Rcx, E.Rcx))
      done;
      out (E.Address_stack (E.Rdx, stage fixed_scratch_slots));
      out (E.Binary (E.Add, E.Rdx, E.Rcx));
      out (E.Load_indirect (E.R8, E.Rdx, 0));
      out (E.Test E.R8);
      jump (if pointer then Equal else Not_equal) (fault 14);
      store 5 E.R8;
      out (E.Address_stack (E.Rdx, emitter.slot call.arguments_stage));
      out (E.Binary (E.Add, E.Rdx, E.Rcx));
      out (E.Load_indirect (E.Rax, E.Rdx, 0));
      store (if pointer then 3 else 6) E.Rax;
      increment 1)
  in
  out (E.Load_context (E.Rcx, 56));
  out (E.Test E.Rcx);
  jump Equal (fault 4);
  out (E.Load_context (E.Rcx, 48));
  out (E.Mov_imm64 (E.Rax, Int64.of_int call.activation_bytes));
  out (E.Cmp (E.Rcx, E.Rax));
  jump Below (fault 5);
  constant 0 0L;
  constant 1 0L;
  constant 2 0L;
  Array.iteri
    (fun index kind -> constant (fixed_scratch_slots + index) (kind_tag kind))
    call.argument_kinds;
  let format_loop = fresh () in
  let format_conversion = fresh () in
  let decimal = fresh () in
  let string = fresh () in
  let packed = fresh () in
  let percent = fresh () in
  let complete = fresh () in
  mark format_loop;
  read_format ();
  out (E.Test E.Rax);
  jump Equal complete;
  out (E.Cmp_imm8 (E.Rax, Char.code '%'));
  jump Equal format_conversion;
  append ();
  jump Always format_loop;
  mark format_conversion;
  read_format ();
  List.iter
    (fun (byte, label) ->
      out (E.Cmp_imm8 (E.Rax, Char.code byte));
      jump Equal label)
    [ ('%', percent); ('d', decimal); ('s', string); ('c', packed) ];
  jump Always (fault 13);
  mark percent;
  append ();
  jump Always format_loop;
  mark string;
  take_argument ~pointer:true;
  constant 4 0L;
  let string_loop = fresh () in
  mark string_loop;
  read_byte ();
  out (E.Test E.Rax);
  jump Equal format_loop;
  append ();
  increment 4;
  jump Always string_loop;
  mark packed;
  take_argument ~pointer:false;
  constant 7 0L;
  let packed_loop = fresh () in
  mark packed_loop;
  charge ();
  load E.Rax 6;
  out (E.Mov_imm64 (E.R8, 255L));
  out (E.Binary (E.And, E.Rax, E.R8));
  out (E.Test E.Rax);
  jump Equal format_loop;
  append ();
  increment 7;
  load E.Rax 7;
  out (E.Cmp_imm8 (E.Rax, 8));
  jump Equal format_loop;
  load E.Rax 6;
  out (E.Mov_imm64 (E.Rcx, 8L));
  out (E.Shift_cl (E.Shr, E.Rax));
  store 6 E.Rax;
  jump Always packed_loop;
  mark decimal;
  take_argument ~pointer:false;
  constant 8 0L;
  constant 9 0L;
  load E.Rax 6;
  let negative = fresh () in
  let magnitude = fresh () in
  out (E.Test E.Rax);
  jump Less negative;
  jump Always magnitude;
  mark negative;
  out (E.Unary (E.Neg, E.Rax));
  store 6 E.Rax;
  constant 9 1L;
  mark magnitude;
  let digits = fresh () in
  mark digits;
  load E.Rax 6;
  out E.Zero_edx;
  out (E.Mov_imm64 (E.Rcx, 10L));
  out E.Div_rcx;
  store 6 E.Rax;
  out (E.Mov_imm64 (E.R8, 48L));
  out (E.Binary (E.Add, E.Rdx, E.R8));
  load E.Rcx 8;
  out (E.Address_stack (E.R8, stage 12));
  out (E.Binary (E.Add, E.R8, E.Rcx));
  out (E.Store_indirect_narrow (E.R8, E.Frame8, E.Rdx));
  increment 8;
  load E.Rax 6;
  out (E.Test E.Rax);
  jump Not_equal digits;
  let emit_digits = fresh () in
  load E.Rax 9;
  out (E.Test E.Rax);
  jump Equal emit_digits;
  out (E.Mov_imm64 (E.Rax, 45L));
  append ();
  mark emit_digits;
  load E.Rcx 8;
  out (E.Dec E.Rcx);
  store 8 E.Rcx;
  out (E.Address_stack (E.Rdx, stage 12));
  out (E.Binary (E.Add, E.Rdx, E.Rcx));
  out (E.Load_indirect_narrow (E.Rax, E.Rdx, E.Frame8, E.Zero_extend));
  append ();
  load E.Rax 8;
  out (E.Test E.Rax);
  jump Not_equal emit_digits;
  jump Always format_loop;
  mark complete;
  load E.Rax 2;
  out (E.Load_context (E.Rcx, 88));
  out (E.Binary (E.Sub, E.Rcx, E.Rax));
  out (E.Store_context (88, E.Rcx));
  out (E.Load_context (E.Rcx, 104));
  out (E.Binary (E.Add, E.Rcx, E.Rax));
  out (E.Store_context (104, E.Rcx))
