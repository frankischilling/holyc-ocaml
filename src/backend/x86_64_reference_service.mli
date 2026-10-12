type argument =
  | Register of X86_64_encoder.register
  | Immediate of int64
  | Frame_address of int
  | Arena_address of int

val scratch_slots : int

type 'label emitter = {
  abi : X86_64_encoder.status_abi;
  emit : X86_64_encoder.instruction -> unit;
  fresh : unit -> 'label;
  mark : 'label -> unit;
  equal : 'label -> unit;
  abort : unit -> unit;
  slot : int -> X86_64_encoder.stack_slot;
  invoke :
    return_slot:X86_64_encoder.stack_slot ->
    rax_slot:X86_64_encoder.stack_slot ->
    unit;
}

val emit :
  'label emitter ->
  stage:int ->
  operation:int ->
  arguments:argument list ->
  site:int option ->
  unit
(** Native ownership bookkeeping through the private context callback. All
    allocated registers, the instruction budget and arena/context bases survive
    the C ABI call. The generated callable frame supplies its ABI home space. *)

val emit_shared :
  abi:X86_64_encoder.status_abi ->
  emit:(X86_64_encoder.instruction -> unit) ->
  slot:(int -> X86_64_encoder.stack_slot) ->
  stage:int ->
  unit
(** Emit the shared service body within its caller's fixed frame. Entry and
    return use jumps, so this adds no generated activation or stack charge. *)
