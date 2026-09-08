type prepared_root =
  | Prepared_global of
      Sema.Function_call_expression_result.top_level_root_result
  | Prepared_static of
      Integer_globals.static_slot
      * Sema.Function_call_expression_result.initializer_result

type description = {
  prepared_root : prepared_root;
  before : Instruction_sequence.Instruction_id.t;
}

type t

val create :
  globals:Integer_globals.t -> entry:X87_stack.t -> description list -> t
(** Internal receipt minted only by complete entry lowering. This constructor is
    absent from the maintained public library interface. *)

val matches :
  t ->
  globals:Integer_globals.t ->
  entry:X87_stack.t ->
  description list ->
  bool
