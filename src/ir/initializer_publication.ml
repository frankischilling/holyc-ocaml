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

type t = {
  globals : Integer_globals.t;
  entry : X87_stack.t;
  descriptions : description list;
}

let create ~globals ~entry descriptions = { globals; entry; descriptions }

let same_root left right =
  match (left, right) with
  | Prepared_global a, Prepared_global b -> a == b
  | Prepared_static (a, x), Prepared_static (b, y) -> a == b && x == y
  | _ -> false

let matches receipt ~globals ~entry descriptions =
  receipt.globals == globals && receipt.entry == entry
  && List.length receipt.descriptions = List.length descriptions
  && List.for_all2
       (fun expected actual ->
         Instruction_sequence.Instruction_id.equal expected.before actual.before
         && same_root expected.prepared_root actual.prepared_root)
       receipt.descriptions descriptions
