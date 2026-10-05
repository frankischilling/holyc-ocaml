type t

val create :
  table:Symbol_table.t ->
  header:Function_collection.collected_function ->
  allocation:Compiler_record.static_allocation ->
  selection:Frontend.Parser.reference_selection ->
  (t, string) result

val allocation : t -> Compiler_record.static_allocation
val symbol : t -> Symbol.t
val type_ : t -> Type.t
val dimensions : t -> int64 list

val owns_identifier : t -> Frontend.Ast.identifier -> bool
(** The exact identifier whose original token selected this private allocation.
    Another occurrence selecting the same local retains its own receipt. *)
