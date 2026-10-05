type t

val create :
  table:Sema.Symbol_table.t ->
  header:Sema.Function_collection.collected_function ->
  Sema.Compiler_record.static_allocation ->
  (t, string) result
(** Retain the original live allocation, checked integer shape and exact symbol
    already inserted in the declaring partial header. This contains no initial
    values, interpreter cells, native address or execution permission. *)

val source : t -> Sema.Compiler_record.static_allocation
val symbol : t -> Sema.Symbol.t
val type_ : t -> Sema.Type.t
val shape : t -> Integer_storage_shape.t
val owns_table : t -> Sema.Symbol_table.t -> bool

val check_completed : t -> Sema.Static_local_source.t -> (unit, string) result
(** Join only the original allocation and symbol to their checked completed
    frame and location. A matching spelling or extent cannot replace either
    owner. This check grants no native function-body authority. *)
