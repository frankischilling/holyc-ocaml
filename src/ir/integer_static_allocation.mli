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
val cursor : t -> Integer_initializer_layout.stream

val record_native_leaf :
  t ->
  Frontend.Parser.static_initializer_preparation ->
  cell_offset:int ->
  byte_offset:int ->
  operation:Integer_initializer_layout.operation ->
  (unit, string) result
(** Record the successful original live leaf and its checked destination in the
    initializer stream. The receipt retains no computed value or native address.
    Function completion must join each receipt to the exact original typed root
    and destination; repeated, missing or reordered leaves fail that join. *)

val native_leaf_executed :
  t -> Sema.Function_call_expression_result.initializer_result -> bool

val check_native_layout :
  t ->
  (Sema.Function_call_expression_result.initializer_result * int * int) list ->
  (unit, string) result

val owns_table : t -> Sema.Symbol_table.t -> bool

val check_completed : t -> Sema.Static_local_source.t -> (unit, string) result
(** Join only the original allocation and symbol to their checked completed
    frame and location. A matching spelling or extent cannot replace either
    owner. This check grants no native function-body authority. *)
