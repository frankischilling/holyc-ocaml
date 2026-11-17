type t

val create :
  mode:Sema.Global_resolution.compilation_mode ->
  parameter:Sema.Function_type_resolution.parameter ->
  Sema.Function_call_expression_result.lastclass_substitution ->
  (t, string) result
(** Describe one original omitted lastclass slot. The runtime call context must
    independently join it to the exact selected header and typed call before it
    authorizes a producer. *)

val matches : t -> t -> bool
val is_aot : t -> bool
val name : t -> string option
val shared_class : t -> Sema.Type.t option

val callback_pointer :
  t -> Sema.Function_type_resolution.function_pointer option

val type_ : t -> Sema.Type.t
val has_name : t -> bool
