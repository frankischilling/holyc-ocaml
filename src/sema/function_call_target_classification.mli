type t
type error_kind = Invalid_input of string
type error

val classify :
  records:Function_record_classification.t ->
  Function_call_expression_result.direct_call ->
  (t, error) result
(** Join a typed function-scope direct call to the exact declaration-order
    function record retained for its source position. *)

val source : t -> Function_call_expression_result.direct_call
val declaration : t -> Function_resolution.resolved_declaration

val classified_declaration :
  t -> Function_record_classification.classified_declaration

val record : t -> Function_record_classification.record
val call_access : t -> Function_record_classification.call_access
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
