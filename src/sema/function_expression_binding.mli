type event
type function_input

type resolution =
  | Function_binding of Function_binding_index.binding
  | Nonlocal_candidate

type occurrence
type resolved_function
type t

type error_kind =
  | Invalid_input of string
  | Publication_mismatch of {
      function_symbol : Symbol.t;
      name : string;
      declaration_index : int;
      declarator_index : int;
      expected : Symbol.t option;
    }
  | Missing_publication of {
      function_symbol : Symbol.t;
      binding : Symbol.t;
      declaration_index : int;
      declarator_index : int;
    }

type error

val make_identifier :
  name:string -> origin:Symbol.origin -> (event, string) result

val make_local_publication :
  name:string ->
  origin:Symbol.origin ->
  declaration_index:int ->
  declarator_index:int ->
  (event, string) result

val make_function :
  symbol:Symbol.t ->
  scope:Symbol_table.scope ->
  item_index:int ->
  event list ->
  (function_input, string) result

val resolve :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  bindings:Function_binding_index.t ->
  function_input list ->
  (t, error) result
(** Bind ordinary identifier occurrences in source order. Parameters are visible
    at body entry. Each local becomes visible at its publication event and
    remains visible for the rest of the function. Names that have no visible
    function binding remain explicit nonlocal candidates. *)

val functions : t -> resolved_function list
val find_function : t -> Symbol.t -> resolved_function option
val function_symbol : resolved_function -> Symbol.t
val function_scope : resolved_function -> Symbol_table.scope
val function_item_index : resolved_function -> int
val function_occurrences : resolved_function -> occurrence list
val occurrence_index : occurrence -> int
val occurrence_name : occurrence -> string
val occurrence_origin : occurrence -> Symbol.origin
val occurrence_resolution : occurrence -> resolution
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
