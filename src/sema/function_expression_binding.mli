type event
type function_input

type resolution =
  | Function_binding of Function_binding_index.binding
  | Nonlocal_candidate

type occurrence
type suppression
type initializer_use_reset

type binding_event =
  | Bound_use of occurrence
  | No_warn_suppression of suppression
  | Initializer_use_reset of initializer_use_reset

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
  | Suppression_mismatch of {
      function_symbol : Symbol.t;
      name : string;
    }
  | Initializer_reset_mismatch of {
      function_symbol : Symbol.t;
      name : string;
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

val make_no_warn_suppression :
  name:string -> origin:Symbol.origin -> (event, string) result

val make_initializer_use_reset :
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
val function_binding_events : resolved_function -> binding_event list
val function_occurrences : resolved_function -> occurrence list
val function_suppressions : resolved_function -> suppression list

val function_initializer_use_resets :
  resolved_function -> initializer_use_reset list

val occurrence_index : occurrence -> int
val occurrence_name : occurrence -> string
val occurrence_origin : occurrence -> Symbol.origin
val occurrence_resolution : occurrence -> resolution
val suppression_index : suppression -> int
val suppression_name : suppression -> string
val suppression_origin : suppression -> Symbol.origin
val suppression_binding : suppression -> Function_binding_index.binding
val initializer_use_reset_index : initializer_use_reset -> int

val initializer_use_reset_origin :
  initializer_use_reset -> Symbol.origin

val initializer_use_reset_binding :
  initializer_use_reset -> Function_binding_index.binding

val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
