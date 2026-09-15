type source_stage =
  | Global_declared
  | Global_completed
  | Function_declared
  | Function_header_completed
  | Function_body_completed

type kind = private
  | Absent
  | Unavailable
  | Local
  | Source of Symbol.t * source_stage
  | Outer of Outer_environment.t * Outer_environment.binding

type t

val absent : table:Symbol_table.t -> name:string -> (t, string) result
val unavailable : table:Symbol_table.t -> name:string -> (t, string) result
val local : table:Symbol_table.t -> name:string -> (t, string) result

val source :
  table:Symbol_table.t ->
  name:string ->
  symbol:Symbol.t ->
  stage:source_stage ->
  (t, string) result

val outer :
  table:Symbol_table.t ->
  name:string ->
  environment:Outer_environment.t ->
  binding:Outer_environment.binding ->
  (t, string) result

val kind : t -> kind

val validate : table:Symbol_table.t -> name:string -> t -> (unit, string) result
(** Checked selection evidence for semantic identifier events. Absence and local
    selection are explicit; source identity and outer entry ownership are exact.
    Function stages describe the identifier observation, not later native call
    parameter or target-emission phases. *)
