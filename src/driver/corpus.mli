type status = Tokenizes | Lexer_diagnostics | Read_error | Internal_error

type diagnostic = {
  code : string;
  severity : string;
  message : string;
  line : int;
  column : int;
  start : int;
  stop : int;
}

type file_result = {
  path : string;
  bytes : int64 option;
  lexed_bytes : int64 option;
  tokens : int64;
  diagnostic_count : int;
  nul_terminated : bool;
  binary_payload_bytes : int64;
  status : status;
  first_diagnostic : diagnostic option;
  failure_message : string option;
}

type t

val lex_tree :
  ?max_file_bytes:int ->
  reference_commit:string ->
  root:string ->
  unit ->
  (t, string) result
(** Lex every [*.HC], [*.HH], and [*.PRJ] file below [root]. This entry point
    does not validate Git metadata; callers supply the revision label recorded
    in the report. *)

val lex_reference :
  ?max_file_bytes:int ->
  expected_commit:string ->
  root:string ->
  unit ->
  (t, string) result
(** Verify that [root] is a clean Git worktree at [expected_commit], then lex
    every committed HolyC source blob. Git object bytes make the report
    independent of checkout line-ending conversion. The worktree is checked
    again after the scan, and no reference file is executed. *)

val reference_commit : t -> string
val files : t -> file_result list
val file_count : t -> int
val tokenizes_count : t -> int
val failure_count : t -> int
val total_bytes : t -> int64
val total_lexed_bytes : t -> int64
val total_tokens : t -> int64
val nul_terminated_count : t -> int
val total_binary_payload_bytes : t -> int64
val has_failures : t -> bool
val status_name : status -> string
val human : t -> string
val json : t -> string
val error_json : string -> string

module Parse : sig
  type status =
    | Parses
    | Frontend_diagnostics
    | Parser_diagnostics
    | Read_error
    | Internal_error

  type diagnostic = {
    code : string;
    severity : string;
    message : string;
    path : string;
    line : int;
    column : int;
  }

  type file_result = {
    path : string;
    bytes : int64 option;
    diagnostic_count : int;
    error_count : int;
    warning_count : int;
    note_count : int;
    diagnostic_codes : (string * int) list;
    status : status;
    first_error : diagnostic option;
    failure_message : string option;
  }

  type t

  val tree :
    ?max_file_bytes:int ->
    reference_commit:string ->
    compilation_mode:Frontend.Preprocessor.compilation_mode ->
    root:string ->
    unit ->
    (t, string) result
  (** Parse every [*.HC], [*.HH], and [*.PRJ] file below [root] in an isolated
      session. This entry point does not validate Git metadata; callers supply
      the revision label recorded in the report. *)

  val reference :
    ?max_file_bytes:int ->
    expected_commit:string ->
    compilation_mode:Frontend.Preprocessor.compilation_mode ->
    root:string ->
    unit ->
    (t, string) result
  (** Verify that [root] is a clean Git worktree at [expected_commit], then
      parse every committed HolyC source blob. Root sources come from Git
      objects, includes stay inside the verified checkout, and no source is
      executed by this scan. *)

  val reference_commit : t -> string
  val compilation_mode : t -> Frontend.Preprocessor.compilation_mode
  val files : t -> file_result list
  val file_count : t -> int
  val parses_count : t -> int
  val frontend_diagnostic_count : t -> int
  val parser_diagnostic_count : t -> int
  val read_error_count : t -> int
  val internal_error_count : t -> int
  val failure_count : t -> int
  val total_bytes : t -> int64
  val diagnostic_count : t -> int
  val error_count : t -> int
  val warning_count : t -> int
  val note_count : t -> int
  val diagnostic_codes : t -> (string * int) list
  val has_failures : t -> bool
  val status_name : status -> string
  val human : t -> string
  val json : t -> string
  val error_json : string -> string
end
