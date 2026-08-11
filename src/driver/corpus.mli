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
