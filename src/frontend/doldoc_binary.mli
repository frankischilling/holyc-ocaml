type record = private {
  number : int64;
  flags : int64;
  payload : string;
  declared_size : int64;
  payload_complete : bool;
  use_count : int64;
  header_offset : int;
}

type error_kind =
  | Truncated_header
  | Payload_too_large
  | Truncated_payload
  | Duplicate_record

type error = private {
  kind : error_kind;
  offset : int;
  record_number : int64 option;
  message : string;
}

type t

val decode : ?recover_normalized:bool -> string -> (t, error) result
(** Decode the [CDocBin] records stored after the first NUL in a saved DolDoc
    file. A source without trailing records produces an empty table.

    [recover_normalized] accepts records shortened by newline normalization in
    the archived TempleOS Git tree. Each recovered record states whether its
    complete declared payload remains available. *)

val find : t -> int64 -> record option
val records : t -> record list
val text_terminator : t -> int option
