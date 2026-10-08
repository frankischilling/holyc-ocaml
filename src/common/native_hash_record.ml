type allocation

external create : string -> allocation = "holyc_compiler_hash_create_function"
external read : allocation -> int64 = "holyc_compiler_hash_use_count"
external increment_raw : allocation -> unit = "holyc_compiler_hash_increment"
external reset_raw : allocation -> unit = "holyc_compiler_hash_reset"

external matches : allocation -> string -> bool
  = "holyc_compiler_hash_matches_function"

external verify_storage : unit -> bool = "holyc_compiler_hash_verify_storage"

type t = { allocation : allocation; domain : Domain.id }

let original record =
  if record.domain <> Domain.self () then
    invalid_arg "compiler hash storage belongs to another original domain";
  record.allocation

let create_function ~name =
  { allocation = create name; domain = Domain.self () }

let use_count record = read (original record)
let increment record = increment_raw (original record)
let reset record = reset_raw (original record)
let matches_function record ~name = matches (original record) name
