type allocation

external create : string -> allocation = "holyc_compiler_hash_create_function"

external create_record : string -> int32 -> allocation
  = "holyc_compiler_hash_create_record"

external hash_string : string -> int64 = "holyc_compiler_hash_str"
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

let create_prefix ~name ~type_bits =
  { allocation = create_record name type_bits; domain = Domain.self () }

let original_record = original

module Table = struct
  type storage

  external create_raw : int -> storage = "holyc_compiler_table_create"
  external add_raw : storage -> allocation -> unit = "holyc_compiler_table_add"
  external link_raw : storage -> storage -> unit = "holyc_compiler_table_link"

  external find_checked :
    storage -> allocation -> string * int32 * int64 * bool * bool -> bool
    = "holyc_compiler_table_find_checked"

  external verify_storage : unit -> bool = "holyc_compiler_table_verify_storage"

  type record = t
  type t = { storage : storage; domain : Domain.id }

  let original table =
    if table.domain <> Domain.self () then
      invalid_arg "compiler hash table belongs to another original domain";
    table.storage

  let create ~size = { storage = create_raw size; domain = Domain.self () }
  let add table record = add_raw (original table) (original_record record)
  let set_next table next = link_raw (original table) (original next)

  let query ~increment ?(instance = 1L) ?(chain = false) table ~expected ~name
      ~mask =
    if instance < 0L then invalid_arg "compiler hash instance is negative";
    find_checked (original table) (original_record expected)
      (name, mask, instance, chain, increment)

  let selects = query ~increment:false
  let find = query ~increment:true
end
