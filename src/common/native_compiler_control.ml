type allocation

external create_raw : int64 -> allocation = "holyc_compiler_control_create"
external options_raw : allocation -> int64 = "holyc_compiler_control_options"

external get_raw : allocation -> int -> bool
  = "holyc_compiler_control_get_option"

external set_raw : allocation -> int -> bool -> bool
  = "holyc_compiler_control_set_option"

external warnings_raw : allocation -> int64 = "holyc_compiler_control_warnings"

external increment_warning_raw : allocation -> unit
  = "holyc_compiler_control_increment_warning"

external verify_storage : unit -> bool = "holyc_compiler_control_verify_storage"

type t = { allocation : allocation; domain : Domain.id }

let original control =
  if control.domain <> Domain.self () then
    invalid_arg "compiler control storage belongs to another original domain";
  control.allocation

let create ~options =
  { allocation = create_raw options; domain = Domain.self () }

let options control = options_raw (original control)
let child control = create ~options:(options control)
let get_option control ~bit_index = get_raw (original control) bit_index

let set_option control ~bit_index enabled =
  set_raw (original control) bit_index enabled

let warning_count control = warnings_raw (original control)
let increment_warning control = increment_warning_raw (original control)
