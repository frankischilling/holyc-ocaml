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

external errors_raw : allocation -> int64 = "holyc_compiler_control_errors"

external increment_error_raw : allocation -> unit
  = "holyc_compiler_control_increment_error"

external verify_storage : unit -> bool = "holyc_compiler_control_verify_storage"

external has_return_raw : allocation -> bool
  = "holyc_compiler_control_has_return"

external set_has_return_raw : allocation -> bool -> unit
  = "holyc_compiler_control_set_has_return"

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
let error_count control = errors_raw (original control)
let increment_error control = increment_error_raw (original control)
let has_return control = has_return_raw (original control)

let set_has_return control enabled =
  set_has_return_raw (original control) enabled
