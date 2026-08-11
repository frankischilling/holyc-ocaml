type source_reference = { path : string; line : int }

type flag_entry = {
  name : string;
  bit_index : int;
  mask : int64;
  definition_line : int;
  consumers : source_reference list;
}

type group_entry = {
  name : string;
  mask : int64;
  members : string list;
  definition_line : int;
  consumers : source_reference list;
}

type transition_operation =
  | Add_bits of int64
  | Replace_preserving of { keep_mask : int64; add_mask : int64 }

type transition_entry = {
  name : string;
  spelling : string;
  operation : transition_operation;
  sources : source_reference list;
}

type behavior = {
  symbol_flag_transfer : source_reference;
  public_type_transfer : source_reference;
  automatic_ret1 : source_reference;
  variadic_declaration : source_reference;
  variadic_optimizer : source_reference;
  caller_cleanup : source_reference;
  try_cleanup : source_reference;
  internal_dispatch : source_reference;
  internal_clobber : source_reference;
  symbol_lookup_exclusion : source_reference;
  interrupt_restore : source_reference;
  interrupt_return : source_reference;
  interrupt_error_code : source_reference;
  callee_cleanup : source_reference;
  interrupt_save : source_reference;
}

type tables = {
  shared_flags : flag_entry list;
  function_flags : flag_entry list;
  staging_flags : flag_entry list;
  groups : group_entry list;
  transitions : transition_entry list;
  behavior : behavior;
}

type error = { path : string option; line : int option; message : string }

val error_to_string : error -> string
val verify_sha256 : expected:string -> string -> (unit, error) result

val apply_transition :
  transition_entry -> int64 -> int64

val parse :
  kernel_source:string ->
  compiler_source:string ->
  prs_stmt_source:string ->
  prs_var_source:string ->
  prs_exp_source:string ->
  opt_pass3_source:string ->
  opt_pass6_source:string ->
  opt_pass789a_source:string ->
  fun_seg_source:string ->
  (tables, error) result
