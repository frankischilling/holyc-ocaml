module Facts = Generated.Compiler_options

type t =
  | Echo
  | Trace
  | Warn_unused_var
  | Warn_paren
  | Warn_dup_types
  | Warn_header_mismatch
  | Externs_to_imports
  | Keep_private
  | No_reg_var
  | Globals_on_data_heap
  | No_builtin_const
  | Use_imm64

type phase =
  | Lexing
  | Parsing
  | Function_diagnostics
  | Symbol_registration
  | Linkage
  | Allocation
  | Optimization
  | Code_emission

type source_status = Defined | Source_marked_incomplete
type source = { path : string; sha256 : string }
type source_reference = { path : string; line : int }

type info = {
  option : t;
  source_name : string;
  bit_index : int;
  mask : int64;
  initially_enabled : bool;
  phases : phase list;
  source_status : source_status;
  definition_line : int;
  source_comment : string option;
  consumers : source_reference list;
}

type api = {
  state_expression : string;
  option_source : source_reference;
  get_option_source : source_reference;
  bit_set_source : source_reference;
  set_returns_previous : bool;
}

type specification = {
  option : t;
  source_name : string;
  phases : phase list;
  source_status : source_status;
}

let reference_commit = Facts.reference_commit

let sources =
  List.map
    (fun (source : Facts.source) ->
      { path = source.path; sha256 = source.sha256 })
    Facts.sources

let specifications =
  [
    {
      option = Echo;
      source_name = "OPTf_ECHO";
      phases = [ Lexing ];
      source_status = Defined;
    };
    {
      option = Trace;
      source_name = "OPTf_TRACE";
      phases = [ Parsing; Code_emission ];
      source_status = Defined;
    };
    {
      option = Warn_unused_var;
      source_name = "OPTf_WARN_UNUSED_VAR";
      phases = [ Function_diagnostics ];
      source_status = Defined;
    };
    {
      option = Warn_paren;
      source_name = "OPTf_WARN_PAREN";
      phases = [ Parsing ];
      source_status = Defined;
    };
    {
      option = Warn_dup_types;
      source_name = "OPTf_WARN_DUP_TYPES";
      phases = [ Parsing ];
      source_status = Defined;
    };
    {
      option = Warn_header_mismatch;
      source_name = "OPTf_WARN_HEADER_MISMATCH";
      phases = [ Parsing; Function_diagnostics ];
      source_status = Defined;
    };
    {
      option = Externs_to_imports;
      source_name = "OPTf_EXTERNS_TO_IMPORTS";
      phases = [ Parsing; Linkage ];
      source_status = Defined;
    };
    {
      option = Keep_private;
      source_name = "OPTf_KEEP_PRIVATE";
      phases = [ Symbol_registration ];
      source_status = Defined;
    };
    {
      option = No_reg_var;
      source_name = "OPTf_NO_REG_VAR";
      phases = [ Optimization ];
      source_status = Defined;
    };
    {
      option = Globals_on_data_heap;
      source_name = "OPTf_GLBLS_ON_DATA_HEAP";
      phases = [ Allocation; Linkage ];
      source_status = Defined;
    };
    {
      option = No_builtin_const;
      source_name = "OPTf_NO_BUILTIN_CONST";
      phases = [ Code_emission ];
      source_status = Defined;
    };
    {
      option = Use_imm64;
      source_name = "OPTf_USE_IMM64";
      phases = [ Optimization; Code_emission ];
      source_status = Source_marked_incomplete;
    };
  ]

let all = List.map (fun specification -> specification.option) specifications

let rank = function
  | Echo -> 0
  | Trace -> 1
  | Warn_unused_var -> 2
  | Warn_paren -> 3
  | Warn_dup_types -> 4
  | Warn_header_mismatch -> 5
  | Externs_to_imports -> 6
  | Keep_private -> 7
  | No_reg_var -> 8
  | Globals_on_data_heap -> 9
  | No_builtin_const -> 10
  | Use_imm64 -> 11

let compare left right = Int.compare (rank left) (rank right)
let equal left right = compare left right = 0

let specification option =
  List.find
    (fun specification -> equal specification.option option)
    specifications

let to_string option = (specification option).source_name

let of_string name =
  List.find_opt
    (fun specification -> String.equal specification.source_name name)
    specifications
  |> Option.map (fun specification -> specification.option)

let fact source_name =
  match
    List.find_opt
      (fun (entry : Facts.option_entry) -> String.equal entry.name source_name)
      Facts.options
  with
  | Some entry -> entry
  | None ->
      invalid_arg
        (Printf.sprintf "missing generated compiler option %s" source_name)

let mask_of_bit_index bit_index = Int64.shift_left 1L bit_index

let make_info specification =
  let fact = fact specification.source_name in
  let consumers =
    List.map
      (fun (reference : Facts.source_reference) ->
        { path = reference.path; line = reference.line })
      fact.consumers
  in
  {
    option = specification.option;
    source_name = fact.name;
    bit_index = fact.bit_index;
    mask = mask_of_bit_index fact.bit_index;
    initially_enabled = fact.initially_enabled;
    phases = specification.phases;
    source_status = specification.source_status;
    definition_line = fact.definition_line;
    source_comment = fact.source_comment;
    consumers;
  }

let information = List.map make_info specifications

let () =
  if List.length information <> List.length Facts.options then
    invalid_arg "typed compiler options do not cover the generated registry"

let info option =
  List.find (fun (entry : info) -> equal entry.option option) information

let mask option = (info option).mask

let of_bit_index bit_index =
  List.find_opt (fun (entry : info) -> entry.bit_index = bit_index) information
  |> Option.map (fun (entry : info) -> entry.option)

let initial_mask =
  List.fold_left
    (fun state (entry : info) ->
      if entry.initially_enabled then Int64.logor state entry.mask else state)
    0L information

let is_enabled ~mask:state option =
  not (Int64.equal (Int64.logand state (mask option)) 0L)

let set ~mask:state option enabled =
  let previous = is_enabled ~mask:state option in
  let option_mask = mask option in
  let state =
    if enabled then Int64.logor state option_mask
    else Int64.logand state (Int64.lognot option_mask)
  in
  (state, previous)

let intentional_gaps =
  List.map (fun (gap : Facts.gap) -> (gap.first, gap.last)) Facts.gaps

let api =
  let source path line = { path; line } in
  {
    state_expression = Facts.api.state_expression;
    option_source = source "Compiler/CMisc.HC" Facts.api.option_line;
    get_option_source = source "Compiler/CMisc.HC" Facts.api.get_option_line;
    bit_set_source = source "Kernel/KUtils.HC" Facts.api.bequ_line;
    set_returns_previous = Facts.api.set_returns_previous;
  }
