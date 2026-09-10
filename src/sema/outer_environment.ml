type compilation_mode = Function_resolution.compilation_mode = Jit | Aot
type table_kind = Jit_task of int | Aot_parent of int | Assembler

type record_kind =
  | Aggregate
  | Function
  | Global_variable
  | Export_system_symbol

type global_declarator_kind =
  | Object_global
  | Function_pointer_global of Function_type_resolution.function_pointer

type global_metadata = {
  type_reference : Type_reference.t;
  declarator_kind : global_declarator_kind;
  array_rank : int;
}

type function_metadata = {
  declaration : Function_resolution.resolved_declaration;
  records : Function_record_classification.t;
  classified_declaration : Function_record_classification.classified_declaration;
}

type entry = {
  symbol : Symbol.t;
  record_kind : record_kind;
  entry_index : int;
  global_metadata : global_metadata option;
  function_metadata : function_metadata option;
}

module Int_set = Set.Make (Int)
module String_map = Map.Make (String)

type table = {
  table_kind : table_kind;
  table_index : int;
  entries : entry list;
  by_name : entry String_map.t;
}

type binding = { table : table; entry : entry }

type t = {
  symbol_table : Symbol_table.t;
  compilation_mode : compilation_mode;
  tables : table list;
  function_versions : binding list;
}

type error_kind = Invalid_input of string
type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let invalid_input ?origin message =
  { code = "HCSEMA0022"; kind = Invalid_input message; origin }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with
  | Invalid_input message -> message

let error_to_string error = error.code ^ ": " ^ error_message error

let compilation_mode_name = function
  | Jit -> "jit"
  | Aot -> "aot"

let table_kind_name = function
  | Jit_task depth -> Printf.sprintf "jit-task-%d" depth
  | Aot_parent depth -> Printf.sprintf "aot-parent-%d" depth
  | Assembler -> "assembler"

let record_kind_name = function
  | Aggregate -> "aggregate"
  | Function -> "function"
  | Global_variable -> "global-variable"
  | Export_system_symbol -> "export-system-symbol"

let symbol_kind_for_record = function
  | Aggregate -> Symbol.Aggregate_type
  | Function -> Symbol.Function
  | Global_variable -> Symbol.Global_variable
  | Export_system_symbol -> Symbol.Assembler_symbol

let make_global_metadata ~type_reference ~declarator_kind ~array_rank =
  if array_rank < 0 then
    Error (invalid_input "outer global array rank cannot be negative")
  else Ok { type_reference; declarator_kind; array_rank }

let make_entry_with_metadata ~symbol ~record_kind ~entry_index global_metadata =
  if entry_index < 0 then
    Error (invalid_input "outer environment entry index cannot be negative")
  else if
    not
      (Symbol.equal_kind (Symbol.kind symbol)
         (symbol_kind_for_record record_kind))
  then
    Error
      (invalid_input ~origin:(Symbol.origin symbol)
         "outer environment entry has the wrong semantic symbol kind")
  else if Option.is_some global_metadata && record_kind <> Global_variable then
    Error
      (invalid_input ~origin:(Symbol.origin symbol)
         "only an outer global-variable record can carry global metadata")
  else
    Ok
      {
        symbol;
        record_kind;
        entry_index;
        global_metadata;
        function_metadata = None;
      }

let make_entry ~symbol ~record_kind ~entry_index =
  make_entry_with_metadata ~symbol ~record_kind ~entry_index None

let make_global_entry ~symbol ~entry_index ~global_metadata =
  make_entry_with_metadata ~symbol ~record_kind:Global_variable ~entry_index
    (Some global_metadata)

let make_function_metadata ~records ~declaration =
  match
    records |> Function_record_classification.declarations
    |> List.find_opt (fun classified ->
        Function_record_classification.classified_declaration_source classified
        == declaration)
  with
  | None ->
      Error
        (invalid_input
           "outer function declaration does not belong to its classification")
  | Some classified_declaration ->
      Ok { declaration; records; classified_declaration }

let make_function_entry ~entry_index ~function_metadata =
  let symbol =
    Function_resolution.resolved_declaration_identity_symbol
      function_metadata.declaration
  in
  match make_entry ~symbol ~record_kind:Function ~entry_index with
  | Error _ as error -> error
  | Ok entry -> Ok { entry with function_metadata = Some function_metadata }

let valid_table_kind = function
  | Jit_task depth | Aot_parent depth -> depth >= 0
  | Assembler -> true

let symbol_number symbol = Symbol.id symbol |> Symbol.Id.to_int

let validate_entries entries =
  let rec loop expected_index seen = function
    | [] -> Ok ()
    | entry :: rest ->
        let number = symbol_number entry.symbol in
        if entry.entry_index <> expected_index then
          Error
            (invalid_input "outer environment entry indexes are not contiguous")
        else if Int_set.mem number seen then
          Error
            (invalid_input
               ~origin:(Symbol.origin entry.symbol)
               "outer environment table repeats a semantic symbol")
        else loop (expected_index + 1) (Int_set.add number seen) rest
  in
  loop 0 Int_set.empty entries

let make_table ~table_kind ~table_index entries =
  if table_index < 0 then
    Error (invalid_input "outer environment table index cannot be negative")
  else if not (valid_table_kind table_kind) then
    Error (invalid_input "outer environment table depth cannot be negative")
  else
    match validate_entries entries with
    | Error _ as error -> error
    | Ok () ->
        let by_name =
          List.fold_left
            (fun by_name entry ->
              String_map.add (Symbol.name entry.symbol) entry by_name)
            String_map.empty entries
        in
        Ok { table_kind; table_index; entries; by_name }

let validate_table_indexes tables =
  let rec loop expected_index = function
    | [] -> Ok ()
    | table :: rest ->
        if table.table_index <> expected_index then
          Error
            (invalid_input "outer environment table indexes are not contiguous")
        else loop (expected_index + 1) rest
  in
  loop 0 tables

let validate_jit_roles tables =
  let rec tasks expected_depth = function
    | { table_kind = Jit_task depth; _ } :: rest when depth = expected_depth ->
        tasks (expected_depth + 1) rest
    | [ { table_kind = Assembler; _ } ] when expected_depth > 0 -> Ok ()
    | _ ->
        Error
          (invalid_input
             "JIT outer tables must contain current and parent tasks in depth \
              order followed by one assembler table")
  in
  tasks 0 tables

let validate_aot_roles tables =
  let rec parents expected_depth = function
    | { table_kind = Aot_parent depth; _ } :: rest when depth = expected_depth
      -> parents (expected_depth + 1) rest
    | [ { table_kind = Assembler; _ } ] -> Ok ()
    | _ ->
        Error
          (invalid_input
             "AOT outer tables must contain enclosing compilations in depth \
              order followed by one assembler table")
  in
  parents 0 tables

let validate_roles compilation_mode tables =
  match compilation_mode with
  | Jit -> validate_jit_roles tables
  | Aot -> validate_aot_roles tables

let type_is_owned symbol_table type_ =
  match Type.base type_ with
  | Type.Primitive _ -> true
  | Type.Aggregate symbol -> Symbol_table.owns_symbol symbol_table symbol

let reference_is_owned symbol_table reference =
  type_is_owned symbol_table (Type_reference.resolved_type reference)

let rec declarator_is_owned symbol_table = function
  | Function_type_resolution.Object -> true
  | Function_type_resolution.Function_pointer pointer ->
      pointer_is_owned symbol_table pointer

and pointer_is_owned symbol_table pointer =
  pointer |> Function_type_resolution.function_pointer_signature
  |> signature_is_owned symbol_table

and signature_is_owned symbol_table signature =
  signature |> Function_type_resolution.signature_parameters
  |> List.for_all (fun parameter ->
      reference_is_owned symbol_table
        (Function_type_resolution.parameter_type_reference parameter)
      && declarator_is_owned symbol_table
           (Function_type_resolution.parameter_declarator_kind parameter))

let global_metadata_is_owned symbol_table metadata =
  reference_is_owned symbol_table metadata.type_reference
  &&
  match metadata.declarator_kind with
  | Object_global -> true
  | Function_pointer_global pointer -> pointer_is_owned symbol_table pointer

let function_header_is_owned symbol_table header =
  Symbol_table.owns_symbol symbol_table
    (Function_type_resolution.function_symbol header)
  && Symbol_table.owns_scope symbol_table
       (Function_type_resolution.function_scope header)
  && reference_is_owned symbol_table
       (Function_type_resolution.function_return_type header)
  && signature_is_owned symbol_table
       (Function_type_resolution.function_signature header)
  && List.for_all
       (fun binding ->
         Symbol_table.owns_symbol symbol_table
           (Function_type_resolution.parameter_binding_symbol binding))
       (Function_type_resolution.function_parameter_bindings header)
  && Option.fold ~none:true
       ~some:(fun bindings ->
         List.for_all
           (fun binding ->
             Symbol_table.owns_symbol symbol_table
               (Function_type_resolution.synthetic_binding_symbol binding)
             && type_is_owned symbol_table
                  (Function_type_resolution.synthetic_binding_type binding))
           [
             Function_type_resolution.variadic_argc bindings;
             Function_type_resolution.variadic_argv bindings;
           ])
       (Function_type_resolution.function_variadic_bindings header)

let function_metadata_is_owned symbol_table metadata =
  let declaration = metadata.declaration in
  let source =
    declaration |> Function_resolution.resolved_declaration_site
    |> Function_resolution.declaration_site_function
  in
  let header = Function_resolution.resolved_declaration_header declaration in
  Symbol_table.owns_symbol symbol_table
    (Function_resolution.resolved_declaration_identity_symbol declaration)
  && function_header_is_owned symbol_table source
  && (source == header || function_header_is_owned symbol_table header)

let validate_symbols symbol_table compilation_mode table_chain =
  let rec entries seen = function
    | [] -> Ok seen
    | entry :: rest ->
        let number = symbol_number entry.symbol in
        if not (Symbol_table.owns_symbol symbol_table entry.symbol) then
          Error
            (invalid_input
               ~origin:(Symbol.origin entry.symbol)
               "outer environment entry belongs to another symbol table")
        else if
          match entry.global_metadata with
          | None -> false
          | Some metadata ->
              not (global_metadata_is_owned symbol_table metadata)
        then
          Error
            (invalid_input
               ~origin:(Symbol.origin entry.symbol)
               "outer global metadata belongs to another symbol table")
        else if
          match entry.function_metadata with
          | None -> false
          | Some metadata ->
              not (function_metadata_is_owned symbol_table metadata)
        then
          Error
            (invalid_input
               ~origin:(Symbol.origin entry.symbol)
               "outer function metadata belongs to another symbol table")
        else if
          match entry.function_metadata with
          | None -> false
          | Some metadata ->
              Function_record_classification.compilation_mode metadata.records
              <> compilation_mode
        then
          Error
            (invalid_input
               ~origin:(Symbol.origin entry.symbol)
               "outer function metadata uses another compilation mode")
        else if Int_set.mem number seen then
          Error
            (invalid_input
               ~origin:(Symbol.origin entry.symbol)
               "outer environment chain repeats a semantic symbol")
        else entries (Int_set.add number seen) rest
  in
  let rec loop_tables seen = function
    | [] -> Ok ()
    | table :: rest -> (
        match entries seen table.entries with
        | Error _ as error -> error
        | Ok seen -> loop_tables seen rest)
  in
  loop_tables Int_set.empty table_chain

let create ~table:symbol_table ~compilation_mode tables =
  match validate_table_indexes tables with
  | Error _ as error -> error
  | Ok () -> (
      match validate_roles compilation_mode tables with
      | Error _ as error -> error
      | Ok () -> (
          match validate_symbols symbol_table compilation_mode tables with
          | Error _ as error -> error
          | Ok () ->
              Ok
                {
                  symbol_table;
                  compilation_mode;
                  tables;
                  function_versions = [];
                }))

let with_function_versions environment ~table metadata =
  let existing =
    List.filter
      (fun binding -> binding.table == table)
      environment.function_versions
  in
  let same_declaration left right = left.declaration == right.declaration in
  let seen =
    List.filter_map (fun binding -> binding.entry.function_metadata) existing
  in
  let primary_count = List.length table.entries in
  let existing_count = List.length existing in
  let classified_ancestor metadata current =
    let versions =
      Function_record_classification.declarations metadata.records
    in
    let rec follows current =
      (* A retained record can be the last of several declarations classified
         together. Exact membership proves ownership of that entire snapshot;
         declaration ancestry below still limits which version is selected. *)
      List.exists (fun original -> original == current) versions
      || Option.fold ~none:false ~some:follows
           (Function_record_classification
            .classified_declaration_retained_predecessor current)
    in
    follows current.classified_declaration
  in
  let rec extend next_index seen versions_rev = function
    | [] ->
        let versions = List.rev versions_rev in
        Ok
          ( {
              environment with
              function_versions = environment.function_versions @ versions;
            },
            List.map (fun binding -> binding.entry) versions )
    | metadata :: rest -> (
        let declaration = metadata.declaration in
        let symbol =
          Function_resolution.resolved_declaration_identity_symbol declaration
        in
        let current =
          List.find_map
            (fun entry ->
              if entry.symbol == symbol then entry.function_metadata else None)
            table.entries
        in
        if not (function_metadata_is_owned environment.symbol_table metadata)
        then
          Error
            (invalid_input
               "historical function header belongs to another symbol table")
        else if
          Function_record_classification.compilation_mode metadata.records
          <> environment.compilation_mode
        then
          Error
            (invalid_input
               "historical function metadata uses another compilation mode")
        else if List.exists (same_declaration metadata) seen then
          Error (invalid_input "historical function declaration is repeated")
        else if
          not
            (Option.fold ~none:false
               ~some:(fun current ->
                 (current.declaration == declaration
                 || Function_resolution.is_joined_successor ~earlier:declaration
                      ~later:current.declaration)
                 && classified_ancestor metadata current)
               current)
        then
          Error
            (invalid_input
               "historical function declaration is not an ancestor of its \
                current record")
        else if next_index = max_int then
          Error
            (invalid_input "historical function entry index space is exhausted")
        else
          match
            make_function_entry ~entry_index:next_index
              ~function_metadata:metadata
          with
          | Error _ as error -> error
          | Ok entry ->
              extend (next_index + 1) (metadata :: seen)
                ({ table; entry } :: versions_rev)
                rest)
  in
  if not (List.exists (fun current -> current == table) environment.tables) then
    Error
      (invalid_input
         "historical function versions require an exact existing table")
  else if existing_count > max_int - primary_count then
    Error (invalid_input "historical function entry index space is exhausted")
  else extend (primary_count + existing_count) seen [] metadata

let compilation_mode environment = environment.compilation_mode
let tables environment = environment.tables
let owns_table environment table = environment.symbol_table == table
let table_kind table = table.table_kind
let table_index table = table.table_index
let table_entries table = table.entries
let entry_symbol entry = entry.symbol
let entry_record_kind entry = entry.record_kind
let entry_index entry = entry.entry_index
let entry_global_metadata entry = entry.global_metadata
let entry_function_metadata entry = entry.function_metadata
let function_declaration metadata = metadata.declaration
let function_classified_declaration metadata = metadata.classified_declaration
let global_type_reference metadata = metadata.type_reference
let global_declarator_kind metadata = metadata.declarator_kind
let global_array_rank metadata = metadata.array_rank
let binding_table binding = binding.table
let binding_entry binding = binding.entry

let binding_for_entry environment entry =
  match
    List.find_map
      (fun table ->
        if List.exists (fun owned -> owned == entry) table.entries then
          Some { table; entry }
        else None)
      environment.tables
  with
  | Some _ as binding -> binding
  | None ->
      List.find_opt
        (fun binding -> binding.entry == entry)
        environment.function_versions

let owns_binding environment binding =
  match binding_for_entry environment binding.entry with
  | Some original -> original.table == binding.table
  | None -> false

let find environment name =
  let rec find_table = function
    | [] -> None
    | table :: rest -> (
        match String_map.find_opt name table.by_name with
        | Some entry -> Some { table; entry }
        | None -> find_table rest)
  in
  find_table environment.tables

let find_record environment ~name ~record_kind =
  let rec find_entries = function
    | [] -> None
    | entry :: rest ->
        if
          entry.record_kind = record_kind
          && String.equal (Symbol.name entry.symbol) name
        then Some entry
        else find_entries rest
  in
  let rec find_table = function
    | [] -> None
    | table :: rest -> (
        match find_entries (List.rev table.entries) with
        | Some entry -> Some { table; entry }
        | None -> find_table rest)
  in
  find_table environment.tables
