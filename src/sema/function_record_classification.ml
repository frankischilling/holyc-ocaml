module Shared_flag = Function_flag.Shared
module Stored_flag = Function_flag.Stored
module Hash_flag = Global_record_flag.Hash_flag

type call_access =
  | Internal_operation
  | Direct_executable_call
  | Jit_extern_address_slot_call
  | Aot_import_call
  | Aot_extern_call

type hash_value_access =
  | Hash_returns_function_record
  | Hash_returns_executable_address

type runtime_lookup =
  | Runtime_lookup_visible
  | Runtime_lookup_omits_extern
  | Runtime_lookup_omits_internal
  | Runtime_lookup_omits_extern_and_internal

type map_visibility =
  | Map_visible
  | Map_omitted_import
  | Map_omitted_private
  | Map_omitted_import_and_private

type aot_resolution =
  | No_aot_resolution
  | Aot_resolve_references
  | Aot_resolution_shadowed_by_import

type aot_publication =
  | No_aot_publication
  | Aot_import_record
  | Aot_export_record

type declaration_state = {
  staging_mask : int64;
  compiler_option_mask : int64;
  import_name : string option;
}

type record = {
  shared_flag_mask : int64;
  stored_flag_mask : int64;
  hash_flag_mask : int64;
  import_name : string option;
  call_access : call_access;
  hash_value_access : hash_value_access;
  runtime_lookup : runtime_lookup;
  map_visibility : map_visibility;
  aot_resolution : aot_resolution;
  aot_publication : aot_publication;
}

type classified_declaration = {
  source : Function_resolution.resolved_declaration;
  state : declaration_state;
  record : record;
}

type classified_identity = {
  source : Function_resolution.identity;
  record : record;
}

type t = {
  compilation_mode : Function_resolution.compilation_mode;
  declarations : classified_declaration list;
  identities : classified_identity list;
}

let make_declaration_state ~staging_mask ~compiler_option_mask ?import_name () =
  { staging_mask; compiler_option_mask; import_name }

let compilation_mode classification = classification.compilation_mode
let declarations classification = classification.declarations
let identities classification = classification.identities

let declaration_state_staging_mask (state : declaration_state) =
  state.staging_mask

let declaration_state_compiler_option_mask (state : declaration_state) =
  state.compiler_option_mask

let declaration_state_import_name (state : declaration_state) =
  state.import_name

let classified_declaration_source (declaration : classified_declaration) =
  declaration.source

let classified_declaration_state (declaration : classified_declaration) =
  declaration.state

let classified_declaration_record (declaration : classified_declaration) =
  declaration.record

let classified_identity_source (identity : classified_identity) =
  identity.source

let classified_identity_record (identity : classified_identity) =
  identity.record

let shared_flag_mask record = record.shared_flag_mask
let stored_flag_mask record = record.stored_flag_mask

let function_flag_mask record =
  Int64.logor record.shared_flag_mask record.stored_flag_mask

let hash_type_mask _ = Function_flag.function_type.type_mask
let hash_flag_mask record = record.hash_flag_mask

let combined_hash_mask record =
  Int64.logor (hash_type_mask record) record.hash_flag_mask

let import_name (record : record) = record.import_name
let call_access record = record.call_access
let hash_value_access record = record.hash_value_access
let runtime_lookup record = record.runtime_lookup
let map_visibility record = record.map_visibility
let aot_resolution record = record.aot_resolution
let aot_publication record = record.aot_publication

let is_extern record =
  Shared_flag.is_set ~mask:record.shared_flag_mask Shared_flag.Extern

let is_internal record =
  Stored_flag.is_set ~mask:record.stored_flag_mask Stored_flag.Internal

let is_public record =
  Hash_flag.is_set ~mask:record.hash_flag_mask Hash_flag.Public

let is_private record =
  Hash_flag.is_set ~mask:record.hash_flag_mask Hash_flag.Private

let call_access_name = function
  | Internal_operation -> "internal-operation"
  | Direct_executable_call -> "direct-executable-call"
  | Jit_extern_address_slot_call -> "jit-extern-address-slot-call"
  | Aot_import_call -> "aot-import-call"
  | Aot_extern_call -> "aot-extern-call"

let hash_value_access_name = function
  | Hash_returns_function_record -> "function-record"
  | Hash_returns_executable_address -> "executable-address"

let runtime_lookup_name = function
  | Runtime_lookup_visible -> "visible"
  | Runtime_lookup_omits_extern -> "omitted-extern"
  | Runtime_lookup_omits_internal -> "omitted-internal"
  | Runtime_lookup_omits_extern_and_internal -> "omitted-extern-and-internal"

let map_visibility_name = function
  | Map_visible -> "visible"
  | Map_omitted_import -> "omitted-import"
  | Map_omitted_private -> "omitted-private"
  | Map_omitted_import_and_private -> "omitted-import-and-private"

let aot_resolution_name = function
  | No_aot_resolution -> "none"
  | Aot_resolve_references -> "resolve"
  | Aot_resolution_shadowed_by_import -> "shadowed-by-import"

let aot_publication_name = function
  | No_aot_publication -> "none"
  | Aot_import_record -> "import"
  | Aot_export_record -> "export"

let set_hash condition flag mask =
  if condition then Hash_flag.set ~mask flag else Hash_flag.clear ~mask flag

let add_hash condition flag mask =
  if condition then Hash_flag.set ~mask flag else mask

let add_stored condition flag mask =
  if condition then Stored_flag.set ~mask flag else mask

let public_requested state = Function_flag.public_requested state.staging_mask

let private_requested state =
  Compiler_option.is_enabled ~mask:state.compiler_option_mask
    Compiler_option.Keep_private

let signature_shape declaration =
  let site = Function_resolution.resolved_declaration_site declaration in
  let function_ = Function_resolution.declaration_site_function site in
  let signature = Function_type_resolution.function_signature function_ in
  let argument_count =
    Function_type_resolution.signature_parameters signature
    |> List.length |> Int64.of_int
  in
  let variadic =
    Function_type_resolution.function_variadic_bindings function_
    |> Option.is_some
  in
  (argument_count, variadic)

let new_record state =
  {
    shared_flag_mask = Shared_flag.set ~mask:0L Shared_flag.Extern;
    stored_flag_mask = Function_flag.stored_mask_of_staging state.staging_mask;
    hash_flag_mask = 0L;
    import_name = None;
    call_access = Direct_executable_call;
    hash_value_access = Hash_returns_function_record;
    runtime_lookup = Runtime_lookup_omits_extern;
    map_visibility = Map_visible;
    aot_resolution = No_aot_resolution;
    aot_publication = No_aot_publication;
  }

let apply_header declaration (state : declaration_state) (record : record) =
  let argument_count, variadic = signature_shape declaration in
  let stored_flag_mask =
    record.stored_flag_mask
    |> add_stored variadic Stored_flag.Variadic
    |> add_stored
         (Function_flag.derives_ret1 ~argument_count ~variadic)
         Stored_flag.Ret1
  in
  let hash_flag_mask =
    record.hash_flag_mask
    |> set_hash (public_requested state) Hash_flag.Public
    |> add_hash (private_requested state) Hash_flag.Private
  in
  { record with stored_flag_mask; hash_flag_mask }

let apply_binding compilation_mode declaration (state : declaration_state)
    (record : record) =
  let site = Function_resolution.resolved_declaration_site declaration in
  match Function_resolution.declaration_site_kind site with
  | Function_resolution.Extern -> record
  | Function_resolution.Bound_extern ->
      {
        record with
        shared_flag_mask =
          Shared_flag.clear ~mask:record.shared_flag_mask Shared_flag.Extern;
        stored_flag_mask =
          Stored_flag.set ~mask:record.stored_flag_mask
            Stored_flag.Underscore_extern;
        hash_flag_mask =
          add_hash
            (compilation_mode = Function_resolution.Aot)
            Hash_flag.Resolve record.hash_flag_mask;
      }
  | Function_resolution.Import ->
      {
        record with
        hash_flag_mask =
          Hash_flag.set ~mask:record.hash_flag_mask Hash_flag.Import;
        import_name = state.import_name;
      }
  | Function_resolution.Intern ->
      {
        record with
        shared_flag_mask =
          Shared_flag.clear ~mask:record.shared_flag_mask Shared_flag.Extern;
        stored_flag_mask =
          Stored_flag.set ~mask:record.stored_flag_mask Stored_flag.Internal;
      }
  | Function_resolution.Definition ->
      {
        record with
        shared_flag_mask =
          Shared_flag.clear ~mask:record.shared_flag_mask Shared_flag.Extern;
        hash_flag_mask =
          record.hash_flag_mask
          |> add_hash
               (compilation_mode = Function_resolution.Aot)
               Hash_flag.Export
          |> add_hash
               (compilation_mode = Function_resolution.Aot)
               Hash_flag.Resolve;
      }

let call_access_for compilation_mode record =
  if is_internal record then Internal_operation
  else if is_extern record then
    match compilation_mode with
    | Function_resolution.Jit -> Jit_extern_address_slot_call
    | Function_resolution.Aot ->
        if Hash_flag.is_set ~mask:record.hash_flag_mask Hash_flag.Import then
          Aot_import_call
        else Aot_extern_call
  else Direct_executable_call

let hash_value_access_for record =
  if is_extern record then Hash_returns_function_record
  else Hash_returns_executable_address

let runtime_lookup_for record =
  match (is_extern record, is_internal record) with
  | false, false -> Runtime_lookup_visible
  | true, false -> Runtime_lookup_omits_extern
  | false, true -> Runtime_lookup_omits_internal
  | true, true -> Runtime_lookup_omits_extern_and_internal

let map_visibility_for record =
  match
    ( Hash_flag.is_set ~mask:record.hash_flag_mask Hash_flag.Import,
      Hash_flag.is_set ~mask:record.hash_flag_mask Hash_flag.Private )
  with
  | false, false -> Map_visible
  | true, false -> Map_omitted_import
  | false, true -> Map_omitted_private
  | true, true -> Map_omitted_import_and_private

let aot_resolution_for compilation_mode record =
  if compilation_mode = Function_resolution.Jit then No_aot_resolution
  else
    let resolve =
      Hash_flag.is_set ~mask:record.hash_flag_mask Hash_flag.Resolve
    in
    let import =
      Hash_flag.is_set ~mask:record.hash_flag_mask Hash_flag.Import
    in
    if resolve && import then Aot_resolution_shadowed_by_import
    else if resolve then Aot_resolve_references
    else No_aot_resolution

let aot_publication_for compilation_mode record =
  if compilation_mode = Function_resolution.Jit then No_aot_publication
  else if Hash_flag.is_set ~mask:record.hash_flag_mask Hash_flag.Import then
    Aot_import_record
  else if Hash_flag.is_set ~mask:record.hash_flag_mask Hash_flag.Export then
    Aot_export_record
  else No_aot_publication

let classify_consumers compilation_mode record =
  {
    record with
    call_access = call_access_for compilation_mode record;
    hash_value_access = hash_value_access_for record;
    runtime_lookup = runtime_lookup_for record;
    map_visibility = map_visibility_for record;
    aot_resolution = aot_resolution_for compilation_mode record;
    aot_publication = aot_publication_for compilation_mode record;
  }

let known_staging_mask =
  Function_flag.Staging.all
  |> List.fold_left
       (fun mask flag -> Int64.logor mask (Function_flag.Staging.to_mask flag))
       0L

let validate_state declaration (state : declaration_state) =
  let unknown_staging =
    Int64.logand state.staging_mask (Int64.lognot known_staging_mask)
  in
  let site = Function_resolution.resolved_declaration_site declaration in
  let kind = Function_resolution.declaration_site_kind site in
  if not (Int64.equal unknown_staging 0L) then
    Error "function record classification received unknown parser staging bits"
  else
    match (kind, state.import_name) with
    | Function_resolution.Import, Some name when not (String.equal name "") ->
        Ok ()
    | Function_resolution.Import, _ ->
        Error "function record classification requires an import name"
    | ( ( Function_resolution.Extern
        | Function_resolution.Bound_extern
        | Function_resolution.Intern
        | Function_resolution.Definition ),
        None ) -> Ok ()
    | _, Some _ ->
        Error
          "function record classification accepts import names only on imports"

module Int_map = Map.Make (Int)

let symbol_number symbol = Symbol.Id.to_int (Symbol.id symbol)

let classify resolution states =
  let sources = Function_resolution.declarations resolution in
  if List.length sources <> List.length states then
    Error
      "function record classification requires one source state per resolved \
       declaration"
  else
    let compilation_mode = Function_resolution.compilation_mode resolution in
    let rec replay records declarations_rev sources states =
      match (sources, states) with
      | [], [] -> Ok (records, List.rev declarations_rev)
      | source :: source_rest, state :: state_rest -> (
          match validate_state source state with
          | Error _ as error -> error
          | Ok () ->
              let symbol =
                Function_resolution.resolved_declaration_identity_symbol source
              in
              let key = symbol_number symbol in
              let record =
                match Int_map.find_opt key records with
                | Some record -> record
                | None -> new_record state
              in
              let record =
                record |> apply_header source state
                |> apply_binding compilation_mode source state
                |> classify_consumers compilation_mode
              in
              replay
                (Int_map.add key record records)
                ({ source; state; record } :: declarations_rev)
                source_rest state_rest)
      | [], _ :: _ | _ :: _, [] -> assert false
    in
    match replay Int_map.empty [] sources states with
    | Error _ as error -> error
    | Ok (records, declarations) ->
        let rec final_identities found = function
          | [] -> Ok (List.rev found)
          | source :: rest -> (
              let key =
                Function_resolution.identity_symbol source |> symbol_number
              in
              match Int_map.find_opt key records with
              | Some record ->
                  final_identities ({ source; record } :: found) rest
              | None ->
                  Error
                    "function record classification is missing an identity \
                     state")
        in
        Result.map
          (fun identities -> { compilation_mode; declarations; identities })
          (final_identities [] (Function_resolution.identities resolution))
