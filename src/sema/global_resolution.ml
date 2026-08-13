type compilation_mode = Jit | Aot
type storage = Code_heap | Data_heap
type binding_kind = Extern_binding | Import_binding | Intern_binding
type binding_target_kind = No_target | Symbol_target | Expression_target

type binding_target =
  | No_binding_target
  | Symbol_binding_target of { name : string; origin : Symbol.origin }
  | Expression_binding_target of Symbol.origin

type source_binding = {
  kind : binding_kind;
  spelling : string;
  origin : Symbol.origin;
  target : binding_target;
}

type declaration_kind =
  | Definition
  | Extern
  | Alternate_extern
  | Import
  | Alternate_import
  | Intern

type state =
  | Defined
  | Unresolved_extern
  | Declared_extern
  | Bound_extern
  | Imported

type declaration = {
  global : Global_type_resolution.global;
  storage : storage;
  binding : source_binding option;
  kind : declaration_kind;
}

type global_record = {
  declaration : declaration;
  state : state;
  alias_target : Symbol.t option;
}

type t = { compilation_mode : compilation_mode; records : global_record list }

let no_binding_target = No_binding_target

let make_symbol_binding_target ~name ~origin =
  if String.equal name "" then
    Error "semantic global binding target cannot have an empty name"
  else Ok (Symbol_binding_target { name; origin })

let make_expression_binding_target ~origin = Expression_binding_target origin

let make_source_binding ~kind ~spelling ~origin ~target =
  let valid =
    match (kind, spelling, target) with
    | Extern_binding, "extern", No_binding_target
    | Extern_binding, "_extern", Symbol_binding_target _
    | Import_binding, "import", No_binding_target
    | Import_binding, "_import", Symbol_binding_target _
    | Intern_binding, "_intern", Expression_binding_target _ -> true
    | Extern_binding, _, _ | Import_binding, _, _ | Intern_binding, _, _ ->
        false
  in
  if valid then Ok { kind; spelling; origin; target }
  else Error "semantic global binding has an inconsistent source shape"

let declaration_kind_of_binding = function
  | None -> Definition
  | Some { kind = Extern_binding; target = No_binding_target; _ } -> Extern
  | Some { kind = Extern_binding; target = Symbol_binding_target _; _ } ->
      Alternate_extern
  | Some { kind = Import_binding; target = No_binding_target; _ } -> Import
  | Some { kind = Import_binding; target = Symbol_binding_target _; _ } ->
      Alternate_import
  | Some { kind = Intern_binding; target = Expression_binding_target _; _ } ->
      Intern
  | Some
      {
        kind = Extern_binding | Import_binding;
        target = Expression_binding_target _;
        _;
      }
  | Some
      {
        kind = Intern_binding;
        target = No_binding_target | Symbol_binding_target _;
        _;
      } -> assert false

let make_declaration ~global ~storage ?binding () =
  let symbol = Global_type_resolution.global_symbol global in
  if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Global_variable) then
    Error "semantic global record requires a global-variable symbol"
  else
    Ok { global; storage; binding; kind = declaration_kind_of_binding binding }

let compilation_mode resolution = resolution.compilation_mode
let records resolution = resolution.records
let declaration_global (declaration : declaration) = declaration.global
let declaration_storage (declaration : declaration) = declaration.storage
let declaration_binding (declaration : declaration) = declaration.binding
let declaration_kind (declaration : declaration) = declaration.kind
let global_record_declaration (record : global_record) = record.declaration
let global_record_global (record : global_record) = record.declaration.global

let global_record_symbol (record : global_record) =
  Global_type_resolution.global_symbol record.declaration.global

let global_record_kind (record : global_record) = record.declaration.kind
let global_record_storage (record : global_record) = record.declaration.storage
let global_record_state (record : global_record) = record.state
let global_record_alias_target (record : global_record) = record.alias_target
let source_binding_kind (binding : source_binding) = binding.kind
let source_binding_spelling (binding : source_binding) = binding.spelling
let source_binding_origin (binding : source_binding) = binding.origin
let source_binding_target (binding : source_binding) = binding.target

let binding_target_kind = function
  | No_binding_target -> No_target
  | Symbol_binding_target _ -> Symbol_target
  | Expression_binding_target _ -> Expression_target

let binding_target_name = function
  | Symbol_binding_target target -> Some target.name
  | No_binding_target | Expression_binding_target _ -> None

let binding_target_origin = function
  | No_binding_target -> None
  | Symbol_binding_target target -> Some target.origin
  | Expression_binding_target origin -> Some origin

let compilation_mode_name = function
  | Jit -> "jit"
  | Aot -> "aot"

let storage_name = function
  | Code_heap -> "code-heap"
  | Data_heap -> "data-heap"

let binding_kind_name = function
  | Extern_binding -> "extern"
  | Import_binding -> "import"
  | Intern_binding -> "intern"

let binding_target_kind_name = function
  | No_target -> "none"
  | Symbol_target -> "symbol"
  | Expression_target -> "expression"

let declaration_kind_name = function
  | Definition -> "definition"
  | Extern -> "extern"
  | Alternate_extern -> "alternate-extern"
  | Import -> "import"
  | Alternate_import -> "alternate-import"
  | Intern -> "intern"

let state_name = function
  | Defined -> "defined"
  | Unresolved_extern -> "unresolved-extern"
  | Declared_extern -> "declared-extern"
  | Bound_extern -> "bound-extern"
  | Imported -> "imported"

let state_for compilation_mode kind =
  match (compilation_mode, kind) with
  | _, (Definition | Intern) -> Defined
  | Jit, Extern -> Unresolved_extern
  | Aot, Extern -> Declared_extern
  | _, Alternate_extern -> Bound_extern
  | _, (Import | Alternate_import) -> Imported

let is_definition = function
  | Definition | Intern -> true
  | _ -> false

let is_import = function
  | Import | Alternate_import -> true
  | _ -> false

let validate ~table ~parent ~compilation_mode declarations =
  if not (Symbol_table.owns_scope table parent) then
    Error "semantic global record parent belongs to a different symbol table"
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error "semantic global records require a module scope"
  else if
    compilation_mode = Jit
    && List.exists (fun declaration -> is_import declaration.kind) declarations
  then Error "semantic global imports require AOT compilation mode"
  else
    declarations
    |> List.map (fun declaration -> declaration.global)
    |> Global_type_resolution.resolve ~table ~parent
    |> Result.map (fun _ -> ())

module String_map = Map.Make (String)

type pending_record = {
  declaration : declaration;
  alias_target : Symbol.t option;
}

let resolve_validated compilation_mode declarations =
  let count = List.length declarations in
  let pending = Array.make count None in
  let latest_by_name = ref String_map.empty in
  let rec add index = function
    | [] ->
        let records =
          List.init count (fun record_index ->
              match pending.(record_index) with
              | None -> assert false
              | Some record ->
                  {
                    declaration = record.declaration;
                    state = state_for compilation_mode record.declaration.kind;
                    alias_target = record.alias_target;
                  })
        in
        Ok { compilation_mode; records }
    | declaration :: rest -> (
        let symbol = Global_type_resolution.global_symbol declaration.global in
        let name = Symbol.name symbol in
        let previous = String_map.find_opt name !latest_by_name in
        let aliases_previous =
          match (compilation_mode, declaration.kind, declaration.storage) with
          | _, kind, _ when not (is_definition kind) -> Ok false
          | Aot, _, Data_heap when Option.is_some previous ->
              Error
                "AOT data-heap global definitions cannot follow a same-name \
                 global; the pinned compiler reports this case as \
                 unimplemented"
          | Aot, _, Data_heap -> Ok false
          | Aot, _, Code_heap -> Ok (Option.is_some previous)
          | Jit, _, _ ->
              Ok
                (match previous with
                | None -> false
                | Some previous_index -> (
                    match pending.(previous_index) with
                    | Some previous -> previous.declaration.kind = Extern
                    | None -> assert false))
        in
        match aliases_previous with
        | Error _ as error -> error
        | Ok aliases_previous ->
            (if aliases_previous then
               match previous with
               | None -> assert false
               | Some previous_index -> (
                   match pending.(previous_index) with
                   | None -> assert false
                   | Some previous_record ->
                       pending.(previous_index) <-
                         Some
                           { previous_record with alias_target = Some symbol }));
            pending.(index) <- Some { declaration; alias_target = None };
            latest_by_name := String_map.add name index !latest_by_name;
            add (index + 1) rest)
  in
  add 0 declarations

let resolve ~table ~parent ~compilation_mode declarations =
  match validate ~table ~parent ~compilation_mode declarations with
  | Error _ as error -> error
  | Ok () -> resolve_validated compilation_mode declarations
