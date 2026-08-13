type scope_kind =
  | Task
  | Module
  | Function
  | Block
  | Aggregate
  | Assembler_block

type owner = unit ref

type scope = {
  id : Symbol.Scope_id.t;
  kind : scope_kind;
  name : string option;
  parent : scope option;
  owner : owner;
  entries_by_name : (string, Symbol.t list) Hashtbl.t;
}

type t = {
  owner : owner;
  root : scope;
  mutable scopes_rev : scope list;
  mutable symbols_rev : Symbol.t list;
  symbols_by_id : (int, Symbol.t) Hashtbl.t;
  mutable next_scope_id : int;
  mutable next_symbol_id : int;
}

let scope_kind_name = function
  | Task -> "task"
  | Module -> "module"
  | Function -> "function"
  | Block -> "block"
  | Aggregate -> "aggregate"
  | Assembler_block -> "assembler-block"

let valid_optional_name = function
  | None -> true
  | Some name -> not (String.equal name "")

let create ?root_name () =
  if not (valid_optional_name root_name) then
    invalid_arg "semantic root scope name cannot be empty";
  let owner = ref () in
  let root =
    {
      id = Symbol.Scope_id.of_int 0;
      kind = Task;
      name = root_name;
      parent = None;
      owner;
      entries_by_name = Hashtbl.create 128;
    }
  in
  {
    owner;
    root;
    scopes_rev = [ root ];
    symbols_rev = [];
    symbols_by_id = Hashtbl.create 128;
    next_scope_id = 1;
    next_symbol_id = 0;
  }

let root table = table.root
let scope_id scope = scope.id
let scope_kind scope = scope.kind
let scope_name scope = scope.name
let parent scope = scope.parent
let owns (table : t) (scope : scope) = table.owner == scope.owner
let owns_scope = owns

let owns_symbol table symbol =
  match
    Hashtbl.find_opt table.symbols_by_id (Symbol.Id.to_int (Symbol.id symbol))
  with
  | Some owned -> owned == symbol
  | None -> false

let create_scope (table : t) ~(parent : scope) ~kind ?name () =
  if not (owns table parent) then
    Error "semantic scope belongs to a different symbol table"
  else if not (valid_optional_name name) then
    Error "semantic scope name cannot be empty"
  else if table.next_scope_id = max_int then
    Error "semantic scope identity space is exhausted"
  else
    let scope =
      {
        id = Symbol.Scope_id.of_int table.next_scope_id;
        kind;
        name;
        parent = Some parent;
        owner = table.owner;
        entries_by_name = Hashtbl.create 32;
      }
    in
    table.next_scope_id <- table.next_scope_id + 1;
    table.scopes_rev <- scope :: table.scopes_rev;
    Ok scope

let add (table : t) ~(scope : scope) ~name ~kind ~origin =
  if not (owns table scope) then
    Error "semantic scope belongs to a different symbol table"
  else if String.equal name "" then Error "semantic symbol name cannot be empty"
  else if table.next_symbol_id = max_int then
    Error "semantic symbol identity space is exhausted"
  else
    try
      let symbol =
        Symbol.create
          ~id:(Symbol.Id.of_int table.next_symbol_id)
          ~scope_id:scope.id ~name ~kind ~origin
      in
      table.next_symbol_id <- table.next_symbol_id + 1;
      let existing =
        Option.value (Hashtbl.find_opt scope.entries_by_name name) ~default:[]
      in
      Hashtbl.replace scope.entries_by_name name (symbol :: existing);
      table.symbols_rev <- symbol :: table.symbols_rev;
      Hashtbl.add table.symbols_by_id
        (Symbol.Id.to_int (Symbol.id symbol))
        symbol;
      Ok symbol
    with Invalid_argument message -> Error message

type local_search = Found of Symbol.t | Remaining of int

let matches kinds symbol =
  List.exists (fun kind -> Symbol.equal_kind kind (Symbol.kind symbol)) kinds

let search_entries entries ~kinds ~instance =
  let rec search remaining = function
    | [] -> Remaining remaining
    | symbol :: rest ->
        if matches kinds symbol then
          if remaining = 1 then Found symbol else search (remaining - 1) rest
        else search remaining rest
  in
  search instance entries

let validate_lookup table scope name kinds instance =
  if not (owns table scope) then
    Error "semantic scope belongs to a different symbol table"
  else if String.equal name "" then Error "semantic lookup name cannot be empty"
  else if kinds = [] then Error "semantic lookup kind set cannot be empty"
  else if instance < 1 then Error "semantic lookup instance must be positive"
  else Ok ()

let local_entries scope name =
  Option.value (Hashtbl.find_opt scope.entries_by_name name) ~default:[]

let lookup_local (table : t) ~(scope : scope) ~name ~kinds ?(instance = 1) () =
  match validate_lookup table scope name kinds instance with
  | Error _ as error -> error
  | Ok () -> (
      match search_entries (local_entries scope name) ~kinds ~instance with
      | Found symbol -> Ok (Some symbol)
      | Remaining _ -> Ok None)

let lookup (table : t) ~(scope : scope) ~name ~kinds ?(instance = 1) () =
  match validate_lookup table scope name kinds instance with
  | Error _ as error -> error
  | Ok () ->
      let rec search scope instance =
        match search_entries (local_entries scope name) ~kinds ~instance with
        | Found symbol -> Some symbol
        | Remaining remaining -> (
            match scope.parent with
            | None -> None
            | Some parent -> search parent remaining)
      in
      Ok (search scope instance)

let all_scopes table = List.rev table.scopes_rev
let all_symbols table = List.rev table.symbols_rev

let span_position sources span =
  match Common.Source_manager.find sources span.Common.Span.source with
  | None ->
      Printf.sprintf "source-%d:%d..%d"
        (Common.Source_id.to_int span.source)
        span.start span.stop
  | Some source -> (
      match
        ( Common.Source_file.position source span.start,
          Common.Source_file.position source span.stop )
      with
      | Ok start, Ok stop ->
          Printf.sprintf "%s:%d:%d..%d:%d"
            (Common.Source_file.display_path source)
            start.line start.column stop.line stop.column
      | _ ->
          Printf.sprintf "%s:%d..%d"
            (Common.Source_file.display_path source)
            span.start span.stop)

let origin_text sources = function
  | Symbol.Pinned_source { path; line } -> Printf.sprintf "%s:%d" path line
  | Symbol.Source_location source -> span_position sources source.span
  | Symbol.Synthesized description -> Printf.sprintf "<%s>" description

let print_source_origin sources buffer source =
  List.iteri
    (fun index span ->
      Printf.bprintf buffer "  source-segment %d location=%s\n" index
        (span_position sources span))
    source.Symbol.source_segments;
  Option.iter
    (fun span ->
      Printf.bprintf buffer "  generated-from=%s\n" (span_position sources span))
    source.generated_from;
  Option.iter
    (fun span ->
      Printf.bprintf buffer "  defined-at=%s\n" (span_position sources span))
    source.defined_at

let optional_name_text = function
  | None -> "none"
  | Some name -> Printf.sprintf "%S" name

let optional_parent_text = function
  | None -> "none"
  | Some parent -> string_of_int (Symbol.Scope_id.to_int parent.id)

let human sources table =
  let buffer = Buffer.create 512 in
  Buffer.add_string buffer "holyc-semantic-symbol-table-v1\n";
  Printf.bprintf buffer "reference_commit=%s\n" Symbol.reference_commit;
  List.iter
    (fun scope ->
      Printf.bprintf buffer "scope %d kind=%s name=%s parent=%s\n"
        (Symbol.Scope_id.to_int scope.id)
        (scope_kind_name scope.kind)
        (optional_name_text scope.name)
        (optional_parent_text scope.parent))
    (all_scopes table);
  List.iter
    (fun symbol ->
      Printf.bprintf buffer "symbol %d scope=%d name=%S kind=%s origin=%s\n"
        (Symbol.Id.to_int (Symbol.id symbol))
        (Symbol.Scope_id.to_int (Symbol.scope_id symbol))
        (Symbol.name symbol)
        (Symbol.kind_name (Symbol.kind symbol))
        (origin_text sources (Symbol.origin symbol));
      match Symbol.origin symbol with
      | Symbol.Source_location source ->
          print_source_origin sources buffer source
      | Symbol.Pinned_source _ | Symbol.Synthesized _ -> ())
    (all_symbols table);
  Buffer.contents buffer

let span_to_yojson sources span =
  let fields =
    [
      ("source_id", `Int (Common.Source_id.to_int span.Common.Span.source));
      ("start", `Int span.start);
      ("stop", `Int span.stop);
    ]
  in
  match Common.Source_manager.find sources span.source with
  | None -> `Assoc fields
  | Some source ->
      let fields =
        ("path", `String (Common.Source_file.display_path source)) :: fields
      in
      let position_fields prefix offset =
        match Common.Source_file.position source offset with
        | Error _ -> []
        | Ok position ->
            [
              (prefix ^ "line", `Int position.line);
              (prefix ^ "column", `Int position.column);
            ]
      in
      `Assoc
        (fields
        @ position_fields "" span.start
        @ position_fields "end_" span.stop)

let optional_span_to_yojson sources = function
  | None -> `Null
  | Some span -> span_to_yojson sources span

let origin_to_yojson sources = function
  | Symbol.Pinned_source { path; line } ->
      `Assoc
        [
          ("kind", `String "pinned-source");
          ("path", `String path);
          ("line", `Int line);
        ]
  | Symbol.Source_location source ->
      `Assoc
        [
          ("kind", `String "source-location");
          ("span", span_to_yojson sources source.span);
          ( "source_segments",
            `List (List.map (span_to_yojson sources) source.source_segments) );
          ( "generated_from",
            optional_span_to_yojson sources source.generated_from );
          ("defined_at", optional_span_to_yojson sources source.defined_at);
        ]
  | Symbol.Synthesized description ->
      `Assoc
        [
          ("kind", `String "synthesized"); ("description", `String description);
        ]

let scope_to_yojson scope =
  `Assoc
    [
      ("id", `Int (Symbol.Scope_id.to_int scope.id));
      ("kind", `String (scope_kind_name scope.kind));
      ( "name",
        match scope.name with
        | None -> `Null
        | Some name -> `String name );
      ( "parent",
        match scope.parent with
        | None -> `Null
        | Some parent -> `Int (Symbol.Scope_id.to_int parent.id) );
    ]

let symbol_to_yojson sources symbol =
  `Assoc
    [
      ("id", `Int (Symbol.Id.to_int (Symbol.id symbol)));
      ("scope", `Int (Symbol.Scope_id.to_int (Symbol.scope_id symbol)));
      ("name", `String (Symbol.name symbol));
      ("kind", `String (Symbol.kind_name (Symbol.kind symbol)));
      ("origin", origin_to_yojson sources (Symbol.origin symbol));
    ]

let to_yojson sources table =
  `Assoc
    [
      ("schema", `String "holyc-semantic-symbol-table-v1");
      ("reference_commit", `String Symbol.reference_commit);
      ("scopes", `List (List.map scope_to_yojson (all_scopes table)));
      ( "symbols",
        `List (List.map (symbol_to_yojson sources) (all_symbols table)) );
    ]

let json sources table = to_yojson sources table |> Yojson.Safe.pretty_to_string
