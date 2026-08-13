type aggregate_kind = Class | Union
type declaration_kind = Forward | Definition

type declaration = {
  symbol : Symbol.t;
  declaration_kind : declaration_kind;
  aggregate_kind : aggregate_kind;
  item_index : int;
}

type declaration_site = declaration

type identity = {
  symbol : Symbol.t;
  forward : declaration_site option;
  definition : declaration_site option;
  aggregate_kind : aggregate_kind;
  first_item_index : int;
}

type resolved_declaration = {
  site : declaration_site;
  identity_symbol : Symbol.t;
}

type t = {
  identities : identity list;
  declarations : resolved_declaration list;
}

let identities resolution = resolution.identities
let declarations resolution = resolution.declarations
let identity_symbol (identity : identity) = identity.symbol
let identity_forward (identity : identity) = identity.forward
let identity_definition (identity : identity) = identity.definition
let identity_kind (identity : identity) = identity.aggregate_kind
let identity_first_item_index (identity : identity) = identity.first_item_index
let declaration_site_symbol (site : declaration_site) = site.symbol
let declaration_site_kind (site : declaration_site) = site.declaration_kind

let declaration_site_aggregate_kind (site : declaration_site) =
  site.aggregate_kind

let declaration_site_item_index (site : declaration_site) = site.item_index

let resolved_declaration_site (declaration : resolved_declaration) =
  declaration.site

let resolved_declaration_identity_symbol (declaration : resolved_declaration) =
  declaration.identity_symbol

let aggregate_kind_name = function
  | Class -> "class"
  | Union -> "union"

let declaration_kind_name = function
  | Forward -> "forward"
  | Definition -> "definition"

let make_declaration ~symbol ~declaration_kind ~aggregate_kind ~item_index =
  if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Aggregate_type) then
    Error "semantic aggregate resolution requires an aggregate-type symbol"
  else if item_index < 0 then
    Error "semantic aggregate declaration item index cannot be negative"
  else Ok { symbol; declaration_kind; aggregate_kind; item_index }

module Int_set = Set.Make (Int)

let validate_declaration table parent previous_item_index seen
    (declaration : declaration) =
  let symbol_id = Symbol.Id.to_int (Symbol.id declaration.symbol) in
  if not (Symbol_table.owns_symbol table declaration.symbol) then
    Error "semantic aggregate declaration belongs to a different symbol table"
  else if
    not
      (Symbol.equal_kind (Symbol.kind declaration.symbol) Symbol.Aggregate_type)
  then Error "semantic aggregate resolution requires aggregate-type symbols"
  else if
    not
      (Symbol.Scope_id.equal
         (Symbol.scope_id declaration.symbol)
         (Symbol_table.scope_id parent))
  then
    Error "semantic aggregate declaration does not belong to the module scope"
  else if declaration.item_index <= previous_item_index then
    Error "semantic aggregate declarations must be in increasing item order"
  else if Int_set.mem symbol_id seen then
    Error "semantic aggregate declaration symbols cannot repeat"
  else Ok (declaration.item_index, Int_set.add symbol_id seen)

let validate table parent declarations =
  if not (Symbol_table.owns_scope table parent) then
    Error "semantic aggregate parent belongs to a different symbol table"
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error "semantic aggregate parent must be a module scope"
  else
    let rec check previous_item_index seen = function
      | [] -> Ok ()
      | declaration :: rest -> (
          match
            validate_declaration table parent previous_item_index seen
              declaration
          with
          | Error _ as error -> error
          | Ok (item_index, seen) -> check item_index seen rest)
    in
    check (-1) Int_set.empty declarations

type pending_identity = {
  forward : declaration_site option;
  definition : declaration_site option;
  first_item_index : int;
}

let resolve_validated declarations =
  let declaration_count = List.length declarations in
  let pending = Array.make declaration_count None in
  let declaration_identity = Array.make declaration_count (-1) in
  let latest_by_name = Hashtbl.create declaration_count in
  let identity_count = ref 0 in
  let add_identity (declaration : declaration) =
    let identity_index = !identity_count in
    let identity =
      match declaration.declaration_kind with
      | Forward ->
          {
            forward = Some declaration;
            definition = None;
            first_item_index = declaration.item_index;
          }
      | Definition ->
          {
            forward = None;
            definition = Some declaration;
            first_item_index = declaration.item_index;
          }
    in
    pending.(identity_index) <- Some identity;
    identity_count := identity_index + 1;
    Hashtbl.replace latest_by_name
      (Symbol.name declaration.symbol)
      identity_index;
    identity_index
  in
  let complete_or_add (declaration : declaration) =
    let name = Symbol.name declaration.symbol in
    match Hashtbl.find_opt latest_by_name name with
    | None -> add_identity declaration
    | Some identity_index -> (
        match pending.(identity_index) with
        | Some ({ forward = Some _; definition = None; _ } as identity) ->
            pending.(identity_index) <-
              Some { identity with definition = Some declaration };
            identity_index
        | Some _ -> add_identity declaration
        | None -> assert false)
  in
  List.iteri
    (fun declaration_index declaration ->
      let identity_index =
        match declaration.declaration_kind with
        | Forward -> add_identity declaration
        | Definition -> complete_or_add declaration
      in
      declaration_identity.(declaration_index) <- identity_index)
    declarations;
  let identity_at index =
    match pending.(index) with
    | Some identity -> identity
    | None -> assert false
  in
  let resolved_identities =
    List.init !identity_count (fun index ->
        let pending = identity_at index in
        match (pending.forward, pending.definition) with
        | forward, Some definition ->
            {
              symbol = definition.symbol;
              forward;
              definition = Some definition;
              aggregate_kind = definition.aggregate_kind;
              first_item_index = pending.first_item_index;
            }
        | Some forward, None ->
            {
              symbol = forward.symbol;
              forward = Some forward;
              definition = None;
              aggregate_kind = forward.aggregate_kind;
              first_item_index = pending.first_item_index;
            }
        | None, None -> assert false)
  in
  let identities_by_index = Array.of_list resolved_identities in
  let resolved_declarations =
    List.mapi
      (fun declaration_index site ->
        let identity =
          identities_by_index.(declaration_identity.(declaration_index))
        in
        { site; identity_symbol = identity.symbol })
      declarations
  in
  { identities = resolved_identities; declarations = resolved_declarations }

let resolve ~table ~parent declarations =
  Result.map
    (fun () -> resolve_validated declarations)
    (validate table parent declarations)
