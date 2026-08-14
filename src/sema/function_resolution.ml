type compilation_mode = Jit | Aot
type declaration_kind = Extern | Bound_extern | Import | Intern | Definition
type state = Unresolved_extern | Imported | Resolved

type declaration = {
  function_ : Function_type_resolution.resolved_function;
  source_kind : declaration_kind;
  kind : declaration_kind;
  compiler_option_mask : int64;
}

type declaration_site = {
  function_ : Function_type_resolution.resolved_function;
  source_kind : declaration_kind;
  kind : declaration_kind;
  compiler_option_mask : int64;
  state : state;
}

type identity = {
  symbol : Symbol.t;
  sites : declaration_site list;
  state : state;
  first_item_index : int;
}

type resolved_declaration = {
  site : declaration_site;
  identity_symbol : Symbol.t;
  replaced_header : declaration_site option;
}

type t = {
  compilation_mode : compilation_mode;
  identities : identity list;
  declarations : resolved_declaration list;
}

let compilation_mode (resolution : t) = resolution.compilation_mode
let identities (resolution : t) = resolution.identities
let declarations (resolution : t) = resolution.declarations
let identity_symbol (identity : identity) = identity.symbol
let identity_sites (identity : identity) = identity.sites
let identity_state (identity : identity) = identity.state
let identity_first_item_index (identity : identity) = identity.first_item_index
let declaration_site_function (site : declaration_site) = site.function_
let declaration_site_source_kind (site : declaration_site) = site.source_kind
let declaration_site_kind (site : declaration_site) = site.kind

let declaration_site_compiler_option_mask (site : declaration_site) =
  site.compiler_option_mask

let declaration_site_state (site : declaration_site) = site.state

let resolved_declaration_site (declaration : resolved_declaration) =
  declaration.site

let resolved_declaration_identity_symbol (declaration : resolved_declaration) =
  declaration.identity_symbol

let resolved_declaration_replaced_header (declaration : resolved_declaration) =
  declaration.replaced_header

let compilation_mode_name = function
  | Jit -> "jit"
  | Aot -> "aot"

let declaration_kind_name = function
  | Extern -> "extern"
  | Bound_extern -> "bound-extern"
  | Import -> "import"
  | Intern -> "intern"
  | Definition -> "definition"

let state_name = function
  | Unresolved_extern -> "unresolved-extern"
  | Imported -> "imported"
  | Resolved -> "resolved"

let state_after = function
  | Extern -> Unresolved_extern
  | Import -> Imported
  | Bound_extern | Intern | Definition -> Resolved

let effective_kind compiler_option_mask kind =
  if
    Compiler_option.is_enabled ~mask:compiler_option_mask
      Compiler_option.Externs_to_imports
  then
    match kind with
    | Extern | Bound_extern -> Import
    | Import | Intern | Definition -> kind
  else kind

let make_declaration_with_options ~compiler_option_mask ~function_ ~kind =
  let symbol = Function_type_resolution.function_symbol function_ in
  if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Function) then
    Error "semantic function identity requires a function symbol"
  else
    Ok
      {
        function_;
        source_kind = kind;
        kind = effective_kind compiler_option_mask kind;
        compiler_option_mask;
      }

let make_declaration ~function_ ~kind =
  make_declaration_with_options
    ~compiler_option_mask:Compiler_option.initial_mask ~function_ ~kind

module Int_set = Set.Make (Int)
module String_map = Map.Make (String)

let symbol_number symbol = Symbol.Id.to_int (Symbol.id symbol)
let scope_number scope = Symbol.Scope_id.to_int (Symbol_table.scope_id scope)

let validate_declaration ~table ~parent ~compilation_mode previous_item
    seen_symbols seen_scopes (declaration : declaration) =
  let function_ = declaration.function_ in
  let symbol = Function_type_resolution.function_symbol function_ in
  let scope = Function_type_resolution.function_scope function_ in
  let item_index = Function_type_resolution.function_item_index function_ in
  let symbol_number = symbol_number symbol in
  let scope_number = scope_number scope in
  if compilation_mode = Jit && declaration.kind = Import then
    Error "semantic function imports require AOT compilation mode"
  else if item_index <= previous_item then
    Error "semantic function identities must follow module source order"
  else if Int_set.mem symbol_number seen_symbols then
    Error "semantic function identity declaration symbol is repeated"
  else if Int_set.mem scope_number seen_scopes then
    Error "semantic function identity declaration scope is repeated"
  else if not (Symbol_table.owns_symbol table symbol) then
    Error "semantic function identity belongs to a different symbol table"
  else if not (Symbol_table.owns_scope table scope) then
    Error "semantic function identity scope belongs to a different symbol table"
  else if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Function) then
    Error "semantic function identity requires function symbols"
  else if Symbol_table.scope_kind scope <> Symbol_table.Function then
    Error "semantic function identity requires function scopes"
  else if
    not
      (Symbol.Scope_id.equal (Symbol.scope_id symbol)
         (Symbol_table.scope_id parent))
  then Error "semantic function identity does not belong to the module scope"
  else if
    match Symbol_table.parent scope with
    | Some scope ->
        not
          (Symbol.Scope_id.equal
             (Symbol_table.scope_id scope)
             (Symbol_table.scope_id parent))
    | None -> true
  then Error "semantic function identity scope does not belong to the module"
  else
    Ok
      ( item_index,
        Int_set.add symbol_number seen_symbols,
        Int_set.add scope_number seen_scopes )

let validate ~table ~parent ~compilation_mode declarations =
  if not (Symbol_table.owns_scope table parent) then
    Error
      "semantic function identity parent belongs to a different symbol table"
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error "semantic function identities require a module scope"
  else
    let rec check previous_item seen_symbols seen_scopes = function
      | [] -> Ok ()
      | declaration :: rest -> (
          match
            validate_declaration ~table ~parent ~compilation_mode previous_item
              seen_symbols seen_scopes declaration
          with
          | Error _ as error -> error
          | Ok (item_index, seen_symbols, seen_scopes) ->
              check item_index seen_symbols seen_scopes rest)
    in
    check (-1) Int_set.empty Int_set.empty declarations

type pending_identity = {
  symbol : Symbol.t;
  sites_rev : declaration_site list;
  state : state;
  first_item_index : int;
}

let may_join compilation_mode state =
  match compilation_mode with
  | Jit -> state = Unresolved_extern
  | Aot -> state <> Imported

let resolve_validated compilation_mode (declarations : declaration list) =
  let declaration_count = List.length declarations in
  let pending = Array.make declaration_count None in
  let declaration_identity = Array.make declaration_count (-1) in
  let declaration_replaced_header = Array.make declaration_count None in
  let sites = Array.make declaration_count None in
  let latest_by_name = ref String_map.empty in
  let identity_count = ref 0 in
  let site_of (declaration : declaration) =
    {
      function_ = declaration.function_;
      source_kind = declaration.source_kind;
      kind = declaration.kind;
      compiler_option_mask = declaration.compiler_option_mask;
      state = state_after declaration.kind;
    }
  in
  let add_identity site =
    let identity_index = !identity_count in
    let symbol = Function_type_resolution.function_symbol site.function_ in
    let first_item_index =
      Function_type_resolution.function_item_index site.function_
    in
    pending.(identity_index) <-
      Some
        { symbol; sites_rev = [ site ]; state = site.state; first_item_index };
    identity_count := identity_index + 1;
    latest_by_name :=
      String_map.add (Symbol.name symbol) identity_index !latest_by_name;
    identity_index
  in
  let join_or_add site =
    let symbol = Function_type_resolution.function_symbol site.function_ in
    match String_map.find_opt (Symbol.name symbol) !latest_by_name with
    | None -> (add_identity site, None)
    | Some identity_index -> (
        match pending.(identity_index) with
        | Some identity when may_join compilation_mode identity.state ->
            let replaced_header = List.hd identity.sites_rev in
            pending.(identity_index) <-
              Some
                {
                  identity with
                  sites_rev = site :: identity.sites_rev;
                  state = site.state;
                };
            (identity_index, Some replaced_header)
        | Some _ -> (add_identity site, None)
        | None -> assert false)
  in
  List.iteri
    (fun declaration_index declaration ->
      let site = site_of declaration in
      let identity_index, replaced_header = join_or_add site in
      sites.(declaration_index) <- Some site;
      declaration_identity.(declaration_index) <- identity_index;
      declaration_replaced_header.(declaration_index) <- replaced_header)
    declarations;
  let identity_at index =
    match pending.(index) with
    | Some identity -> identity
    | None -> assert false
  in
  let identities =
    List.init !identity_count (fun index ->
        let pending = identity_at index in
        {
          symbol = pending.symbol;
          sites = List.rev pending.sites_rev;
          state = pending.state;
          first_item_index = pending.first_item_index;
        })
  in
  let identities_by_index = Array.of_list identities in
  let declarations =
    List.init declaration_count (fun declaration_index ->
        let identity =
          identities_by_index.(declaration_identity.(declaration_index))
        in
        let site =
          match sites.(declaration_index) with
          | Some site -> site
          | None -> assert false
        in
        {
          site;
          identity_symbol = identity.symbol;
          replaced_header = declaration_replaced_header.(declaration_index);
        })
  in
  { compilation_mode; identities; declarations }

let resolve ~table ~parent ~compilation_mode declarations =
  Result.map
    (fun () -> resolve_validated compilation_mode declarations)
    (validate ~table ~parent ~compilation_mode declarations)
