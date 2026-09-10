type compilation_mode = Jit | Aot
type declaration_kind = Extern | Bound_extern | Import | Intern | Definition
type state = Unresolved_extern | Imported | Resolved

type declaration_site = {
  function_ : Function_type_resolution.resolved_function;
  source_kind : declaration_kind;
  kind : declaration_kind;
  compiler_option_mask : int64;
  state : state;
  header_source : Compiler_record.declared_function option;
  pending_header : bool;
}

type identity = {
  symbol : Symbol.t;
  sites : declaration_site list;
  state : state;
  first_item_index : int;
}

type resolved_declaration = {
  compilation_mode : compilation_mode;
  source_history : Function_type_resolution.resolved_function list;
  site : declaration_site;
  header : Function_type_resolution.resolved_function;
  identity_symbol : Symbol.t;
  replaced_header : declaration_site option;
  retained_predecessor : resolved_declaration option;
  joined_predecessor : resolved_declaration option;
  completion_source : resolved_declaration option;
  mutable completed : bool;
}

type declaration = {
  function_ : Function_type_resolution.resolved_function;
  source_kind : declaration_kind;
  kind : declaration_kind;
  compiler_option_mask : int64;
  header_source : Compiler_record.declared_function option;
  pending_header : bool;
  completion_predecessor : resolved_declaration option;
  completion_current : resolved_declaration option;
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
let declaration_site_is_pending (site : declaration_site) = site.pending_header

let declaration_site_header_source (site : declaration_site) =
  site.header_source

let declaration_site_pending_source (site : declaration_site) =
  if site.pending_header then site.header_source else None

let resolved_declaration_site (declaration : resolved_declaration) =
  declaration.site

let resolved_declaration_header declaration = declaration.header

let resolved_declaration_completion_source declaration =
  declaration.completion_source

let resolved_declaration_compilation_mode (declaration : resolved_declaration) =
  declaration.compilation_mode

let resolved_declaration_identity_symbol (declaration : resolved_declaration) =
  declaration.identity_symbol

let resolved_declaration_replaced_header (declaration : resolved_declaration) =
  declaration.replaced_header

let resolved_declaration_retained_predecessor declaration =
  declaration.retained_predecessor

let rec is_joined_successor ~earlier ~later =
  match later.joined_predecessor with
  | None -> false
  | Some predecessor ->
      predecessor == earlier || is_joined_successor ~earlier ~later:predecessor

let rec find_pending_source ~current ~function_ =
  if current.site.pending_header && current.site.function_ == function_ then
    Some current
  else
    Option.bind current.joined_predecessor (fun current ->
        find_pending_source ~current ~function_)

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
        header_source = None;
        pending_header = false;
        completion_predecessor = None;
        completion_current = None;
      }

let make_declaration ~function_ ~kind =
  make_declaration_with_options
    ~compiler_option_mask:Compiler_option.initial_mask ~function_ ~kind

let source_origin (location : Frontend.Ast.location) =
  Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let source_kind (source : Frontend.Parser.completed_function_header) =
  match source.function_publication.function_header.binding with
  | None -> Ok Definition
  | Some binding -> (
      match (binding.kind, binding.spelling, binding.target) with
      | Frontend.Ast.Extern, "extern", Frontend.Ast.No_binding_target ->
          Ok Extern
      | Frontend.Ast.Extern, "_extern", Frontend.Ast.Symbol_binding_target _ ->
          Ok Bound_extern
      | Frontend.Ast.Import, "import", Frontend.Ast.No_binding_target
      | Frontend.Ast.Import, "_import", Frontend.Ast.Symbol_binding_target _ ->
          Ok Import
      | Frontend.Ast.Intern, "_intern", Frontend.Ast.Expression_binding_target _
        -> Ok Intern
      | _ -> Error "pending function header has an inconsistent source binding")

let source_registers_match sources requests =
  List.length sources = List.length requests
  && List.for_all2
       (fun (source : Frontend.Ast.register_qualifier) request ->
         source_origin source.location = Register_request.origin request
         && source.spelling = Register_request.spelling request
         && (match (source.kind, Register_request.kind request) with
           | Frontend.Ast.Reg, Register_request.Allocate
           | Frontend.Ast.Noreg, Register_request.Disable -> true
           | _ -> false)
         && (match (source.position, Register_request.position request) with
           | Frontend.Ast.Before_type, Register_request.Before_type
           | Frontend.Ast.After_type, Register_request.After_type -> true
           | _ -> false)
         &&
         match
           (source.explicit_register, Register_request.explicit_register request)
         with
         | None, None -> true
         | Some source, Some request ->
             source.spelling
             = Register_request.explicit_register_spelling request
             && source_origin source.location
                = Register_request.explicit_register_origin request
         | _ -> false)
       sources requests

let rec source_signature_matches ~opening ~parameters ~variadic ~closing
    signature =
  let module H = Function_type_resolution in
  H.signature_opening_origin signature = source_origin opening
  && H.signature_closing_origin signature = Option.map source_origin closing
  && H.signature_variadic_origin signature
     = Option.map
         (fun (marker : Frontend.Ast.variadic_marker) ->
           source_origin marker.location)
         variadic
  && source_registers_match
       (Option.fold ~none:[]
          ~some:(fun (marker : Frontend.Ast.variadic_marker) ->
            marker.register_qualifiers)
          variadic)
       (H.signature_variadic_register_requests signature)
  && List.length parameters = List.length (H.signature_parameters signature)
  && List.for_all2
       (fun (source : Frontend.Ast.function_parameter) parameter ->
         Option.fold ~none:false ~some:(( == ) source)
           (H.parameter_source parameter)
         && source_registers_match source.register_qualifiers
              (H.parameter_register_requests parameter)
         && H.parameter_delimiter_origin parameter
            = Option.map
                (fun (delimiter : Frontend.Ast.declaration_delimiter) ->
                  source_origin delimiter.location)
                source.delimiter
         &&
         match
           (source.function_pointer, H.parameter_declarator_kind parameter)
         with
         | None, H.Object -> true
         | Some source, H.Function_pointer pointer ->
             H.function_pointer_origin pointer
             = source_origin source.function_pointer_location
             && H.function_pointer_opening_origin pointer
                = source_origin source.declarator_opening_parenthesis
             && H.function_pointer_closing_origin pointer
                = source_origin source.declarator_closing_parenthesis
             && H.function_pointer_indirection_origins pointer
                = List.map
                    (fun (layer : Frontend.Ast.pointer_layer) ->
                      source_origin layer.location)
                    source.indirection_layers
             && source_signature_matches
                  ~opening:source.signature_opening_parenthesis
                  ~parameters:source.signature_parameters
                  ~variadic:source.signature_variadic
                  ~closing:source.signature_closing_parenthesis
                  (H.function_pointer_signature pointer)
         | _ -> false)
       parameters
       (H.signature_parameters signature)

let validate_header_source ~table ~namespace ~source ~function_ =
  let module H = Function_type_resolution in
  let header = Compiler_record.declared_function_source source in
  let publication = header.function_publication in
  let parent = Declaration_collection.namespace_scope namespace in
  let scope = H.function_scope function_ in
  let return_type = H.function_return_type function_ in
  if
    (not (Compiler_record.declared_function_owns_table source table))
    || (not (Compiler_record.declared_function_owns_namespace source namespace))
    || (not (Symbol_table.owns_scope table parent))
    || (not (Symbol_table.owns_scope table scope))
    || H.function_symbol function_
       != Compiler_record.declared_function_symbol source
    || (not
          (Option.fold ~none:false ~some:(( == ) header)
             (H.function_completed_header function_)))
    || (not (Symbol_table.owns_symbol table (H.function_symbol function_)))
    || (not
          (Option.fold ~none:false ~some:(( == ) parent)
             (Symbol_table.parent scope)))
    || Symbol_table.scope_kind scope <> Symbol_table.Function
    || H.function_item_index function_ <> 0
  then
    Error
      "pending function header requires its original symbol, namespace and \
       scope"
  else if
    Type_reference.spelling return_type
    <> Frontend.Ast.type_specifier_spelling
         publication.function_header.type_specifier
    || Type_reference.spelling_origin return_type
       <> source_origin
            (Frontend.Ast.type_specifier_location
               publication.function_header.type_specifier)
    || Type_reference.pointer_origins return_type
       <> List.map
            (fun (layer : Frontend.Ast.pointer_layer) ->
              source_origin layer.location)
            publication.function_pointer_layers
    || not
         (source_signature_matches
            ~opening:publication.function_opening_parenthesis
            ~parameters:header.parameters ~variadic:header.variadic
            ~closing:header.closing_parenthesis
            (H.function_signature function_))
  then
    Error
      "pending function header requires its original checked source children"
  else Ok ()

let make_pending_declaration ~table ~namespace ~compiler_option_mask ~source
    ~function_ =
  let ( let* ) = Result.bind in
  let* () = validate_header_source ~table ~namespace ~source ~function_ in
  if
    Int64.logand compiler_option_mask (Int64.lognot Compiler_option.known_mask)
    <> 0L
  then Error "pending function header has unknown compiler options"
  else
    let* kind = source_kind (Compiler_record.declared_function_source source) in
    let* declaration =
      make_declaration_with_options ~compiler_option_mask ~function_ ~kind
    in
    Ok { declaration with header_source = Some source; pending_header = true }

let make_completion_declaration_against ~table ~namespace
    ~(pending : resolved_declaration) ~(current : resolved_declaration)
    ~function_ =
  let ( let* ) = Result.bind in
  match pending.site.header_source with
  | Some source when pending.site.pending_header && not pending.completed ->
      if function_ != pending.site.function_ then
        Error "function completion requires its exact retained typed header"
      else if
        current.compilation_mode <> pending.compilation_mode
        || current.identity_symbol != pending.identity_symbol
        || not
             (current == pending
             || is_joined_successor ~earlier:pending ~later:current)
      then Error "function completion requires its exact current record lineage"
      else
        let* () = validate_header_source ~table ~namespace ~source ~function_ in
        Ok
          {
            function_;
            source_kind = pending.site.source_kind;
            kind = pending.site.kind;
            compiler_option_mask = pending.site.compiler_option_mask;
            header_source = Some source;
            pending_header = false;
            completion_predecessor = Some pending;
            completion_current = Some current;
          }
  | _ -> Error "function completion requires an uncompleted pending declaration"

let make_completion_declaration ~table ~namespace ~pending ~function_ =
  make_completion_declaration_against ~table ~namespace ~pending
    ~current:pending ~function_

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
  source_history : Function_type_resolution.resolved_function list;
  symbol : Symbol.t;
  sites_rev : declaration_site list;
  state : state;
  first_item_index : int;
  retained_predecessor : resolved_declaration option;
}

let may_join compilation_mode state =
  match compilation_mode with
  | Jit -> state = Unresolved_extern
  | Aot -> state <> Imported

let resolve_validated ~previous compilation_mode
    (declarations : declaration list) =
  let declaration_count = List.length declarations in
  let pending = Array.make declaration_count None in
  let declaration_identity = Array.make declaration_count (-1) in
  let declaration_replaced_header = Array.make declaration_count None in
  let declaration_history = Array.make declaration_count [] in
  let sites = Array.make declaration_count None in
  let latest_by_name = ref String_map.empty in
  let identity_count = ref 0 in
  let site_of (declaration : declaration) =
    {
      function_ = declaration.function_;
      source_kind = declaration.source_kind;
      kind = declaration.kind;
      compiler_option_mask = declaration.compiler_option_mask;
      state =
        (if declaration.pending_header then Unresolved_extern
         else state_after declaration.kind);
      header_source = declaration.header_source;
      pending_header = declaration.pending_header;
    }
  in
  let add_identity ?predecessor ?(completion = false) ?(update_name = true)
      (site : declaration_site) =
    let identity_index = !identity_count in
    let symbol =
      match predecessor with
      | Some prior -> prior.identity_symbol
      | None -> Function_type_resolution.function_symbol site.function_
    in
    let first_item_index =
      Function_type_resolution.function_item_index site.function_
    in
    pending.(identity_index) <-
      Some
        {
          symbol;
          sites_rev = [ site ];
          state = site.state;
          first_item_index;
          retained_predecessor = predecessor;
          source_history =
            (match predecessor with
            | Some prior when completion -> prior.source_history
            | Some prior -> site.function_ :: prior.source_history
            | None -> [ site.function_ ]);
        };
    identity_count := identity_index + 1;
    if update_name then
      latest_by_name :=
        String_map.add (Symbol.name symbol) identity_index !latest_by_name;
    identity_index
  in
  let join_or_add (site : declaration_site) =
    let symbol = Function_type_resolution.function_symbol site.function_ in
    match String_map.find_opt (Symbol.name symbol) !latest_by_name with
    | None -> (
        match
          List.find_opt
            (fun prior ->
              Symbol.name prior.identity_symbol = Symbol.name symbol)
            previous
        with
        | Some prior when may_join compilation_mode prior.site.state ->
            (add_identity ~predecessor:prior site, Some prior.site)
        | _ -> (add_identity site, None))
    | Some identity_index -> (
        match pending.(identity_index) with
        | Some identity when may_join compilation_mode identity.state ->
            let replaced_header = List.hd identity.sites_rev in
            pending.(identity_index) <-
              Some
                {
                  identity with
                  sites_rev = site :: identity.sites_rev;
                  source_history = site.function_ :: identity.source_history;
                  state = site.state;
                };
            (identity_index, Some replaced_header)
        | Some _ -> (add_identity site, None)
        | None -> assert false)
  in
  List.iteri
    (fun declaration_index declaration ->
      let site = site_of declaration in
      let identity_index, replaced_header =
        match declaration.completion_current with
        | Some predecessor ->
            ( add_identity ~predecessor ~completion:true
                ~update_name:(List.memq predecessor previous)
                site,
              None )
        | None -> join_or_add site
      in
      sites.(declaration_index) <- Some site;
      declaration_identity.(declaration_index) <- identity_index;
      declaration_replaced_header.(declaration_index) <- replaced_header;
      declaration_history.(declaration_index) <-
        (match pending.(identity_index) with
        | Some identity -> identity.source_history
        | None -> assert false))
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
  let latest_declaration_by_identity = Array.make !identity_count None in
  let declarations =
    List.init declaration_count (fun declaration_index ->
        let identity_index = declaration_identity.(declaration_index) in
        let identity = identities_by_index.(identity_index) in
        let site =
          match sites.(declaration_index) with
          | Some site -> site
          | None -> assert false
        in
        let retained_predecessor =
          (identity_at identity_index).retained_predecessor
        in
        let joined_predecessor =
          match latest_declaration_by_identity.(identity_index) with
          | Some _ as predecessor -> predecessor
          | None -> retained_predecessor
        in
        let declaration =
          let source = List.nth declarations declaration_index in
          {
            site;
            header =
              (match source.completion_current with
              | Some current -> current.header
              | None -> site.function_);
            compilation_mode;
            source_history = declaration_history.(declaration_index);
            identity_symbol = identity.symbol;
            replaced_header = declaration_replaced_header.(declaration_index);
            retained_predecessor;
            joined_predecessor;
            completion_source = source.completion_predecessor;
            completed = false;
          }
        in
        latest_declaration_by_identity.(identity_index) <- Some declaration;
        declaration)
  in
  { compilation_mode; identities; declarations }

let resolve ?(previous = []) ?(record_heads = []) ~table ~parent
    ~compilation_mode declarations =
  let ( let* ) = Result.bind in
  let* () = validate ~table ~parent ~compilation_mode declarations in
  let exact_completion (declaration : declaration) prior =
    match
      (declaration.completion_predecessor, declaration.completion_current)
    with
    | Some pending, Some current ->
        current == prior
        && declaration.function_ == pending.site.function_
        && (current == pending
           || is_joined_successor ~earlier:pending ~later:current)
    | _ -> false
  in
  let rec validate_completions earlier_names = function
    | [] -> Ok ()
    | (declaration : declaration) :: rest ->
        let name =
          Symbol.name
            (Function_type_resolution.function_symbol declaration.function_)
        in
        let* () =
          match
            (declaration.completion_predecessor, declaration.completion_current)
          with
          | None, None -> Ok ()
          | Some pending, Some current ->
              if
                pending.completed
                || (not pending.site.pending_header)
                || pending.compilation_mode <> compilation_mode
                || declaration.function_ != pending.site.function_
                || List.mem name earlier_names
                || (not (exact_completion declaration current))
                || not (List.exists (( == ) current) (previous @ record_heads))
              then
                Error
                  "function completion requires its exact current pending \
                   predecessor lineage"
              else Ok ()
          | _ -> Error "function completion has incomplete source authority"
        in
        validate_completions (name :: earlier_names) rest
  in
  let* () = validate_completions [] declarations in
  let rec validate_previous names = function
    | [] -> Ok ()
    | prior :: rest ->
        let symbol = prior.identity_symbol in
        let source =
          Function_type_resolution.function_symbol prior.site.function_
        in
        let repeated_source =
          List.exists
            (fun (declaration : declaration) ->
              let current = declaration.function_ in
              (not (exact_completion declaration prior))
              && List.exists
                   (fun previous ->
                     Function_type_resolution.function_symbol current
                     == Function_type_resolution.function_symbol previous
                     || Function_type_resolution.function_scope current
                        == Function_type_resolution.function_scope previous)
                   prior.source_history)
            declarations
        in
        if
          compilation_mode <> Jit
          && not
               (List.exists
                  (fun declaration -> exact_completion declaration prior)
                  declarations)
          || prior.compilation_mode <> compilation_mode
          || repeated_source
          || (not (Symbol_table.owns_symbol table symbol))
          || (not (Symbol_table.owns_symbol table source))
          || (not
                (Symbol.Scope_id.equal (Symbol.scope_id symbol)
                   (Symbol_table.scope_id parent)))
          || (not
                (Symbol.Scope_id.equal (Symbol.scope_id source)
                   (Symbol_table.scope_id parent)))
          || Symbol.name symbol <> Symbol.name source
          || List.mem (Symbol.name symbol) names
        then
          Error
            "retained function predecessor has another namespace, mode or \
             duplicate name"
        else validate_previous (Symbol.name symbol :: names) rest
  in
  let* () = validate_previous [] previous in
  let rec validate_heads identities = function
    | [] -> Ok ()
    | head :: rest ->
        let symbol = head.identity_symbol in
        if
          List.exists (fun prior -> prior.identity_symbol == symbol) identities
          || List.exists
               (fun prior -> prior.identity_symbol == symbol && prior != head)
               previous
        then
          Error "function record heads repeat or substitute a current identity"
        else
          let* () = validate_previous [] [ head ] in
          validate_heads (head :: identities) rest
  in
  let* () = validate_heads [] record_heads in
  let resolution = resolve_validated ~previous compilation_mode declarations in
  List.iter
    (fun (declaration : declaration) ->
      Option.iter
        (fun pending -> pending.completed <- true)
        declaration.completion_predecessor)
    declarations;
  Ok resolution

let complete_pending ~table ~namespace ~pending ~function_ =
  Result.bind
    (make_completion_declaration ~table ~namespace ~pending ~function_)
    (fun declaration ->
      resolve ~previous:[ pending ] ~table
        ~parent:(Declaration_collection.namespace_scope namespace)
        ~compilation_mode:pending.compilation_mode [ declaration ])
