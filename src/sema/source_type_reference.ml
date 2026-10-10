let origin (location : Frontend.Ast.location) =
  Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

type selected_pointer = {
  layers : Frontend.Ast.pointer_layer list;
  callback_metadata : bool;
}

type selected_aggregate = {
  selected_type_specifier : Frontend.Ast.type_specifier;
  selected_identifier : Frontend.Ast.identifier;
  selected_pointers : selected_pointer list;
  selected_entry : Frontend.Symbol_visibility.entry;
  selected_environment : Frontend.Symbol_visibility.Environment.t;
  selected_symbol : Symbol.t;
}

type selected_source =
  | Function_return of Frontend.Parser.function_publication
  | Function_parameter of Frontend.Parser.function_parameter_publication
  | Callback_return of Frontend.Parser.callback_signature_publication
  | Callback_parameter of Frontend.Parser.callback_parameter_publication
  | Function_local of Frontend.Parser.function_local_allocation
  | Global_type of Frontend.Parser.global_publication
  | Aggregate_member of Frontend.Parser.aggregate_phase
  | Aggregate_backing of Frontend.Parser.aggregate_publication

let same_physical_list left right =
  List.length left = List.length right && List.for_all2 ( == ) left right

let pointer_depth pointer_layers =
  let rec collect expected = function
    | [] when expected - 1 <= Type.max_pointer_depth -> Ok (expected - 1)
    | [] -> Error "semantic source type exceeds the native pointer depth"
    | (layer : Frontend.Ast.pointer_layer) :: rest ->
        if layer.depth <> expected || layer.spelling <> "*" then
          Error "semantic source type has inconsistent pointer children"
        else collect (expected + 1) rest
  in
  collect 1 pointer_layers

let builtin type_specifier pointer_layers =
  let ( let* ) = Result.bind in
  let* pointer_depth = pointer_depth pointer_layers in
  let* resolved_type =
    match type_specifier with
    | Frontend.Ast.Primitive_type_specifier primitive ->
        Type.make_primitive ~form:Type.Public_spelling
          ~primitive:primitive.primitive ~pointer_depth
    | Frontend.Ast.Internal_type_specifier internal ->
        Type.make_primitive ~form:Type.Internal_storage
          ~primitive:internal.primitive ~pointer_depth
    | Frontend.Ast.Named_type_specifier _ ->
        Error "source query type requires its selected aggregate metadata"
  in
  Type_reference.make
    ~spelling:(Frontend.Ast.type_specifier_spelling type_specifier)
    ~spelling_origin:
      (origin (Frontend.Ast.type_specifier_location type_specifier))
    ~pointer_origins:
      (List.map
         (fun (layer : Frontend.Ast.pointer_layer) -> origin layer.location)
         pointer_layers)
    ~resolved_type

let source_parts ?(require_current = true) source =
  let module Parser = Frontend.Parser in
  match source with
  | Aggregate_backing aggregate -> (
      if
        require_current
        && not (Parser.aggregate_publication_is_current aggregate)
      then Error "selected backing is outside its original class publication"
      else
        match aggregate.aggregate_backing with
        | Some backing ->
            Ok
              ( aggregate.aggregate_backing_selection,
                backing.backing_type_specifier,
                backing.backing_pointer_layers,
                aggregate.aggregate_environment,
                false )
        | None -> Error "selected backing has no original backing occurrence")
  | Aggregate_member phase -> (
      if require_current && not (Parser.aggregate_phase_is_current phase) then
        Error "selected member type is outside its original placement callback"
      else
        match phase.phase_step with
        | Parser.Aggregate_member_prepared member ->
            Ok
              ( member.member_selection,
                member.member_type,
                member.member_pointers,
                phase.phase_aggregate.aggregate_environment,
                Option.is_some member.member_callback )
        | _ -> Error "selected member type lacks its original member phase")
  | Function_return function_ ->
      if
        require_current
        && not (Frontend.Parser.function_publication_is_current function_)
      then
        Error "selected aggregate return type is outside its original callback"
      else
        Ok
          ( function_.function_return_selection,
            function_.function_header.type_specifier,
            function_.function_pointer_layers,
            function_.function_environment,
            false )
  | Function_parameter parameter ->
      if
        require_current
        && not (Frontend.Parser.function_parameter_is_current parameter)
      then
        Error
          "selected aggregate parameter type is outside its original callback"
      else
        Ok
          ( parameter.parameter_type_selection,
            parameter.parameter_type_specifier,
            parameter.parameter_pointer_layers,
            parameter.parameter_function.function_environment,
            Option.is_some parameter.parameter_function_pointer )
  | Callback_return callback ->
      if require_current && not (Parser.callback_signature_is_current callback)
      then Error "selected callback return is outside its original callback"
      else
        Ok
          ( callback.callback_return_selection,
            callback.callback_return_type_specifier,
            callback.callback_return_pointer_layers,
            Parser.context_environment callback.callback_command.command_context,
            true )
  | Callback_parameter parameter ->
      if require_current && not (Parser.callback_parameter_is_current parameter)
      then Error "selected callback parameter is outside its original callback"
      else
        Ok
          ( parameter.callback_parameter_type_selection,
            parameter.callback_parameter_type_specifier,
            parameter.callback_parameter_pointer_layers,
            Parser.context_environment
              parameter.callback_parameter_signature.callback_command
                .command_context,
            Option.is_some parameter.callback_parameter_function_pointer )
  | Function_local allocation -> (
      if
        require_current
        && not (Parser.function_local_allocation_is_current allocation)
      then Error "selected local type is outside its original callback"
      else
        match allocation.allocation_local.local_source with
        | Parser.Local_variable local ->
            Ok
              ( local.local_type_selection,
                local.local_type_specifier,
                local.local_pointer_layers,
                allocation.allocation_local.local_environment,
                Option.is_some local.local_function_pointer )
        | _ -> Error "selected local type lacks its original variable")
  | Global_type global ->
      if require_current && not (Parser.global_publication_is_current global)
      then Error "selected global type is outside its original callback"
      else
        Ok
          ( global.global_header.declaration_type_selection,
            global.global_header.type_specifier,
            global.global_pointer_layers,
            global.global_environment,
            Option.is_some global.global_function_pointer )

let source_type source =
  Result.map
    (fun (selection, type_, _, _, _) -> (type_, selection))
    (source_parts ~require_current:false source)

let select_aggregate ~table ~namespace ~source publication =
  let module Visibility = Frontend.Symbol_visibility in
  let module Collection = Declaration_collection in
  let ( let* ) = Result.bind in
  let* ( selection,
         owner_type_specifier,
         pointer_layers,
         environment,
         callback_metadata ) =
    source_parts source
  in
  let* selection =
    match selection with
    | Some selection -> Ok selection
    | None -> Error "named type lacks its original parser selection"
  in
  let type_specifier = selection.type_specifier in
  let identifier = selection.identifier in
  if
    (not (Collection.namespace_owns_table namespace table))
    || not (Collection.namespace_owns_publication namespace publication)
  then Error "selected aggregate type belongs to another namespace or table"
  else if Visibility.kind selection.entry <> Visibility.Class then
    Error "selected aggregate type did not select a class entry"
  else if selection.environment != environment then
    Error "selected aggregate type substituted its owning parser environment"
  else if
    selection.environment
    !=
    match Collection.publication_source_aggregate publication with
    | Some source -> source.aggregate_environment
    | None -> selection.environment
  then Error "selected aggregate type belongs to another parser environment"
  else if selection.type_specifier != owner_type_specifier then
    Error "selected aggregate type substituted its owning type occurrence"
  else
    let* _ = pointer_depth pointer_layers in
    match
      (type_specifier, Collection.publication_source_aggregate publication)
    with
    | Frontend.Ast.Named_type_specifier original, Some source
      when original == identifier
           && source.aggregate_entry == selection.entry
           && source.aggregate_name.spelling = identifier.spelling
           && Visibility.name selection.entry = identifier.spelling ->
        let* symbol =
          match Collection.publication_aggregate_identity publication with
          | Some symbol -> Ok symbol
          | None ->
              Error "selected aggregate publication has no canonical identity"
        in
        if not (Symbol_table.owns_symbol table symbol) then
          Error "selected aggregate identity belongs to another symbol table"
        else if
          not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Aggregate_type)
        then Error "selected aggregate identity is not an aggregate type"
        else if Symbol.name symbol <> identifier.spelling then
          Error
            "selected aggregate identity disagrees with the selected spelling"
        else if
          not
            (Symbol.Scope_id.equal (Symbol.scope_id symbol)
               (Symbol_table.scope_id
                  (Declaration_collection.namespace_scope namespace)))
        then Error "selected aggregate identity is outside the owning namespace"
        else
          Ok
            {
              selected_type_specifier = type_specifier;
              selected_identifier = identifier;
              selected_pointers =
                [ { layers = pointer_layers; callback_metadata } ];
              selected_entry = selection.entry;
              selected_environment = selection.environment;
              selected_symbol = symbol;
            }
    | Frontend.Ast.Named_type_specifier _, Some _ ->
        Error "selected aggregate receipt substituted its original named type"
    | _, Some _ -> Error "selected aggregate receipt does not name an aggregate"
    | _, None -> Error "selected aggregate identity has no source publication"

let validate_selected_aggregate ~table ~namespace proof =
  if not (Declaration_collection.namespace_owns_table namespace table) then
    Error "selected aggregate consumer belongs to another semantic table"
  else if not (Symbol_table.owns_symbol table proof.selected_symbol) then
    Error "selected aggregate proof belongs to another semantic table"
  else if
    not
      (Symbol.Scope_id.equal
         (Symbol.scope_id proof.selected_symbol)
         (Symbol_table.scope_id
            (Declaration_collection.namespace_scope namespace)))
  then Error "selected aggregate proof belongs to another semantic namespace"
  else if
    not
      (Symbol.equal_kind
         (Symbol.kind proof.selected_symbol)
         Symbol.Aggregate_type)
  then Error "selected aggregate proof no longer identifies an aggregate type"
  else Ok ()

let merge_selections left right =
  if
    left.selected_type_specifier != right.selected_type_specifier
    || left.selected_identifier != right.selected_identifier
    || left.selected_entry != right.selected_entry
    || left.selected_environment != right.selected_environment
    || left.selected_symbol != right.selected_symbol
  then
    Error "selected type receipts do not share their original class occurrence"
  else
    let pointers =
      List.fold_left
        (fun all pointer ->
          if
            List.exists
              (fun existing ->
                same_physical_list existing.layers pointer.layers
                && existing.callback_metadata = pointer.callback_metadata)
              all
          then all
          else pointer :: all)
        left.selected_pointers right.selected_pointers
    in
    if pointers == left.selected_pointers then Ok left
    else Ok { left with selected_pointers = pointers }

let selected_reference ?(allow_value = false) ~callback_metadata proof
    type_specifier pointer_layers =
  let ( let* ) = Result.bind in
  if type_specifier != proof.selected_type_specifier then
    Error "selected aggregate type substituted its original type occurrence"
  else if
    not
      (List.exists
         (fun pointer ->
           same_physical_list pointer_layers pointer.layers
           && ((not callback_metadata) || pointer.callback_metadata))
         proof.selected_pointers)
  then Error "selected aggregate type substituted its original pointer children"
  else
    match type_specifier with
    | Frontend.Ast.Named_type_specifier identifier
      when identifier == proof.selected_identifier ->
        let* depth = pointer_depth pointer_layers in
        if depth = 0 && not (callback_metadata || allow_value) then
          Error "selected aggregate values require separate layout admission"
        else
          let* resolved_type =
            Type.make_aggregate ~symbol:proof.selected_symbol
              ~pointer_depth:depth
          in
          Type_reference.make ~spelling:identifier.spelling
            ~spelling_origin:(origin identifier.location)
            ~pointer_origins:
              (List.map
                 (fun (layer : Frontend.Ast.pointer_layer) ->
                   origin layer.location)
                 pointer_layers)
            ~resolved_type
    | Frontend.Ast.Named_type_specifier _ ->
        Error "selected aggregate type substituted its original named child"
    | _ -> Error "selected aggregate proof cannot authorize a nonaggregate type"

let selected = selected_reference ~allow_value:false ~callback_metadata:false

let selected_callback_return =
  selected_reference ~allow_value:false ~callback_metadata:true

let selected_header_class =
  selected_reference ~allow_value:true ~callback_metadata:false

let selected_base_symbol proof = proof.selected_symbol

let callback_return ~table ~namespace ~selected_aggregate ~header type_specifier
    pointer_layers =
  let ( let* ) = Result.bind in
  let publication = header.Frontend.Parser.callback_signature_publication in
  if
    publication.callback_return_type_specifier != type_specifier
    || not
         (same_physical_list publication.callback_return_pointer_layers
            pointer_layers)
  then Error "callback return metadata substituted its original type children"
  else
    match type_specifier with
    | Frontend.Ast.Primitive_type_specifier _
    | Frontend.Ast.Internal_type_specifier _ ->
        builtin type_specifier pointer_layers
    | Frontend.Ast.Named_type_specifier _ -> (
        match selected_aggregate type_specifier with
        | None -> Error "callback return lacks its original selected class"
        | Some proof ->
            let* () = validate_selected_aggregate ~table ~namespace proof in
            selected_callback_return proof type_specifier pointer_layers)

let callback_storage ~header source =
  let ( let* ) = Result.bind in
  if header.Frontend.Parser.callback_pointer != source then
    Error
      "callback member storage lacks its original completed anonymous header"
  else
    let* depth = pointer_depth source.Frontend.Ast.indirection_layers in
    if depth = 0 then
      Error "callback member storage requires original indirection"
    else
      let* resolved_type =
        Type.make_primitive ~form:Type.Internal_storage
          ~primitive:Common.Primitive_type.I64 ~pointer_depth:depth
      in
      Type_reference.make ~spelling:"I64i"
        ~spelling_origin:(origin source.function_pointer_location)
        ~pointer_origins:
          (List.map
             (fun (p : Frontend.Ast.pointer_layer) -> origin p.location)
             source.indirection_layers)
        ~resolved_type
