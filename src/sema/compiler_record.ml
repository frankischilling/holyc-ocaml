module Visibility = Frontend.Symbol_visibility
module Parser = Frontend.Parser
module Ast = Frontend.Ast

type runtime_dimension_proposal = {
  proposal_namespace : Declaration_collection.namespace;
  proposal_source : Parser.array_dimension_preparation;
  proposal_count : int64;
  proposal_work : int;
}

let runtime_dimension_namespace value = value.proposal_namespace
let runtime_dimension_source value = value.proposal_source
let runtime_dimension_count value = value.proposal_count
let runtime_dimension_work value = value.proposal_work

type aggregate_stamp = { mutable current_stamp : unit ref }

type t = {
  table : Symbol_table.t;
  entry : Visibility.entry;
  symbol : Symbol.t;
  primitive : Primitive_type.t option;
  byte_size : int64;
  internal : bool;
  runtime_dimensions : runtime_dimension_proposal list;
  aggregate_stamp : (aggregate_stamp * unit ref) option;
}

type sizeof_owner =
  | Hash_record of t
  | Local_record of {
      table : Symbol_table.t;
      source : Parser.local_publication;
      byte_size : int64;
      internal : bool;
      runtime_dimensions : runtime_dimension_proposal list;
    }

type sizeof_read = { owner : sizeof_owner; root : Parser.query_root }

type query_role = Query_source.role =
  | Sizeof_root
  | Offset_root
  | Defined_operand

type query_read = {
  table : Symbol_table.t;
  receipt : Parser.completed_query;
  role : query_role;
  name : string;
  origin : Symbol.origin;
  sizeof_read : sizeof_read option;
}

type dimension_preparation = {
  dimension_table : Symbol_table.t;
  dimension_namespace : Declaration_collection.namespace;
  preparation : Parser.array_dimension_preparation;
  queries : query_read list;
  count : int64;
  work : int;
  runtime_dependencies : runtime_dimension_proposal list;
}

type declared_dimension = {
  prepared : dimension_preparation;
  completed : Parser.completed_array_dimension;
}

type extent_evaluation =
  | Prepared_extent of declared_dimension
  | Legacy_extent of query_read list option

type global_dimension_extent = {
  extent_table : Symbol_table.t;
  extent_record : Global_resolution.global_record;
  extent_dimension : Global_type_resolution.array_dimension;
  extent_evaluation : extent_evaluation;
  extent_count : int64;
}

type global_extent = {
  global_extent_table : Symbol_table.t;
  global_extent_record : Global_resolution.global_record;
  global_extent_dimensions : global_dimension_extent list;
  global_extent_count : int64;
}

type declared_global = {
  declared_table : Symbol_table.t;
  declared_namespace : Declaration_collection.namespace;
  declared_predecessor : Parser.completed_command option;
  declared_previous_global : Symbol.t option;
  declared_publication : Declaration_collection.publication;
  declared_source : Parser.global_publication;
  declared_type : Type_reference.t;
  declared_dimensions : declared_dimension list;
  mutable declared_completion : Ast.global_declarator option;
}

let ( let* ) = Result.bind

type declared_function = {
  function_table : Symbol_table.t;
  function_namespace : Declaration_collection.namespace;
  function_publication : Declaration_collection.publication;
  function_source : Parser.completed_function_header;
}

let declare_function ?activation ~table ~namespace publication header =
  if
    (not (Declaration_collection.namespace_owns_table namespace table))
    || not
         (Declaration_collection.namespace_owns_publication namespace
            publication)
  then Error "completed function header requires its exact namespace and table"
  else if
    not
      (Option.fold ~none:false
         ~some:(( == ) header.Parser.function_publication)
         (Declaration_collection.publication_source_function publication))
  then
    Error "completed function header requires its original source publication"
  else if
    not
      (Parser.function_header_is_current header
      || Option.fold ~none:false
           ~some:(fun activation ->
             Source_activation.owns_namespace activation namespace)
           activation
         && Source_activation.function_header activation header)
  then
    Error
      "completed function header is outside its original callback or \
       activation event"
  else
    Ok
      {
        function_table = table;
        function_namespace = namespace;
        function_publication = publication;
        function_source = header;
      }

let declared_function_source declaration = declaration.function_source

let declared_function_symbol declaration =
  Declaration_collection.publication_symbol declaration.function_publication

let declared_function_owns_table declaration table =
  declaration.function_table == table

let declared_function_owns_namespace declaration namespace =
  declaration.function_namespace == namespace

let validate_dimension ~table ~(dimension : Ast.array_dimension) checked =
  if checked.prepared.dimension_table != table then
    Error "checked array dimension belongs to another semantic table"
  else if checked.completed.dimension_ast != dimension then
    Error "checked array dimension belongs to another source node"
  else Ok ()

let dimension_runtime_dependencies checked =
  checked.prepared.runtime_dependencies

let dimension_preparation_runtime_dependencies prepared =
  prepared.runtime_dependencies

let dimension_count checked = checked.prepared.count
let dimension_work checked = checked.prepared.work
let dimension_receipt checked = checked.completed
let prepared_dimension_count checked = checked.count
let dimension_preparation_source checked = checked.preparation
let dimension_preparation_work checked = checked.work
let dimension_preparation_namespace checked = checked.dimension_namespace

let validate_dimension_queries checked queries =
  let original = checked.prepared.queries in
  if
    List.length original = List.length queries
    && List.for_all2 ( == ) original queries
  then Ok ()
  else Error "checked dimension has a substituted query manifest"

let declared_array_size ~table ~namespace ~command ~name ~dimensions ~checked
    size =
  let rec loop index predecessor total dimensions checked =
    match (dimensions, checked) with
    | [], [] -> Ok total
    | dimension :: dimensions, value :: checked ->
        let* () = validate_dimension ~table ~dimension value in
        let prepared = value.prepared in
        let source = prepared.preparation in
        let owner = source.dimension_owner in
        if
          prepared.dimension_namespace != namespace
          || owner.dimensions_command != command
          || owner.dimensions_name != name
          || source.dimension_index <> index
          ||
          match (source.dimension_predecessor, predecessor) with
          | None, None -> false
          | Some left, Some right -> left != right
          | _ -> true
        then
          Error "declared array size lacks its original ordered owner evidence"
        else
          (* PrsArrayDims retains I64 products, including the zero extent of
             an unsized first dimension. Storage admission has its own limits. *)
          loop (index + 1) (Some value.completed)
            (Int64.mul total prepared.count)
            dimensions checked
    | _ ->
        Error "sizeof requires the selected declaration's checked array extent"
  in
  loop 0 None size dimensions checked

let seed_primitive ~table ~entry ~symbol ~primitive =
  if
    (not (Symbol_table.owns_symbol table symbol))
    || Symbol.kind symbol <> Symbol.Internal_type
    || Visibility.kind entry <> Visibility.Internal_type
    || Symbol.name symbol <> Visibility.name entry
  then Error "primitive compiler record has a foreign symbol or entry"
  else
    let info = Primitive_type.info primitive in
    if
      Primitive_type.of_spelling (Visibility.name entry) <> Some primitive
      && Primitive_type.of_storage_spelling (Visibility.name entry)
         <> Some primitive
    then Error "primitive compiler record disagrees with its seeded type"
    else
      Ok
        {
          table;
          entry;
          symbol;
          primitive = Some primitive;
          byte_size = Int64.of_int info.byte_size;
          internal = true;
          runtime_dimensions = [];
          aggregate_stamp = None;
        }

let seed_public_union ~table ~entry ~symbol
    ~(source : Generated.Primitive_raw_types.public_union) =
  let path = Generated.Primitive_raw_types.kernel_source_path in
  let line = source.source_line in
  if
    (not
       (List.exists (( == ) source) Generated.Primitive_raw_types.public_unions))
    || (not (Symbol_table.owns_symbol table symbol))
    || Symbol.kind symbol <> Symbol.Aggregate_type
    || Visibility.kind entry <> Visibility.Class
    || Symbol.name symbol <> source.public_spelling
    || Visibility.name entry <> source.public_spelling
    || Symbol.origin symbol <> Symbol.Pinned_source { path; line }
    || Visibility.origin entry <> Visibility.Pinned_source { path; line }
  then Error "public union compiler record has a foreign seeded association"
  else
    match Primitive_type.of_storage_spelling source.storage_spelling with
    | None -> Error "public union lacks its checked primitive backing"
    | Some primitive ->
        let info = Primitive_type.info primitive in
        if
          info.declaration_form <> Primitive_type.Public_union
          || info.spelling <> source.public_spelling
          || info.declaration_source_line <> source.source_line
        then
          Error
            "public union compiler record disagrees with its pinned declaration"
        else
          Ok
            {
              table;
              entry;
              symbol;
              primitive = Some primitive;
              byte_size = Int64.of_int info.byte_size;
              internal = false;
              runtime_dimensions = [];
              aggregate_stamp = None;
            }

let scalar_size type_ =
  if Type.pointer_depth type_ > 0 then
    Ok (Int64.of_int Primitive_type.pointer_byte_size)
  else
    match Type.base type_ with
    | Type.Primitive (_, primitive) ->
        Ok (Int64.of_int (Primitive_type.info primitive).byte_size)
    | Type.Aggregate _ -> Error "sizeof requires the selected aggregate layout"

type aggregate_progress = {
  progress_namespace : Declaration_collection.namespace;
  progress_publication : Declaration_collection.publication;
  progress_source : Parser.aggregate_publication;
  mutable progress_phase : Parser.aggregate_phase option;
  mutable progress_scopes : (Ast.aggregate_kind * int64) list;
  mutable progress_record : (t, string) result;
  mutable progress_finished : bool;
  progress_stamp : aggregate_stamp;
}

let begin_aggregate ~table ~namespace publication =
  match Declaration_collection.publication_source_aggregate publication with
  | Some source
    when Declaration_collection.namespace_owns_table namespace table
         && Declaration_collection.namespace_owns_publication namespace
              publication
         && Parser.aggregate_publication_is_current source ->
      let stamp = { current_stamp = ref () } in
      Ok
        {
          progress_namespace = namespace;
          progress_publication = publication;
          progress_source = source;
          progress_phase = None;
          progress_scopes = [ (source.aggregate_kind, 0L) ];
          progress_finished = false;
          progress_stamp = stamp;
          progress_record =
            Ok
              {
                table;
                entry = source.aggregate_entry;
                symbol = Declaration_collection.publication_symbol publication;
                primitive = None;
                byte_size = 0L;
                internal = false;
                runtime_dimensions = [];
                aggregate_stamp = Some (stamp, stamp.current_stamp);
              };
        }
  | _ ->
      Error "partial aggregate metadata requires its original live publication"

let aggregate_metadata progress = progress.progress_record

let same_phase left right =
  match (left, right) with
  | None, None -> true
  | Some left, Some right -> left == right
  | _ -> false

let advance_aggregate ~dimensions progress (phase : Parser.aggregate_phase) =
  if
    progress.progress_finished
    || phase.phase_aggregate != progress.progress_source
    || (not (Parser.aggregate_phase_is_current phase))
    || not (same_phase phase.phase_predecessor progress.progress_phase)
  then
    Error "aggregate layout phase is foreign, expired, repeated or out of order"
  else
    let scopes = progress.progress_scopes in
    let* record, scopes =
      match phase.phase_step with
      | Parser.Aggregate_body_started base ->
          if Option.is_some progress.progress_phase then
            Error "aggregate body has already started"
          else
            Ok
              ( (match base with
                | None -> progress.progress_record
                | Some _ ->
                    Error
                      "retained aggregate bases require original selected \
                       layout metadata"),
                scopes )
      | Parser.Aggregate_union_entered ->
          let size =
            match progress.progress_record with
            | Ok record -> record.byte_size
            | Error _ -> 0L
          in
          Ok (progress.progress_record, (Ast.Union_aggregate, size) :: scopes)
      | Parser.Aggregate_union_left -> (
          match scopes with
          | _ :: (_ :: _ as rest) -> Ok (progress.progress_record, rest)
          | _ -> Error "aggregate union phase has no original enclosing scope")
      | Parser.Aggregate_offset_reached ->
          Ok
            ( Error
                "retained aggregate offsets require original expression \
                 preparation",
              scopes )
      | Parser.Aggregate_member_prepared member ->
          let record =
            let* record = progress.progress_record in
            if Option.is_some member.member_callback then
              Error "retained aggregate callbacks require original preparation"
            else
              let* type_ =
                Source_type_reference.builtin member.member_type
                  member.member_pointers
              in
              let* element_size =
                scalar_size (Type_reference.resolved_type type_)
              in
              let checked =
                List.filter_map dimensions member.member_dimensions
              in
              let* _ =
                declared_array_size ~table:record.table
                  ~namespace:progress.progress_namespace
                  ~command:
                    progress.progress_source.aggregate_header
                      .declaration_command ~name:member.member_name
                  ~dimensions:member.member_dimensions ~checked 1L
              in
              if
                List.exists
                  (fun dimension ->
                    dimension.prepared.runtime_dependencies <> [])
                  checked
              then
                Error
                  "retained aggregate runtime bounds require original runtime \
                   layout admission"
              else
                let origin =
                  Closed_numeric_expression.origin member.member_name.location
                in
                let* member_size =
                  Source_aggregate_layout.member_extent ~origin ~element_size
                    ~counts:(List.map dimension_count checked)
                in
                let kind, union_base = List.hd scopes in
                let* byte_size =
                  Source_aggregate_layout.place_member ~origin ~kind ~union_base
                    ~current_size:record.byte_size ~member_size
                in
                Ok { record with byte_size }
          in
          Ok (record, scopes)
    in
    progress.progress_phase <- Some phase;
    progress.progress_scopes <- scopes;
    let stamp = progress.progress_stamp in
    stamp.current_stamp <- ref ();
    progress.progress_record <-
      Result.map
        (fun record ->
          { record with aggregate_stamp = Some (stamp, stamp.current_stamp) })
        record;
    Ok ()

let complete_aggregate ?progress ?(dimensions = fun _ -> None) ~table ~namespace
    publication receipt =
  let source = receipt.Parser.aggregate_publication in
  if
    not
      (Declaration_collection.namespace_owns_table namespace table
      && Declaration_collection.namespace_owns_publication namespace publication
      && Parser.aggregate_completion_is_current receipt
      && Option.fold ~none:false ~some:(( == ) source)
           (Declaration_collection.publication_source_aggregate publication))
  then
    Error
      "aggregate metadata requires its original live publication and completion"
  else
    let* () =
      match progress with
      | None -> Ok ()
      | Some progress ->
          if
            progress.progress_finished
            || progress.progress_namespace != namespace
            || progress.progress_publication != publication
            || progress.progress_source != source
            || (not
                  (same_phase progress.progress_phase
                     receipt.aggregate_final_phase))
            || List.length progress.progress_scopes <> 1
          then
            Error
              "aggregate completion lacks its original complete layout phase \
               chain"
          else (
            progress.progress_finished <- true;
            progress.progress_stamp.current_stamp <- ref ();
            Ok ())
    in
    let symbol = Declaration_collection.publication_symbol publication in
    let* byte_size =
      match receipt.aggregate_item with
      | Ast.Aggregate_forward_declaration forward
        when forward.name == source.aggregate_name -> Ok 0L
      | Ast.Aggregate_definition definition
        when definition.name == source.aggregate_name ->
          let member_dimensions (member : Ast.aggregate_member_declarator) =
            let checked =
              List.filter_map dimensions member.member_array_dimensions
            in
            let* _ =
              declared_array_size ~table ~namespace
                ~command:source.aggregate_header.declaration_command
                ~name:member.member_name
                ~dimensions:member.member_array_dimensions ~checked 1L
            in
            if
              List.exists
                (fun dimension -> dimension.prepared.runtime_dependencies <> [])
                checked
            then
              Error
                "retained aggregate runtime bounds require original runtime \
                 layout admission"
            else Ok (List.map dimension_count checked)
          in
          Source_aggregate_layout.layout ~dimensions:member_dimensions ~table
            ~namespace ~symbol definition
      | _ -> Error "aggregate completion has another original declaration"
    in
    let* () =
      match progress with
      | None -> Ok ()
      | Some progress ->
          let* record = progress.progress_record in
          if record.byte_size = byte_size then Ok ()
          else
            Error "aggregate completion differs from its original member phases"
    in
    Ok
      {
        table;
        entry = source.aggregate_entry;
        symbol;
        primitive = None;
        byte_size;
        internal = false;
        runtime_dimensions = [];
        aggregate_stamp = None;
      }

let rebind_primitive ~table ~symbol record =
  if
    Option.is_none record.primitive
    || (not (Symbol_table.owns_symbol table symbol))
    || Symbol.kind symbol <> Symbol.kind record.symbol
    || Symbol.name symbol <> Symbol.name record.symbol
    || Symbol.origin symbol <> Symbol.origin record.symbol
  then Error "forked primitive compiler record has a foreign symbol"
  else Ok { record with table; symbol }

let published_scalar ?(dimensions = []) ~table ~namespace publication =
  if
    (not (Declaration_collection.namespace_owns_table namespace table))
    || not
         (Declaration_collection.namespace_owns_publication namespace
            publication)
  then Error "compiler record publication belongs to another namespace or table"
  else
    match Declaration_collection.publication_source_global publication with
    | None -> Error "compiler record has no original parser global publication"
    | Some source ->
        if Option.is_some source.global_function_pointer then
          Error "sizeof requires the selected function-pointer signature"
        else
          let* type_reference =
            Source_type_reference.builtin source.global_header.type_specifier
              source.global_pointer_layers
          in
          let* byte_size =
            scalar_size (Type_reference.resolved_type type_reference)
          in
          let* byte_size =
            declared_array_size ~table ~namespace
              ~command:source.global_header.declaration_command
              ~name:source.global_name ~dimensions:source.global_dimensions
              ~checked:dimensions byte_size
          in
          Ok
            {
              table;
              entry = source.global_entry;
              symbol = Declaration_collection.publication_symbol publication;
              primitive = None;
              byte_size;
              internal = false;
              runtime_dimensions =
                List.concat_map dimension_runtime_dependencies dimensions;
              aggregate_stamp = None;
            }

let declare_global ~dimensions ~table ~namespace ~predecessor ~previous_global
    publication =
  let* _ = published_scalar ~dimensions ~table ~namespace publication in
  let source =
    Option.get (Declaration_collection.publication_source_global publication)
  in
  if
    Option.is_some source.global_header.binding
    || List.exists
         (fun (modifier : Ast.declaration_modifier) ->
           modifier.kind <> Ast.Public)
         source.global_header.modifiers
  then Error "partial storage requires an ordinary code-heap definition"
  else
    let* declared_type =
      Source_type_reference.builtin source.global_header.type_specifier
        source.global_pointer_layers
    in
    Ok
      {
        declared_table = table;
        declared_namespace = namespace;
        declared_predecessor = predecessor;
        declared_previous_global = previous_global;
        declared_publication = publication;
        declared_source = source;
        declared_type;
        declared_dimensions = dimensions;
        declared_completion = None;
      }

let declared_global_symbol declaration =
  Declaration_collection.publication_symbol declaration.declared_publication

let declared_global_source declaration = declaration.declared_source
let declared_global_type declaration = declaration.declared_type

let declared_global_dimensions declaration =
  List.map dimension_count declaration.declared_dimensions

let declared_dimension_preparation dimension = dimension.prepared

let declared_global_runtime_dependencies declaration =
  List.concat_map
    (fun dimension -> dimension.prepared.runtime_dependencies)
    declaration.declared_dimensions

let declared_global_owns_table declaration table =
  declaration.declared_table == table

let declared_global_owns_namespace declaration namespace =
  declaration.declared_namespace == namespace

let declared_global_predecessor declaration = declaration.declared_predecessor

let declared_global_previous_global declaration =
  declaration.declared_previous_global

let complete_declared_global declaration event =
  match event with
  | Parser.Global_completed (source, completed)
    when source == declaration.declared_source
         && completed.name == source.global_name
         && completed.pointer_layers == source.global_pointer_layers
         && completed.array_dimensions == source.global_dimensions
         && Option.is_none declaration.declared_completion ->
      declaration.declared_completion <- Some completed;
      Ok ()
  | _ -> Error "declared storage completion is foreign or repeated"

let declared_global_completion declaration = declaration.declared_completion

let validate_declared_global_type declaration global =
  let module Global = Global_type_resolution in
  let reference = Global.global_type_reference global in
  let original = declaration.declared_type in
  let dimensions = Global.global_array_dimensions global in
  if
    Global.global_symbol global != declared_global_symbol declaration
    || Global.global_declarator_kind global <> Global.Object
    || (not
          (Type.equal
             (Type_reference.resolved_type reference)
             (Type_reference.resolved_type original)))
    || Type_reference.spelling reference <> Type_reference.spelling original
    || Type_reference.spelling_origin reference
       <> Type_reference.spelling_origin original
    || Type_reference.pointer_origins reference
       <> Type_reference.pointer_origins original
    || List.length dimensions <> List.length declaration.declared_dimensions
    || not
         (List.for_all2
            (fun dimension checked ->
              Option.fold ~none:false
                ~some:(fun source -> source == checked.completed.dimension_ast)
                (Global.array_dimension_source dimension))
            dimensions declaration.declared_dimensions)
  then Error "completed storage substituted its declared type or dimensions"
  else Ok ()

let bind_retained_scalar ~table ~entry global =
  let symbol = Global_type_resolution.global_symbol global in
  if
    (not (Symbol_table.owns_symbol table symbol))
    || Symbol.kind symbol <> Symbol.Global_variable
    || Visibility.kind entry <> Visibility.Global_variable
    || Symbol.name symbol <> Visibility.name entry
  then Error "retained compiler record has a foreign symbol or entry"
  else if Global_type_resolution.global_array_dimensions global <> [] then
    Error "sizeof requires the retained global's checked array extent"
  else
    match Global_type_resolution.global_declarator_kind global with
    | Global_type_resolution.Function_pointer _ ->
        Error "sizeof requires the retained function-pointer signature"
    | Global_type_resolution.Object ->
        let* byte_size =
          scalar_size
            (Type_reference.resolved_type
               (Global_type_resolution.global_type_reference global))
        in
        Ok
          {
            table;
            entry;
            symbol;
            primitive = None;
            byte_size;
            internal = false;
            runtime_dimensions = [];
            aggregate_stamp = None;
          }

let read_sizeof ~table ~(root : Parser.query_root) (record : t) =
  if record.table != table || not (Symbol_table.owns_symbol table record.symbol)
  then Error "sizeof compiler record belongs to another semantic table"
  else if
    Visibility.kind record.entry = Visibility.Class
    && Option.is_none record.primitive
    && not (Parser.query_root_is_current root)
    || Option.fold ~none:false
         ~some:(fun (stamp, version) -> stamp.current_stamp != version)
         record.aggregate_stamp
  then
    Error
      "partial sizeof requires the current aggregate snapshot at its original \
       query callback"
  else
    match (root.query_node, root.query_lookup) with
    | Parser.Sizeof_target _, Visibility.Present entry
      when entry == record.entry -> Ok { owner = Hash_record record; root }
    | _ -> Error "sizeof read does not select this exact compiler record"

let read_local_sizeof ~dimensions ~table ~namespace ~function_publication
    ~(root : Parser.query_root) =
  if
    (not (Declaration_collection.namespace_owns_table namespace table))
    || not
         (Declaration_collection.namespace_owns_publication namespace
            function_publication)
  then Error "local sizeof owner belongs to another namespace or table"
  else
    match
      ( root.query_node,
        root.query_lookup,
        root.query_local,
        Declaration_collection.publication_source_function function_publication
      )
    with
    | ( Parser.Sizeof_target name,
        Visibility.Shadowed_by_local,
        Some source,
        Some function_ )
      when source.local_environment == root.query_environment
           && source.local_command == root.query_command
           && source.local_spelling = name.spelling
           && function_.function_header.declaration_command
              == source.local_command
           && function_.function_environment == source.local_environment ->
        let declared_size type_specifier pointer_layers function_pointer =
          let* type_reference =
            Source_type_reference.builtin type_specifier pointer_layers
          in
          match function_pointer with
          | Some (pointer : Ast.function_pointer_declarator) ->
              let* depth =
                Source_type_reference.pointer_depth pointer.indirection_layers
              in
              if depth = 0 then
                Error "selected function pointer lacks original indirection"
              else
                (* PrsType (PrsVar.HC:350-356) assigns the RT_PTR companion
                 class, independently of the signature's return type. The
                 original declarator is retained by this local publication. *)
                Ok (Int64.of_int Primitive_type.pointer_byte_size, false)
          | None ->
              let type_ = Type_reference.resolved_type type_reference in
              let* byte_size = scalar_size type_ in
              let internal =
                Type.pointer_depth type_ = 0
                &&
                match Type.base type_ with
                | Type.Primitive (Type.Internal_storage, _) -> true
                | Type.Primitive (Type.Public_spelling, primitive) ->
                    (Primitive_type.info primitive).declaration_form
                    = Primitive_type.Internal_type
                | Type.Aggregate _ -> false
              in
              Ok (byte_size, internal)
        in
        let* byte_size, internal =
          match source.local_source with
          | Parser.Local_parameter parameter ->
              declared_size parameter.type_specifier parameter.pointer_layers
                parameter.function_pointer
          | Parser.Local_variable local ->
              let* byte_size, internal =
                declared_size local.local_type_specifier
                  local.local_pointer_layers local.local_function_pointer
              in
              let* byte_size =
                declared_array_size ~table ~namespace
                  ~command:source.local_command ~name:local.local_name
                  ~dimensions:local.local_array_dimensions ~checked:dimensions
                  byte_size
              in
              Ok (byte_size, internal)
          | Parser.Variadic_count _ ->
              Ok
                ( Int64.of_int (Primitive_type.info Primitive_type.I64).byte_size,
                  true )
          | Parser.Variadic_vector _ ->
              (* PrsVar.HC:392-403 gives argv an internal I64 class and a
               127-element extent, independently of its eight-byte slot. *)
              Ok
                ( Int64.of_int
                    (127 * (Primitive_type.info Primitive_type.I64).byte_size),
                  true )
        in
        Ok
          {
            owner =
              Local_record
                {
                  table;
                  source;
                  byte_size;
                  internal;
                  runtime_dimensions =
                    List.concat_map dimension_runtime_dependencies dimensions;
                };
            root;
          }
    | _ ->
        Error
          "local sizeof read lacks its original function and source publication"

let sizeof_table read =
  match read.owner with
  | Hash_record record -> record.table
  | Local_record local -> local.table

let complete_sizeof ~table ~(receipt : Parser.completed_query) read =
  if sizeof_table read != table || read.root != receipt.query_root then
    Error "sizeof completion belongs to another read or semantic table"
  else
    match receipt.query_expression with
    | Ast.Sizeof_expression expression when expression.sizeof_members = [] ->
        Ok ()
    | _ -> Error "sizeof requires its original checked member reads"

let sizeof_value read ~pointer =
  if pointer then Int64.of_int Primitive_type.pointer_byte_size
  else
    match read.owner with
    | Hash_record record -> record.byte_size
    | Local_record local -> local.byte_size

let sizeof_primitive read =
  match read.owner with
  | Hash_record record -> record.primitive
  | Local_record _ -> None

let sizeof_is_internal read =
  match read.owner with
  | Hash_record record -> record.internal
  | Local_record local -> local.internal

let origin (location : Frontend.Ast.location) =
  Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let complete_query ?sizeof_read ~table
    ~(receipt : Frontend.Parser.completed_query) () =
  let open Frontend in
  let root = receipt.query_root in
  if
    root.query_environment
    != Parser.context_environment root.query_command.command_context
  then Error "query selection has a foreign parser environment"
  else
    let facts =
      match (root.query_node, receipt.query_expression) with
      | Parser.Defined_target operand, Ast.Defined_expression expression
        when operand == expression.defined_operand ->
          Some
            ( Defined_operand,
              operand.defined_operand_spelling,
              origin operand.defined_operand_location )
      | Parser.Sizeof_target target, Ast.Sizeof_expression expression
        when target == expression.sizeof_target ->
          Some (Sizeof_root, target.spelling, origin target.location)
      | Parser.Offset_target target, Ast.Offset_expression expression
        when target == expression.offset_target ->
          Some (Offset_root, target.spelling, origin target.location)
      | _ -> None
    in
    match facts with
    | None -> Error "query selection does not retain its original AST root"
    | Some (role, name, origin) ->
        let checked =
          match sizeof_read with
          | None -> Ok ()
          | Some read -> complete_sizeof ~table ~receipt read
        in
        Result.map
          (fun () -> { table; receipt; role; name; origin; sizeof_read })
          checked

let validate_query ~table ~role ~name ~origin selection =
  if selection.table != table then
    Error "query selection belongs to another symbol table"
  else if
    selection.role <> role || selection.name <> name
    || selection.origin <> origin
  then Error "query selection belongs to another source occurrence or role"
  else Ok ()

let query_expression selection = selection.receipt.query_expression
let query_owns_table selection table = selection.table == table

let query_is_local selection =
  match selection.receipt.query_root.query_lookup with
  | Frontend.Symbol_visibility.Shadowed_by_local -> true
  | Frontend.Symbol_visibility.Absent | Frontend.Symbol_visibility.Present _ ->
      false

let query_presence selection =
  match selection.role with
  | Defined_operand -> Some selection.receipt.query_root.query_present
  | Sizeof_root | Offset_root -> None

let query_sizeof selection =
  match (selection.sizeof_read, selection.receipt.query_expression) with
  | Some read, Frontend.Ast.Sizeof_expression expression ->
      let pointer = expression.sizeof_pointer_layers <> [] in
      Some (sizeof_primitive read, sizeof_value read ~pointer, pointer)
  | _ -> None

let query_runtime_dependencies selection =
  match selection.sizeof_read with
  | None -> []
  | Some read -> (
      match read.owner with
      | Hash_record record -> record.runtime_dimensions
      | Local_record local -> local.runtime_dimensions)

let query_constant selection =
  match query_presence selection with
  | Some present -> Some (if present then 1L else 0L)
  | None -> query_sizeof selection |> Option.map (fun (_, value, _) -> value)

let validate_query_manifest ~table ~expression queries =
  let rec loop expressions queries =
    match (expressions, queries) with
    | [], [] -> Ok ()
    | expression :: expressions, query :: queries
      when query.table == table && query.receipt.query_expression == expression
      -> loop expressions queries
    | _ -> Error "query manifest lacks its exact ordered source reads or table"
  in
  loop (Query_source.source_queries expression) queries

let prepare_dimension ~table ~namespace ~max_work
    ~(preparation : Parser.array_dimension_preparation) ~queries =
  let module Numeric = Closed_numeric_expression in
  let work = ref 0 in
  let result =
    let owner = preparation.dimension_owner in
    if not (Declaration_collection.namespace_owns_table namespace table) then
      Error "array preparation belongs to another namespace or table"
    else if max_work < 0 then Error "array preparation has a negative allowance"
    else if
      owner.dimensions_environment
      != Parser.context_environment owner.dimensions_command.command_context
    then Error "array preparation has a foreign parser environment"
    else if
      List.exists
        (fun query ->
          query.receipt.query_root.query_command != owner.dimensions_command
          || query.receipt.query_root.query_environment
             != owner.dimensions_environment)
        queries
    then Error "array preparation has query reads from another parser command"
    else
      let* count =
        match preparation.dimension_expression with
        | None ->
            if preparation.dimension_index <> 0 || queries <> [] then
              Error
                "only the first array dimension can be empty, without query \
                 reads"
            else Ok 0L
        | Some expression ->
            let* () = validate_query_manifest ~table ~expression queries in
            let expression =
              Numeric.of_ast ~allow_floating:true ~query_expression ~queries
                expression
            in
            let rec closed = function
              | Numeric.Selected_query_expression query ->
                  if Option.is_some (query_constant query) then Ok ()
                  else
                    Error
                      "array preparation requires the selected query's checked \
                       value"
              | Numeric.Integer_expression _
              | Numeric.Unsigned_integer_expression _
              | Numeric.Floating_expression _ -> Ok ()
              | Numeric.Unary_expression { operand; _ } -> closed operand
              | Numeric.Binary_expression { left; right; _ } ->
                  let* () = closed left in
                  closed right
              | Numeric.Current_position_expression _ ->
                  Error
                    "array preparation requires unresolved current-position \
                     evidence"
              | Numeric.Dependency_expression { detail; _ } ->
                  Error
                    ("array preparation requires checked runtime evaluation: "
                   ^ detail)
              | Numeric.Unsupported_expression { description; _ } ->
                  Error
                    ("array preparation has an unsupported expression: "
                   ^ description)
            in
            let* () = closed expression in
            let consume () =
              if !work >= max_work then
                Error
                  (Numeric.make_error "HCIRVM0007"
                     (Numeric.Invalid_input "array preparation work limit")
                     "the bounded array dimension preparation work limit was \
                      exhausted")
              else (
                incr work;
                Ok ())
            in
            let* count =
              Numeric.evaluate_expression ~consume
                ~query_origin:(fun query -> query.origin)
                ~query_value:query_constant ~context:Numeric.Array_dimension
                ~current_position:0L expression
              |> Result.map_error Numeric.error_to_string
            in
            if count < 0L then
              Error (Printf.sprintf "array extent %Ld is negative" count)
            else Ok count
      in
      Ok
        {
          dimension_table = table;
          dimension_namespace = namespace;
          preparation;
          queries;
          count;
          work = !work;
          runtime_dependencies =
            List.concat_map query_runtime_dependencies queries;
        }
  in
  (result, !work)

let complete_dimension ~receipt prepared =
  let source = prepared.preparation in
  let dimension = receipt.Parser.dimension_ast in
  if
    receipt.dimension_preparation != source
    || dimension.opening_bracket != source.dimension_opening
    ||
    match (dimension.dimension_expression, source.dimension_expression) with
    | None, None -> false
    | Some left, Some right -> left != right
    | _ -> true
  then
    Error
      "checked dimension completion substituted its original preparation or \
       children"
  else Ok { prepared; completed = receipt }

let propose_runtime_dimension ~namespace ~preparation ~count ~work =
  if work < 0 || count < 0L then
    Error "runtime dimension proposal requires nonnegative count and work"
  else
    Ok
      {
        proposal_namespace = namespace;
        proposal_source = preparation;
        proposal_count = count;
        proposal_work = work;
      }

let complete_runtime_dimension ~table ~receipt ~queries proposal =
  let preparation = proposal.proposal_source in
  let namespace = proposal.proposal_namespace in
  let* () =
    if Declaration_collection.namespace_owns_table namespace table then Ok ()
    else Error "runtime dimension proposal belongs to another table"
  in
  let* expression =
    match preparation.dimension_expression with
    | Some expression -> Ok expression
    | None -> Error "runtime proposal has no original expression"
  in
  let* () = validate_query_manifest ~table ~expression queries in
  complete_dimension ~receipt
    {
      dimension_table = table;
      dimension_namespace = namespace;
      preparation;
      queries;
      count = proposal.proposal_count;
      work = proposal.proposal_work;
      runtime_dependencies =
        proposal :: List.concat_map query_runtime_dependencies queries;
    }

let global_dimensions record =
  Global_resolution.global_record_global record
  |> Global_type_resolution.global_array_dimensions

let validate_global_dimension_owner ~table ~record ~dimension =
  let symbol = Global_resolution.global_record_symbol record in
  let global = Global_resolution.global_record_global record in
  let index = Global_type_resolution.array_dimension_index dimension in
  if
    (not (Symbol_table.owns_symbol table symbol))
    || symbol != Global_type_resolution.global_symbol global
    || Symbol.kind symbol <> Symbol.Global_variable
  then Error "checked global extent belongs to another table or symbol"
  else
    match List.nth_opt (global_dimensions record) index with
    | Some original when original == dimension -> Ok ()
    | _ -> Error "checked global extent has a foreign dimension or index"

let positive_extent value =
  if Int64.compare value 0L > 0 then Ok ()
  else
    Error
      (Printf.sprintf
         "has nonpositive extent %Ld; persistent arrays require a positive \
          fixed extent"
         value)

let reuse_global_dimension ~table ~record ~dimension ~queries checked =
  let* () = validate_global_dimension_owner ~table ~record ~dimension in
  let namespace = checked.prepared.dimension_namespace in
  let* publication =
    match
      Declaration_collection.source_global_for_symbol namespace
        (Global_resolution.global_record_symbol record)
    with
    | Some publication
      when Declaration_collection.namespace_owns_table namespace table
           && Declaration_collection.namespace_owns_publication namespace
                publication -> Ok publication
    | _ ->
        Error "checked global extent lacks its original declaration publication"
  in
  let* source =
    match Declaration_collection.publication_source_global publication with
    | Some source -> Ok source
    | None ->
        Error "checked global extent lacks its original parser declaration"
  in
  let preparation = checked.prepared.preparation in
  let owner = preparation.dimension_owner in
  let index = Global_type_resolution.array_dimension_index dimension in
  let rec same_dimensions originals dimensions =
    match (originals, dimensions) with
    | [], [] -> true
    | original :: originals, dimension :: dimensions ->
        (match Global_type_resolution.array_dimension_source dimension with
          | Some source -> source == original
          | None -> false)
        && same_dimensions originals dimensions
    | _ -> false
  in
  if
    owner.dimensions_command != source.global_header.declaration_command
    || owner.dimensions_environment != source.global_environment
    || owner.dimensions_name != source.global_name
    || preparation.dimension_index <> index
    || not (same_dimensions source.global_dimensions (global_dimensions record))
  then
    Error
      "checked global extent has substituted declaration dimensions or owner"
  else
    let* original =
      match Global_type_resolution.array_dimension_source dimension with
      | Some original -> Ok original
      | None ->
          Error
            "checked global extent needs its complete original AST dimension"
    in
    let* () = validate_dimension ~table ~dimension:original checked in
    let* () = validate_dimension_queries checked queries in
    let* () = positive_extent checked.prepared.count in
    Ok
      {
        extent_table = table;
        extent_record = record;
        extent_dimension = dimension;
        extent_evaluation = Prepared_extent checked;
        extent_count = checked.prepared.count;
      }

let evaluate_global_dimension ~table ~record ~dimension ~queries =
  let module Numeric = Closed_numeric_expression in
  let* () = validate_global_dimension_owner ~table ~record ~dimension in
  let* source =
    match
      Global_type_resolution.array_dimension_source_expression dimension
    with
    | Some expression -> Ok expression
    | None ->
        Error "has an empty extent; inferred persistent arrays are unresolved"
  in
  let* () =
    match queries with
    | None -> Ok ()
    | Some queries -> validate_query_manifest ~table ~expression:source queries
  in
  let expression =
    Numeric.of_ast ~query_expression
      ~queries:(Option.value queries ~default:[])
      source
  in
  let rec closed = function
    | Numeric.Selected_query_expression query ->
        if Option.is_some (query_constant query) then Ok ()
        else Error "requires the selected query's checked constant metadata"
    | Numeric.Integer_expression _
    | Numeric.Unsigned_integer_expression _
    | Numeric.Floating_expression _ -> Ok ()
    | Numeric.Current_position_expression _ ->
        Error "requires unresolved current-position layout evidence"
    | Numeric.Dependency_expression { detail; _ } ->
        Error ("requires unresolved closed layout evidence: " ^ detail)
    | Numeric.Unsupported_expression { description; _ } ->
        Error ("has an unsupported closed layout expression: " ^ description)
    | Numeric.Unary_expression { operand; _ } -> closed operand
    | Numeric.Binary_expression { left; right; _ } ->
        let* () = closed left in
        closed right
  in
  let* () = closed expression in
  let* count =
    Numeric.evaluate_expression
      ~query_origin:(fun query -> query.origin)
      ~query_value:query_constant ~context:Numeric.Array_dimension
      ~current_position:0L expression
    |> Result.map_error (fun error -> ": " ^ Numeric.error_to_string error)
  in
  let* () = positive_extent count in
  Ok
    {
      extent_table = table;
      extent_record = record;
      extent_dimension = dimension;
      extent_evaluation = Legacy_extent queries;
      extent_count = count;
    }

let make_global_extent ~table ~record dimensions =
  let rec loop predecessor count expected dimensions =
    match (expected, dimensions) with
    | [], [] -> Ok count
    | original :: expected, value :: dimensions ->
        if
          value.extent_table != table
          || value.extent_record != record
          || value.extent_dimension != original
        then Error "global extent has foreign or reordered dimension evidence"
        else
          let* () =
            validate_global_dimension_owner ~table ~record ~dimension:original
          in
          let* next =
            match value.extent_evaluation with
            | Legacy_extent _ -> Ok None
            | Prepared_extent checked ->
                let matches =
                  match
                    ( checked.prepared.preparation.dimension_predecessor,
                      predecessor )
                  with
                  | None, None -> true
                  | Some left, Some (prior, prepared) ->
                      (match
                         Global_type_resolution.array_dimension_source prior
                       with
                        | Some source -> source == left.dimension_ast
                        | None -> false)
                      && Option.fold ~none:true
                           ~some:(fun right -> left == right)
                           prepared
                  | _ -> false
                in
                if matches then Ok (Some checked.completed)
                else
                  Error
                    "global extent has a substituted preparation predecessor"
          in
          let* () = positive_extent value.extent_count in
          if
            Int64.compare count (Int64.div Int64.max_int value.extent_count) > 0
          then Error "global extent overflows the declared element count"
          else
            loop
              (Some (original, next))
              (Int64.mul count value.extent_count)
              expected dimensions
    | _ -> Error "global extent lacks its complete ordered dimensions"
  in
  let symbol = Global_resolution.global_record_symbol record in
  if not (Symbol_table.owns_symbol table symbol) then
    Error "global extent belongs to another semantic table"
  else
    let* count = loop None 1L (global_dimensions record) dimensions in
    Ok
      {
        global_extent_table = table;
        global_extent_record = record;
        global_extent_dimensions = dimensions;
        global_extent_count = count;
      }

let global_extent_record extent = extent.global_extent_record
let global_dimension_extent_count extent = extent.extent_count

let global_extent_runtime_dependencies extent =
  List.concat_map
    (fun dimension ->
      match dimension.extent_evaluation with
      | Prepared_extent prepared -> dimension_runtime_dependencies prepared
      | Legacy_extent queries ->
          Option.fold ~none:[]
            ~some:(List.concat_map query_runtime_dependencies)
            queries)
    extent.global_extent_dimensions

let global_extent_dimensions extent =
  List.map (fun value -> value.extent_count) extent.global_extent_dimensions

let global_extent_element_count extent = extent.global_extent_count

let validate_global_extent ~table ~record extent =
  if
    extent.global_extent_table != table || extent.global_extent_record != record
  then
    Error "checked global extent belongs to another table or declaration record"
  else Ok ()

let bind_retained_global ~table ~entry ~record ~extent =
  let global = Global_resolution.global_record_global record in
  match global_dimensions record with
  | [] ->
      let* () =
        match extent with
        | None -> Ok ()
        | Some extent -> validate_global_extent ~table ~record extent
      in
      bind_retained_scalar ~table ~entry global
  | _ ->
      let symbol = Global_resolution.global_record_symbol record in
      let* extent =
        match extent with
        | None ->
            Error "sizeof requires the retained global's checked array extent"
        | Some extent ->
            let* () = validate_global_extent ~table ~record extent in
            Ok extent
      in
      if
        symbol != Global_type_resolution.global_symbol global
        || (not (Symbol_table.owns_symbol table symbol))
        || Symbol.kind symbol <> Symbol.Global_variable
        || Visibility.kind entry <> Visibility.Global_variable
        || Symbol.name symbol <> Visibility.name entry
      then Error "retained compiler record has a foreign symbol or entry"
      else if
        Global_type_resolution.global_declarator_kind global
        <> Global_type_resolution.Object
      then Error "sizeof requires the retained function-pointer signature"
      else
        let* byte_size =
          Global_type_resolution.global_type_reference global
          |> Type_reference.resolved_type |> scalar_size
        in
        Ok
          {
            table;
            entry;
            symbol;
            primitive = None;
            byte_size = Int64.mul byte_size extent.global_extent_count;
            internal = false;
            runtime_dimensions = global_extent_runtime_dependencies extent;
            aggregate_stamp = None;
          }
