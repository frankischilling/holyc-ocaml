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

module Dimension_identities = Hashtbl.Make (struct
  type t = runtime_dimension_proposal

  let equal = ( == )
  let hash value = Hashtbl.hash value.proposal_source.dimension_opening.span
end)

let merge_dimension_dependencies inherited selected =
  match selected with
  | [] -> inherited
  | _ ->
      let seen = Dimension_identities.create 16 in
      List.filter
        (fun dimension ->
          if Dimension_identities.mem seen dimension then false
          else (
            Dimension_identities.add seen dimension ();
            true))
        (inherited @ selected)

type aggregate_offset = {
  offset_table : Symbol_table.t;
  offset_namespace : Declaration_collection.namespace;
  offset_phase : Parser.aggregate_phase;
  offset_expression : Ast.expression;
  offset_value : int64;
  offset_work : int;
  offset_runtime : bool;
  offset_dimensions : runtime_dimension_proposal list;
  offset_dependencies : aggregate_offset list;
}

let aggregate_offset_namespace offset = offset.offset_namespace
let aggregate_offset_table offset = offset.offset_table
let aggregate_offset_phase offset = offset.offset_phase
let aggregate_offset_expression offset = offset.offset_expression
let aggregate_offset_value offset = offset.offset_value
let aggregate_offset_work offset = offset.offset_work

let aggregate_offset_is_runtime offset =
  offset.offset_runtime
  || offset.offset_dimensions <> []
  || offset.offset_dependencies <> []

module Offset_identities = Hashtbl.Make (struct
  type t = aggregate_offset

  let equal = ( == )
  let hash offset = Hashtbl.hash offset.offset_phase.phase_location.span
end)

let merge_offset_dependencies inherited selected =
  match selected with
  | [] -> inherited
  | _ ->
      let seen = Offset_identities.create 16 in
      List.filter
        (fun offset ->
          if Offset_identities.mem seen offset then false
          else (
            Offset_identities.add seen offset ();
            true))
        (inherited @ selected)

let aggregate_offset_runtime_dependencies offset =
  (if offset.offset_runtime then [ offset ] else [])
  @ offset.offset_dependencies

type compiler_position = {
  position_source : Parser.compiler_position_source;
  position_value : int64;
  position_dimensions : runtime_dimension_proposal list;
  position_dependencies : aggregate_offset list;
}

module Position_sources = Hashtbl.Make (struct
  type t = Parser.compiler_position_source

  let equal = ( == )
  let hash = Hashtbl.hash
end)

type aggregate_stamp = { mutable current_stamp : unit ref }

type sizeof_class =
  | Declared_class of Type.t
  | Pointer_class
  | Unavailable_class

type member_metadata_value =
  | Metadata_word of int64
  | Metadata_string of string

type member_metadata = {
  metadata_table : Symbol_table.t;
  metadata_namespace : Declaration_collection.namespace;
  metadata_phase : Parser.aggregate_phase;
  metadata_source : Ast.aggregate_member_metadata;
  metadata_value : member_metadata_value;
  metadata_work : int;
}

type aggregate_field = {
  field_phase : Parser.aggregate_phase;
  field_namespace : Declaration_collection.namespace;
  field_publication : Declaration_collection.publication;
  field_type : Type.t;
  field_type_reference : Type_reference.t;
  field_callback_signature :
    (Type_reference.t * Function_type_resolution.function_pointer) option;
  field_offset : int64;
  field_element_size : int64;
  field_count : int64;
  field_dimensions : runtime_dimension_proposal list;
  field_offsets : aggregate_offset list;
  field_counts : int64 list;
  field_record : t option;
  field_metadata : member_metadata list;
}

and t = {
  table : Symbol_table.t;
  entry : Visibility.entry;
  symbol : Symbol.t;
  primitive : Primitive_type.t option;
  object_class : sizeof_class option;
  byte_size : int64;
  internal : bool;
  runtime_dimensions : runtime_dimension_proposal list;
  runtime_offsets : aggregate_offset list;
  aggregate_owner :
    (Declaration_collection.namespace
    * Declaration_collection.publication
    * Parser.completed_aggregate option)
    option;
  aggregate_stamp : (aggregate_stamp * unit ref) option;
  aggregate_base_snapshot : t option;
  aggregate_fields : aggregate_field list option;
  aggregate_initializer_backing : (Type_reference.t * t option) option option;
  aggregate_initializer_body : Parser.aggregate_phase option;
  aggregate_initializer_records : (Type.t -> t option) option;
}

type aggregate_member = {
  member_phase : Parser.aggregate_phase;
  member_namespace : Declaration_collection.namespace;
  member_type : Type_reference.t;
  member_size : int64;
  member_record : t option;
}

type automatic_aggregate_allocation = {
  automatic_table : Symbol_table.t;
  automatic_namespace : Declaration_collection.namespace;
  automatic_function : Declaration_collection.publication;
  automatic_source : Parser.function_local_allocation;
  automatic_name : Ast.identifier;
  automatic_type : Type_reference.t;
  automatic_size : int64;
  automatic_record : t;
}

let automatic_aggregate_size allocation = allocation.automatic_size
let automatic_aggregate_source allocation = allocation.automatic_source
let automatic_aggregate_local_name allocation = allocation.automatic_name
let automatic_aggregate_record allocation = allocation.automatic_record

let automatic_aggregate_owns allocation ~table ~parent =
  allocation.automatic_table == table
  && Declaration_collection.namespace_scope allocation.automatic_namespace
     == parent

let static_initializer_aggregate_matches ~table ~namespace ~publication ~source
    allocation =
  allocation.automatic_table == table
  && allocation.automatic_namespace == namespace
  && allocation.automatic_function == publication
  && allocation.automatic_source == source
  && source.Parser.allocation_storage = Ast.Static_local

let automatic_aggregate_type allocation =
  Type_reference.resolved_type allocation.automatic_type

let automatic_aggregate_runtime_dependencies allocation =
  allocation.automatic_record.runtime_dimensions

let automatic_aggregate_offset_dependencies allocation =
  allocation.automatic_record.runtime_offsets

type aggregate_reference_visibility = {
  reference_table : Symbol_table.t;
  reference_namespace : Declaration_collection.namespace;
  reference_source : Parser.reference_selection;
  reference_completed_symbols : Symbol.t list;
  reference_partial_records : (Symbol.t * t) list;
}

type inherited_base = {
  inherited_phase : Parser.aggregate_phase;
  inherited_namespace : Declaration_collection.namespace;
  inherited_selected : Declaration_collection.publication;
  inherited_record : t;
}

type inherited_metadata = {
  metadata_record : t;
  metadata_definition : Ast.aggregate_definition;
}

type aggregate_value_header = inherited_metadata
type aggregate_import = inherited_metadata

type sizeof_member_read = {
  member_source : Parser.query_member;
  member_class : sizeof_class;
  member_byte_size : int64;
  member_dimensions : runtime_dimension_proposal list;
  member_offsets : aggregate_offset list;
}

type sizeof_owner =
  | Hash_record of t
  | Local_record of {
      table : Symbol_table.t;
      namespace : Declaration_collection.namespace;
      source : Parser.local_publication;
      byte_size : int64;
      internal : bool;
      runtime_dimensions : runtime_dimension_proposal list;
      runtime_offsets : aggregate_offset list;
    }

type sizeof_read = {
  owner : sizeof_owner;
  root : Parser.query_root;
  root_class : sizeof_class;
  members_rev : sizeof_member_read list;
}

type sizeof_member_start = {
  start_source : Parser.query_member_start;
  start_namespace : Declaration_collection.namespace;
  start_read : sizeof_read;
}

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
  offset_dependencies : aggregate_offset list;
}

type declared_dimension = {
  prepared : dimension_preparation;
  completed : Parser.completed_array_dimension;
}

module Local_allocations = Hashtbl.Make (struct
  type t = Parser.function_local_allocation

  let equal = ( == )
  let hash receipt = Hashtbl.hash receipt.Parser.allocation_local.local_spelling
end)

type static_allocation = {
  static_table : Symbol_table.t;
  static_namespace : Declaration_collection.namespace;
  static_publication : Declaration_collection.publication;
  static_receipt : Parser.function_local_allocation;
  static_dimensions : declared_dimension list;
  static_aggregate : automatic_aggregate_allocation option;
}

type compiler_positions = {
  positions_sources : Common.Source_manager.t;
  positions : compiler_position option Position_sources.t;
  allocations :
    (int64 * runtime_dimension_proposal list * aggregate_offset list) option
    Local_allocations.t;
  allocated_sizes :
    (int64 * runtime_dimension_proposal list * aggregate_offset list) option
    Local_allocations.t;
  static_allocations : static_allocation Local_allocations.t;
}

let create_compiler_positions ~sources =
  {
    positions_sources = sources;
    positions = Position_sources.create 32;
    allocations = Local_allocations.create 32;
    allocated_sizes = Local_allocations.create 32;
    static_allocations = Local_allocations.create 32;
  }

let compiler_positions_own_sources positions sources =
  positions.positions_sources == sources

let static_allocation positions receipt =
  Local_allocations.find_opt positions.static_allocations receipt

let static_allocation_owns_table value table = value.static_table == table
let static_allocation_table value = value.static_table
let static_allocation_namespace value = value.static_namespace
let static_allocation_publication value = value.static_publication
let static_allocation_receipt value = value.static_receipt
let static_allocation_dimensions value = value.static_dimensions
let static_allocation_aggregate value = value.static_aggregate
let compiler_position_value position = position.position_value
let compiler_position_dependencies position = position.position_dependencies

let compiler_position_runtime_dependencies position =
  position.position_dimensions

let rec native_size positions = function
  | Function_record_phase.Size_value value ->
      Option.map (fun value -> (value, [], [])) value
  | Function_record_phase.Size_add (size, bytes) ->
      Option.bind (native_size positions size)
        (fun (size, dimensions, dependencies) ->
          if size > Int64.sub Int64.max_int bytes then None
          else Some (Int64.add size bytes, dimensions, dependencies))
  | Function_record_phase.Size_local (size, receipt) -> (
      match Local_allocations.find_opt positions.allocated_sizes receipt with
      | Some value -> value
      | None ->
          let value =
            Option.bind (native_size positions size)
              (fun (size, dimensions, dependencies) ->
                Option.bind
                  (Local_allocations.find_opt positions.allocations receipt)
                  (Option.map (fun (bytes, local_dimensions, inherited) ->
                       let alignment =
                         if bytes >= 8L then -8L
                         else if bytes >= 4L then -4L
                         else if bytes >= 2L then -2L
                         else -1L
                       in
                       ( Int64.logand (Int64.sub size bytes) alignment,
                         merge_dimension_dependencies dimensions
                           local_dimensions,
                         merge_offset_dependencies dependencies inherited ))))
          in
          Local_allocations.add positions.allocated_sizes receipt value;
          value)

let record_function_position positions record receipt =
  let sources =
    receipt.Parser.position_function.function_header.declaration_sources
  in
  if not (compiler_positions_own_sources positions sources) then
    Error "function position belongs to another source manager"
  else if Position_sources.mem positions.positions receipt.position_source then
    Error "function position write already has original layout evidence"
  else
    Function_record_phase.size_at record receipt
    |> Result.map (fun value ->
        Position_sources.add positions.positions receipt.position_source
          (Option.map
             (fun (position_value, position_dimensions, position_dependencies)
                ->
               {
                 position_source = receipt.position_source;
                 position_value;
                 position_dimensions;
                 position_dependencies;
               })
             (native_size positions value)))

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
  declared_callback :
    (Parser.completed_callback_signature
    * Function_type_resolution.function_pointer)
    option;
  declared_dimensions : declared_dimension list;
  declared_aggregate : global_aggregate_allocation option;
  mutable declared_completion : Ast.global_declarator option;
}

and global_aggregate_allocation = {
  global_object_table : Symbol_table.t;
  global_object_namespace : Declaration_collection.namespace;
  global_object_publication : Declaration_collection.publication;
  global_object_source : Parser.global_publication;
  global_object_type : Type_reference.t;
  global_object_record : t;
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

let dimension_offset_dependencies checked = checked.prepared.offset_dependencies

let dimension_preparation_offset_dependencies prepared =
  prepared.offset_dependencies

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

let scalar_size type_ =
  if Type.pointer_depth type_ > 0 then
    Ok (Int64.of_int Primitive_type.pointer_byte_size)
  else
    match Type.base type_ with
    | Type.Primitive (_, primitive) ->
        Ok (Int64.of_int (Primitive_type.info primitive).byte_size)
    | Type.Aggregate _ -> Error "sizeof requires the selected aggregate layout"

let record_local_allocation ?automatic_aggregate ~table ~namespace ~dimensions
    positions record receipt =
  let local = receipt.Parser.allocation_local in
  let snapshot = Function_record_phase.snapshot record in
  if
    (not
       (compiler_positions_own_sources positions
          receipt.allocation_function.function_header.declaration_sources))
    || (not (Function_record_phase.owns_table snapshot table))
    || not (Function_record_phase.owns_namespace snapshot namespace)
  then
    Error
      "local allocation belongs to another source manager, table or namespace"
  else if
    (not (Function_record_phase.local_allocation_is_next record receipt))
    || Local_allocations.mem positions.allocations receipt
  then Error "local allocation is foreign, expired, repeated or out of order"
  else
    let* () =
      match automatic_aggregate with
      | Some allocation
        when allocation.automatic_table != table
             || allocation.automatic_namespace != namespace
             || allocation.automatic_source != receipt
             || allocation.automatic_function
                != Function_record_phase.publication snapshot ->
          Error "automatic class allocation belongs to another original local"
      | _ -> Ok ()
    in
    let* allocation =
      match local.local_source with
      | Parser.Local_variable source ->
          let* extent =
            if dimensions = [] && source.local_array_dimensions <> [] then
              Ok None
            else
              declared_array_size ~table ~namespace ~command:local.local_command
                ~name:source.local_name
                ~dimensions:source.local_array_dimensions ~checked:dimensions 1L
              |> Result.map Option.some
          in
          if receipt.allocation_storage = Ast.Static_local then
            Ok (Some (0L, [], []))
          else
            let base =
              match source.local_function_pointer with
              | Some pointer -> (
                  match
                    Source_type_reference.pointer_depth
                      pointer.indirection_layers
                  with
                  | Ok depth when depth > 0 ->
                      Some
                        (Int64.of_int Primitive_type.pointer_byte_size, [], [])
                  | _ -> None)
              | None -> (
                  match automatic_aggregate with
                  | Some allocation ->
                      Some
                        ( allocation.automatic_size,
                          allocation.automatic_record.runtime_dimensions,
                          allocation.automatic_record.runtime_offsets )
                  | None -> (
                      match
                        Source_type_reference.builtin
                          source.local_type_specifier
                          source.local_pointer_layers
                      with
                      | Error _ -> None
                      | Ok reference ->
                          scalar_size (Type_reference.resolved_type reference)
                          |> Result.to_option
                          |> Option.map (fun size -> (size, [], []))))
            in
            Ok
              (Option.bind base
                 (fun (base, inherited_dimensions, inherited_offsets) ->
                   Option.map
                     (fun extent ->
                       ( Int64.mul base extent,
                         merge_dimension_dependencies inherited_dimensions
                           (List.concat_map dimension_runtime_dependencies
                              dimensions),
                         merge_offset_dependencies inherited_offsets
                           (List.concat_map dimension_offset_dependencies
                              dimensions) ))
                     extent))
      | _ -> Error "frame allocation requires its original local variable"
    in
    let* () = Function_record_phase.observe_local_allocation record receipt in
    Local_allocations.add positions.allocations receipt allocation;
    if receipt.allocation_storage = Ast.Static_local then
      Local_allocations.add positions.static_allocations receipt
        {
          static_table = table;
          static_namespace = namespace;
          static_publication = Function_record_phase.publication snapshot;
          static_receipt = receipt;
          static_dimensions = dimensions;
          static_aggregate = automatic_aggregate;
        };
    Ok ()

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
          object_class = None;
          byte_size = Int64.of_int info.byte_size;
          internal = true;
          runtime_dimensions = [];
          runtime_offsets = [];
          aggregate_owner = None;
          aggregate_stamp = None;
          aggregate_base_snapshot = None;
          aggregate_fields = None;
          aggregate_initializer_backing = None;
          aggregate_initializer_body = None;
          aggregate_initializer_records = None;
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
          Visibility.public_primitive entry <> Some primitive
          || info.declaration_form <> Primitive_type.Public_union
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
              object_class = None;
              byte_size = Int64.of_int info.byte_size;
              internal = false;
              runtime_dimensions = [];
              runtime_offsets = [];
              aggregate_owner = None;
              aggregate_stamp = None;
              aggregate_base_snapshot = None;
              aggregate_fields = None;
              aggregate_initializer_backing = None;
              aggregate_initializer_body = None;
              aggregate_initializer_records = None;
            }

let aggregate_snapshot_is_current record =
  Option.fold ~none:true
    ~some:(fun (stamp, version) -> stamp.current_stamp == version)
    record.aggregate_stamp

let select_aggregate_base ~table ~namespace ~selected_publication
    (phase : Parser.aggregate_phase) record =
  let invalid () =
    Error
      "inherited layout requires the original selected class and current \
       source record"
  in
  match
    ( phase.phase_step,
      Declaration_collection.publication_source_aggregate selected_publication,
      record.aggregate_owner )
  with
  | ( Parser.Aggregate_base_attached selection,
      Some selected,
      Some (owner_namespace, owner_publication, _) ) ->
      let same_identity =
        match
          ( Declaration_collection.publication_aggregate_identity
              selected_publication,
            Declaration_collection.publication_aggregate_identity
              owner_publication )
        with
        | Some selected, Some owner -> selected == owner
        | _ -> false
      in
      if
        (not (Parser.aggregate_phase_is_current phase))
        || record.table != table
        || owner_namespace != namespace
        || (not (Declaration_collection.namespace_owns_table namespace table))
        || (not
              (Declaration_collection.namespace_owns_publication namespace
                 selected_publication))
        || (not
              (Declaration_collection.namespace_owns_publication namespace
                 owner_publication))
        || selected.aggregate_entry != selection.base_entry
        || selected.aggregate_environment != selection.base_environment
        || phase.phase_aggregate.aggregate_environment
           != selection.base_environment
        || record.symbol
           != Declaration_collection.publication_symbol owner_publication
        || (not same_identity)
        || (not (aggregate_snapshot_is_current record))
        || (not
              (Option.fold ~none:false ~some:(( == ) owner_publication)
                 (Declaration_collection.current_aggregate_publication namespace
                    selected_publication)))
        || not
             (Option.fold ~none:false
                ~some:(fun (source : Parser.aggregate_publication) ->
                  source.aggregate_entry == record.entry
                  && source.aggregate_environment == selection.base_environment)
                (Declaration_collection.publication_source_aggregate
                   owner_publication))
      then invalid ()
      else
        Ok
          {
            inherited_phase = phase;
            inherited_namespace = namespace;
            inherited_selected = selected_publication;
            inherited_record = record;
          }
  | _ -> invalid ()

let select_aggregate_member ~table ~namespace ~selected_aggregate phase
    (record : t option) =
  let ( let* ) = Result.bind in
  if not (Parser.aggregate_phase_is_current phase) then
    Error "member layout requires its original live placement phase"
  else
    match phase.Parser.phase_step with
    | Parser.Aggregate_member_prepared member
      when Option.is_none member.member_callback ->
        let* () =
          Source_type_reference.validate_selected_aggregate ~table ~namespace
            selected_aggregate
        in
        let* type_ =
          Source_type_reference.selected_header_class selected_aggregate
            member.member_type member.member_pointers
        in
        let type_value = Type_reference.resolved_type type_ in
        let* size, record =
          if Type.pointer_depth type_value > 0 then
            Ok (Int64.of_int Primitive_type.pointer_byte_size, None)
          else
            match record with
            | Some record
              when record.table == table && aggregate_snapshot_is_current record
              -> (
                match record.aggregate_owner with
                | Some (owner, publication, _) when owner == namespace ->
                    let* symbol =
                      match
                        Declaration_collection.publication_aggregate_identity
                          publication
                      with
                      | Some symbol -> Ok symbol
                      | None ->
                          Error
                            "member class record lacks its canonical identity"
                    in
                    if
                      symbol
                      != Source_type_reference.selected_base_symbol
                           selected_aggregate
                    then
                      Error
                        "member class record substituted its original selected \
                         identity"
                    else Ok (record.byte_size, Some record)
                | _ ->
                    Error
                      "member class record belongs to another source namespace")
            | _ -> Error "member class layout lacks its current original record"
        in
        Ok
          {
            member_phase = phase;
            member_namespace = namespace;
            member_type = type_;
            member_size = size;
            member_record = record;
          }
    | _ -> Error "member layout requires an original selected object member"

let select_automatic_aggregate ~table ~namespace ~function_publication
    ~selected_aggregate receipt (record : t) =
  if
    (not (Parser.function_local_allocation_is_current receipt))
    || receipt.allocation_storage <> Ast.Automatic_local
       && receipt.allocation_storage <> Ast.Static_local
  then Error "automatic class layout requires its original live allocation"
  else if
    (not (Declaration_collection.namespace_owns_table namespace table))
    || (not
          (Declaration_collection.namespace_owns_publication namespace
             function_publication))
    || not
         (Option.fold ~none:false
            ~some:(( == ) receipt.allocation_function)
            (Declaration_collection.publication_source_function
               function_publication))
  then Error "automatic class allocation belongs to another original function"
  else
    match receipt.allocation_local.local_source with
    | Parser.Local_variable local
      when Option.is_none local.local_function_pointer -> (
        let* () =
          Source_type_reference.validate_selected_aggregate ~table ~namespace
            selected_aggregate
        in
        let* type_ =
          Source_type_reference.selected_header_class selected_aggregate
            local.local_type_specifier local.local_pointer_layers
        in
        let* () =
          match (local.local_type_specifier, record.aggregate_owner) with
          | Ast.Named_type_specifier name, Some (_, publication, _) -> (
              match
                Declaration_collection.publication_source_aggregate publication
              with
              | Some source
                when source.aggregate_name == name
                     && Option.is_some source.aggregate_backing ->
                  Error
                    "inline backed class has no ordinary automatic allocation"
              | _ -> Ok ())
          | _ -> Ok ()
        in
        if Type.pointer_depth (Type_reference.resolved_type type_) <> 0 then
          Error "automatic class object allocation cannot describe a pointer"
        else if
          receipt.allocation_local.local_command
          != receipt.allocation_function.function_header.declaration_command
          || receipt.allocation_local.local_environment
             != receipt.allocation_function.function_environment
        then Error "automatic class allocation has a foreign local occurrence"
        else
          match record.aggregate_owner with
          | Some (owner, publication, _)
            when record.table == table && owner == namespace
                 && Declaration_collection.namespace_owns_publication namespace
                      publication
                 && record.symbol
                    == Declaration_collection.publication_symbol publication
                 && aggregate_snapshot_is_current record
                 && Option.fold ~none:false ~some:(( == ) publication)
                      (Declaration_collection.current_aggregate_publication
                         namespace publication)
                 && Option.fold ~none:false
                      ~some:
                        (( == )
                           (Source_type_reference.selected_base_symbol
                              selected_aggregate))
                      (Declaration_collection.publication_aggregate_identity
                         publication)
                 && Option.fold ~none:false
                      ~some:(fun (source : Parser.aggregate_publication) ->
                        source.aggregate_entry == record.entry
                        && source.aggregate_environment
                           == receipt.allocation_local.local_environment)
                      (Declaration_collection.publication_source_aggregate
                         publication) ->
              (* PrsType consumes name and dimension lookahead before
                 PrsVar.HC:531-532 reads this selected class size. The saved
                 value includes zero; later forward completion cannot grow it. *)
              Ok
                {
                  automatic_table = table;
                  automatic_namespace = namespace;
                  automatic_function = function_publication;
                  automatic_source = receipt;
                  automatic_name = local.local_name;
                  automatic_type = type_;
                  automatic_size = record.byte_size;
                  automatic_record = record;
                }
          | _ ->
              Error
                "automatic class allocation lacks its selected current source \
                 record")
    | _ -> Error "automatic class allocation requires an original object local"

let validate_automatic_aggregate ~table ~parent ~function_symbol ~local_symbol
    ~local_name ~checked_type allocation =
  if
    allocation.automatic_table != table
    || Declaration_collection.namespace_scope allocation.automatic_namespace
       != parent
    || not
         (Declaration_collection.namespace_owns_table
            allocation.automatic_namespace table)
  then Error "automatic class allocation belongs to another table or namespace"
  else if
    function_symbol
    != Declaration_collection.publication_symbol allocation.automatic_function
    || not (Symbol_table.owns_symbol table function_symbol)
  then Error "automatic class allocation belongs to another function"
  else if
    allocation.automatic_name != local_name
    || (not (Symbol_table.owns_symbol table local_symbol))
    || Symbol.kind local_symbol <> Symbol.Local_variable
    || Symbol.name local_symbol <> local_name.Ast.spelling
    || Symbol.origin local_symbol
       <> Initializer_source.origin_of_location local_name.location
  then Error "automatic class allocation belongs to another original local"
  else if
    not
      (Type.equal checked_type
         (Type_reference.resolved_type allocation.automatic_type))
  then Error "automatic class allocation changed its selected original type"
  else Ok ()

let capture_aggregate_reference_visibility ~table ~namespace reference records =
  if
    (not (Parser.reference_selection_is_current reference))
    || not (Declaration_collection.namespace_owns_table namespace table)
  then Error "aggregate visibility requires its original live reference"
  else
    let rec completed symbols partial = function
      | [] ->
          Ok
            {
              reference_table = table;
              reference_namespace = namespace;
              reference_source = reference;
              reference_completed_symbols = symbols;
              reference_partial_records = partial;
            }
      | (record : t) :: rest -> (
          match record.aggregate_owner with
          | Some
              ( owner,
                publication,
                Some
                  ({ aggregate_item = Ast.Aggregate_definition _; _ } as source)
              )
            when record.table == table && owner == namespace
                 && Declaration_collection.namespace_owns_publication namespace
                      publication
                 && record.symbol
                    == Declaration_collection.publication_symbol publication
                 && aggregate_snapshot_is_current record
                 && source.aggregate_publication.aggregate_environment
                    == Parser.selected_environment reference
                 && source.aggregate_publication.aggregate_entry == record.entry
                 && Option.fold ~none:false
                      ~some:(( == ) source.aggregate_publication)
                      (Declaration_collection.publication_source_aggregate
                         publication)
                 && (Parser.selected_member_reference reference
                    || Option.fold ~none:false ~some:(( == ) publication)
                         (Declaration_collection.current_aggregate_publication
                            namespace publication)) -> (
              match
                Declaration_collection.publication_aggregate_identity
                  publication
              with
              | Some symbol -> completed (symbol :: symbols) partial rest
              | None ->
                  Error "completed reference class lacks its canonical identity"
              )
          | Some (owner, publication, None)
            when record.table == table && owner == namespace
                 && Declaration_collection.namespace_owns_publication namespace
                      publication
                 && record.symbol
                    == Declaration_collection.publication_symbol publication
                 && aggregate_snapshot_is_current record -> (
              match
                ( Declaration_collection.publication_source_aggregate publication,
                  Declaration_collection.publication_aggregate_identity
                    publication,
                  Declaration_collection.current_aggregate_publication namespace
                    publication )
              with
              | Some source, Some symbol, current
                when (Parser.selected_member_reference reference
                     || Option.fold ~none:false ~some:(( == ) publication)
                          current)
                     && Option.is_none source.aggregate_header.binding
                     && source.aggregate_environment
                        == Parser.selected_environment reference
                     && source.aggregate_entry == record.entry ->
                  completed symbols ((symbol, record) :: partial) rest
              | _ ->
                  Error "partial member visibility has another original class")
          | _ ->
              Error
                "aggregate reference visibility requires current completed \
                 original class records")
    in
    completed [] [] records

let aggregate_reference_allows ~table ~parent ~identifier ~symbol visibility =
  visibility.reference_table == table
  && Declaration_collection.namespace_scope visibility.reference_namespace
     == parent
  && Declaration_collection.namespace_owns_table visibility.reference_namespace
       table
  && Parser.selected_identifier visibility.reference_source == identifier
  && Symbol_table.owns_symbol table symbol
  && List.exists (( == ) symbol) visibility.reference_completed_symbols

let aggregate_reference_is_member visibility =
  Parser.selected_member_reference visibility.reference_source

let aggregate_reference_partial ~table ~parent ~identifier ~symbol visibility =
  if
    visibility.reference_table != table
    || Declaration_collection.namespace_scope visibility.reference_namespace
       != parent
    || Parser.selected_identifier visibility.reference_source != identifier
    || not (Symbol_table.owns_symbol table symbol)
  then None
  else
    List.find_map
      (fun (original, record) ->
        if original == symbol then Some record else None)
      visibility.reference_partial_records

type aggregate_progress = {
  progress_compiler_positions : compiler_positions;
  progress_namespace : Declaration_collection.namespace;
  progress_publication : Declaration_collection.publication;
  progress_source : Parser.aggregate_publication;
  progress_initializer_backing : (Type_reference.t * t option) option option;
  mutable progress_phase : Parser.aggregate_phase option;
  mutable progress_scopes : (Ast.aggregate_kind * int64) list;
  mutable progress_record : (t, string) result;
  mutable progress_finished : bool;
  progress_stamp : aggregate_stamp;
  mutable progress_negative_offset : int64;
  mutable progress_base_phase : Parser.aggregate_phase option;
  mutable progress_base : inherited_base option;
  mutable progress_body_started : bool;
  mutable progress_body_finished : bool;
  mutable progress_backing_attached : bool;
  mutable progress_offset_attempt : Parser.aggregate_phase option;
  mutable progress_offsets : aggregate_offset list;
  mutable progress_members : aggregate_member list;
  mutable progress_metadata_attempt : Parser.aggregate_phase option;
  mutable progress_metadata : member_metadata list;
}

type runtime_aggregate_offset = {
  runtime_progress : aggregate_progress;
  runtime_phase : Parser.aggregate_phase;
  runtime_table : Symbol_table.t;
  runtime_expression : Ast.expression;
  mutable runtime_finished : bool;
  runtime_dimension_dependencies : runtime_dimension_proposal list;
  runtime_offset_dependencies : aggregate_offset list;
}

type runtime_member_metadata = {
  runtime_metadata_progress : aggregate_progress;
  runtime_metadata_phase : Parser.aggregate_phase;
  runtime_metadata_table : Symbol_table.t;
  runtime_metadata_source : Ast.aggregate_member_metadata;
  mutable runtime_metadata_finished : bool;
  runtime_metadata_dimensions : runtime_dimension_proposal list;
  runtime_metadata_offsets : aggregate_offset list;
}

let runtime_member_metadata_dimension_dependencies value =
  value.runtime_metadata_dimensions

let runtime_member_metadata_offset_dependencies value =
  value.runtime_metadata_offsets

let runtime_aggregate_offset_dimension_dependencies preparation =
  preparation.runtime_dimension_dependencies

let runtime_aggregate_offset_dependencies preparation =
  preparation.runtime_offset_dependencies

let begin_aggregate ?compiler_positions ?backing_record ?previous_record
    ?initializer_records
    ?(selected_aggregate : Function_type_resolution.selected_aggregate_resolver =
      fun _ -> None) ~table ~namespace publication =
  match Declaration_collection.publication_source_aggregate publication with
  | Some source
    when Option.fold ~none:true
           ~some:(fun positions ->
             compiler_positions_own_sources positions
               source.aggregate_header.declaration_sources)
           compiler_positions
         && Declaration_collection.namespace_owns_table namespace table
         && Declaration_collection.namespace_owns_publication namespace
              publication
         && Parser.aggregate_publication_is_current source ->
      let initializer_backing =
        match source.aggregate_backing with
        | None -> Some None
        | Some backing ->
            let reference =
              match backing.Ast.backing_type_specifier with
              | Ast.Named_type_specifier _ as type_specifier ->
                  Option.bind (selected_aggregate type_specifier)
                    (fun selected ->
                      Result.to_option
                        (let* () =
                           Source_type_reference.validate_selected_aggregate
                             ~table ~namespace selected
                         in
                         Source_type_reference.selected_header_class selected
                           type_specifier backing.backing_pointer_layers))
              | type_specifier ->
                  Result.to_option
                    (Source_type_reference.builtin type_specifier
                       backing.backing_pointer_layers)
            in
            Option.map
              (fun reference -> Some (reference, backing_record))
              reference
      in
      let* () =
        match backing_record with
        | None -> Ok ()
        | Some record -> (
            match (initializer_backing, record.aggregate_owner) with
            | Some (Some (reference, _)), Some (owner, selected, _) -> (
                match Type.base (Type_reference.resolved_type reference) with
                | Type.Aggregate symbol
                  when record.table == table && owner == namespace
                       && Type.pointer_depth
                            (Type_reference.resolved_type reference)
                          = 0
                       && Option.fold ~none:false ~some:(( == ) symbol)
                            (Declaration_collection
                             .publication_aggregate_identity selected) -> Ok ()
                | _ ->
                    Error
                      "initializer backing replaced its original selected \
                       class record")
            | _ ->
                Error
                  "initializer backing record has no original class selection")
      in
      let stamp = { current_stamp = ref () } in
      let* initial_backing =
        match previous_record with
        | None -> Ok (Some None)
        | Some previous -> (
            match previous.aggregate_owner with
            | Some (owner, previous_publication, _)
              when previous.table == table && owner == namespace
                   && Option.is_none source.aggregate_header.binding
                   && Option.equal ( == )
                        (Declaration_collection.publication_aggregate_identity
                           previous_publication)
                        (Declaration_collection.publication_aggregate_identity
                           publication)
                   && Option.fold ~none:false
                        ~some:(fun original ->
                          Option.fold ~none:false
                            ~some:(( == ) original.Parser.aggregate_entry)
                            source.aggregate_previous
                          && original.aggregate_environment
                             == source.aggregate_environment
                          && Option.fold ~none:false
                               ~some:(fun binding ->
                                 binding.Ast.kind = Ast.Extern)
                               original.aggregate_header.binding)
                        (Declaration_collection.publication_source_aggregate
                           previous_publication) ->
                Ok previous.aggregate_initializer_backing
            | _ ->
                Error
                  "initial forwarding has no original completed forward \
                   selection")
      in
      let compiler_positions =
        match compiler_positions with
        | Some positions -> positions
        | None ->
            create_compiler_positions
              ~sources:source.aggregate_header.declaration_sources
      in
      Ok
        {
          progress_compiler_positions = compiler_positions;
          progress_namespace = namespace;
          progress_publication = publication;
          progress_source = source;
          progress_initializer_backing = initializer_backing;
          progress_phase = None;
          progress_scopes = [ (source.aggregate_kind, 0L) ];
          progress_finished = false;
          progress_stamp = stamp;
          progress_negative_offset = 0L;
          progress_base_phase = None;
          progress_base = None;
          progress_body_started = false;
          progress_body_finished = false;
          progress_backing_attached = false;
          progress_offset_attempt = None;
          progress_offsets = [];
          progress_members = [];
          progress_metadata_attempt = None;
          progress_metadata = [];
          progress_record =
            Ok
              {
                table;
                entry = source.aggregate_entry;
                symbol = Declaration_collection.publication_symbol publication;
                primitive = None;
                object_class = None;
                byte_size = 0L;
                internal = false;
                runtime_dimensions = [];
                runtime_offsets = [];
                aggregate_owner = Some (namespace, publication, None);
                aggregate_stamp = Some (stamp, stamp.current_stamp);
                aggregate_base_snapshot = None;
                aggregate_fields = Some [];
                aggregate_initializer_backing = initial_backing;
                aggregate_initializer_body = None;
                aggregate_initializer_records = initializer_records;
              };
        }
  | _ ->
      Error "partial aggregate metadata requires its original live publication"

let aggregate_metadata progress = progress.progress_record
let aggregate_fields record = Option.value ~default:[] record.aggregate_fields

let aggregate_field_name field =
  match field.field_phase.phase_step with
  | Parser.Aggregate_member_prepared member -> member.member_name
  | _ -> assert false

let aggregate_initializer_size (record : t) = record.byte_size

let aggregate_initializer_has_members (record : t) =
  Option.is_some record.aggregate_fields

let aggregate_initializer_is_complete (record : t) =
  match record.aggregate_owner with
  | Some (_, _, Some { Parser.aggregate_item = Ast.Aggregate_definition _; _ })
    -> true
  | _ -> Option.is_some record.aggregate_initializer_body

let aggregate_initializer_is_definition (record : t) =
  match record.aggregate_owner with
  | Some (_, publication, _) ->
      Option.fold ~none:false
        ~some:(fun source ->
          Option.is_none source.Parser.aggregate_header.binding)
        (Declaration_collection.publication_source_aggregate publication)
  | None -> false

let aggregate_field_type field = field.field_type
let aggregate_field_type_reference field = field.field_type_reference
let aggregate_field_callback_signature field = field.field_callback_signature
let aggregate_field_offset field = field.field_offset
let aggregate_field_placement_size field = field.field_element_size
let aggregate_field_counts field = field.field_counts
let aggregate_field_record field = field.field_record
let aggregate_field_metadata field = field.field_metadata
let member_metadata_source metadata = metadata.metadata_source
let member_metadata_value metadata = metadata.metadata_value
let member_metadata_work metadata = metadata.metadata_work
let member_metadata_phase metadata = metadata.metadata_phase
let member_metadata_table metadata = metadata.metadata_table
let member_metadata_namespace metadata = metadata.metadata_namespace

let aggregate_field_metadata_find field name =
  List.find_opt
    (fun metadata ->
      metadata.metadata_source.member_metadata_name.spelling = name)
    field.field_metadata

let aggregate_field_callback field =
  match field.field_phase.Parser.phase_step with
  | Parser.Aggregate_member_prepared member -> member.member_callback
  | _ -> assert false

let aggregate_reached_field record name =
  let rec find visited depth record =
    match record.aggregate_owner with
    | None -> None
    | Some (_, publication, _) -> (
        match
          Declaration_collection.publication_aggregate_identity publication
        with
        | None -> None
        | Some symbol when List.memq symbol visited -> None
        | Some symbol -> (
            let field =
              List.fold_left
                (fun found field ->
                  if (aggregate_field_name field).spelling = name then
                    Some field
                  else found)
                None (aggregate_fields record)
            in
            match field with
            | Some field -> Some (symbol, depth, field)
            | None ->
                Option.bind record.aggregate_base_snapshot
                  (find (symbol :: visited) (depth + 1))))
  in
  find [] 0 record

let aggregate_initializer_backing record = record.aggregate_initializer_backing

let aggregate_initializer_record record type_ =
  Option.bind record.aggregate_initializer_records (fun resolve ->
      resolve type_)

let aggregate_record_matches ~(owner : t) ~type_ (record : t) =
  record.table == owner.table
  &&
  match (record.aggregate_owner, owner.aggregate_owner, Type.base type_) with
  | ( Some (namespace, publication, _),
      Some (original, _, _),
      Type.Aggregate symbol )
    when Type.pointer_depth type_ = 0 ->
      namespace == original
      && Option.fold ~none:false ~some:(( == ) symbol)
           (Declaration_collection.publication_aggregate_identity publication)
  | _ -> false

let aggregate_field_element_size ~table ~parent ~aggregate ~type_specifier
    ~(declarator : Ast.aggregate_member_declarator) ~type_ field =
  let namespace = field.field_namespace in
  let publication = field.field_publication in
  if
    (not (Declaration_collection.namespace_owns_table namespace table))
    || Declaration_collection.namespace_scope namespace != parent
    || (not
          (Declaration_collection.namespace_owns_publication namespace
             publication))
    || (not
          (Option.fold ~none:false ~some:(( == ) aggregate)
             (Declaration_collection.publication_aggregate_identity publication)))
    || (not
          (Option.fold ~none:false
             ~some:(( == ) field.field_phase.phase_aggregate)
             (Declaration_collection.publication_source_aggregate publication)))
    || not (Type.equal type_ field.field_type)
  then Error "member placement belongs to another original class or type"
  else
    match field.field_phase.phase_step with
    | Parser.Aggregate_member_prepared original
      when original.member_type == type_specifier
           && original.member_name == declarator.member_name
           && original.member_pointers == declarator.member_pointer_layers
           && original.member_dimensions == declarator.member_array_dimensions
           && Option.is_none original.member_callback
           && Option.is_none declarator.member_function_pointer ->
        Ok field.field_element_size
    | _ -> Error "member placement substituted its original source children"

let same_phase left right =
  match (left, right) with
  | None, None -> true
  | Some left, Some right -> left == right
  | _ -> false

let advance_aggregate
    ?(members =
      fun _ -> Error "member lacks its original selected class layout")
    ?(callbacks = fun _ -> None) ?(callback_types = fun _ -> None)
    ?(bases =
      fun _ -> Error "inherited layout lacks its original selected record")
    ~dimensions progress (phase : Parser.aggregate_phase) =
  if
    progress.progress_finished
    || phase.phase_aggregate != progress.progress_source
    || (not (Parser.aggregate_phase_is_current phase))
    || not (same_phase phase.phase_predecessor progress.progress_phase)
  then
    Error "aggregate layout phase is foreign, expired, repeated or out of order"
  else if
    Option.fold ~none:false
      ~some:
        (Position_sources.mem progress.progress_compiler_positions.positions)
      phase.phase_written_position
  then Error "compiler position write already has original layout evidence"
  else
    let scopes = progress.progress_scopes in
    let* record, scopes =
      match phase.phase_step with
      | Parser.Aggregate_position_reset -> Ok (progress.progress_record, scopes)
      | Parser.Aggregate_base_attached _ ->
          if
            Option.is_some progress.progress_phase
            || progress.progress_body_started
          then
            Error "aggregate base has already been attached or its body started"
          else (
            progress.progress_base_phase <- Some phase;
            let record =
              let* record = progress.progress_record in
              let* base = bases phase in
              if
                base.inherited_phase != phase
                || base.inherited_namespace != progress.progress_namespace
              then
                Error "inherited layout proof belongs to another original phase"
              else
                let* base =
                  select_aggregate_base ~table:record.table
                    ~namespace:progress.progress_namespace
                    ~selected_publication:base.inherited_selected phase
                    base.inherited_record
                in
                progress.progress_base <- Some base;
                Ok
                  {
                    record with
                    byte_size = base.inherited_record.byte_size;
                    aggregate_base_snapshot = Some base.inherited_record;
                    runtime_dimensions =
                      merge_dimension_dependencies record.runtime_dimensions
                        base.inherited_record.runtime_dimensions;
                    runtime_offsets =
                      merge_offset_dependencies record.runtime_offsets
                        base.inherited_record.runtime_offsets;
                  }
            in
            Ok (record, scopes))
      | Parser.Aggregate_body_started base ->
          if progress.progress_body_started then
            Error "aggregate body has already started"
          else if
            match (base, progress.progress_base_phase) with
            | None, None -> false
            | ( Some base,
                Some
                  { phase_step = Parser.Aggregate_base_attached selection; _ } )
              -> selection.base_ast != base
            | _ -> true
          then Error "aggregate body substituted its original base selection"
          else (
            progress.progress_body_started <- true;
            Ok (progress.progress_record, scopes))
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
      | Parser.Aggregate_body_finished ->
          if
            (not progress.progress_body_started)
            || progress.progress_body_finished
            || List.length scopes <> 1
          then Error "aggregate body completion has no original enclosing scope"
          else (
            progress.progress_body_finished <- true;
            let record =
              let* record = progress.progress_record in
              let* byte_size =
                Source_aggregate_layout.finish_size
                  ~origin:
                    (Closed_numeric_expression.origin phase.phase_location)
                  ~size:record.byte_size
                  ~negative_offset:progress.progress_negative_offset
              in
              Ok
                {
                  record with
                  byte_size;
                  aggregate_initializer_body = Some phase;
                }
            in
            Ok (record, scopes))
      | Parser.Aggregate_backing_attached backing ->
          if
            (not progress.progress_body_finished)
            || progress.progress_backing_attached
            || not
                 (Option.fold ~none:false ~some:(( == ) backing)
                    progress.progress_source.aggregate_backing)
          then
            Error
              "aggregate forwarding is foreign, repeated or precedes body \
               completion"
          else (
            progress.progress_backing_attached <- true;
            Ok
              ( Result.map
                  (fun record ->
                    {
                      record with
                      aggregate_initializer_backing =
                        progress.progress_initializer_backing;
                    })
                  progress.progress_record,
                scopes ))
      | Parser.Aggregate_offset_reached _ -> (
          match progress.progress_offsets with
          | offset :: _ when offset.offset_phase == phase -> (
              let* negative_offset =
                Source_aggregate_layout.negative_offset
                  ~origin:
                    (Closed_numeric_expression.origin phase.phase_location)
                  ~previous:progress.progress_negative_offset
                  ~position:offset.offset_value
              in
              progress.progress_negative_offset <- negative_offset;
              progress.progress_record <-
                Result.map
                  (fun record ->
                    {
                      record with
                      (* Preparation already retains the preceding layout. *)
                      runtime_dimensions = offset.offset_dimensions;
                      runtime_offsets =
                        aggregate_offset_runtime_dependencies offset;
                    })
                  progress.progress_record;
              match scopes with
              | (Ast.Class_aggregate, _) :: _ ->
                  Ok
                    ( Result.map
                        (fun record ->
                          { record with byte_size = offset.offset_value })
                        progress.progress_record,
                      scopes )
              | (Ast.Union_aggregate, _) :: rest ->
                  Ok
                    ( progress.progress_record,
                      (Ast.Union_aggregate, offset.offset_value) :: rest )
              | [] -> assert false)
          | _ ->
              Ok
                ( Error
                    "retained aggregate offsets require original expression \
                     preparation",
                  scopes ))
      | Parser.Aggregate_member_prepared member ->
          let record =
            let* record = progress.progress_record in
            let* element_size, selected_record, field_type_reference =
              match (member.member_callback, member.member_type) with
              | None, Ast.Named_type_specifier _ ->
                  let* selected = members phase in
                  if
                    selected.member_phase != phase
                    || selected.member_namespace != progress.progress_namespace
                  then
                    Error
                      "member layout substituted another original placement \
                       phase"
                  else (
                    progress.progress_members <-
                      selected :: progress.progress_members;
                    Ok
                      ( selected.member_size,
                        selected.member_record,
                        selected.member_type ))
              | callback, _ ->
                  let* type_ =
                    match callback with
                    | None ->
                        Source_type_reference.builtin member.member_type
                          member.member_pointers
                    | Some source -> (
                        match callbacks source with
                        | Some header ->
                            Source_type_reference.callback_storage ~header
                              source
                        | None ->
                            Error
                              "retained callback member lacks its original \
                               completed header")
                  in
                  let* size =
                    scalar_size (Type_reference.resolved_type type_)
                  in
                  Ok (size, None, type_)
            in
            let checked = List.filter_map dimensions member.member_dimensions in
            let* field_callback_signature =
              match Option.bind member.member_callback callback_types with
              | None -> Ok None
              | Some (return_type, pointer) as signature ->
                  let* storage_type =
                    Function_type_resolution.function_pointer_storage_type
                      pointer
                  in
                  if
                    (not
                       (Option.equal ( == )
                          (Function_type_resolution.function_pointer_source
                             pointer)
                          member.member_callback))
                    || Type_reference.spelling_origin return_type
                       <> Closed_numeric_expression.origin
                            (Ast.type_specifier_location member.member_type)
                    || Type_reference.spelling return_type
                       <> Ast.type_specifier_spelling member.member_type
                    || Type_reference.pointer_origins return_type
                       <> List.map
                            (fun (layer : Ast.pointer_layer) ->
                              Closed_numeric_expression.origin layer.location)
                            member.member_pointers
                    || (match
                          Type.base (Type_reference.resolved_type return_type)
                        with
                      | Type.Aggregate symbol ->
                          not (Symbol_table.owns_symbol record.table symbol)
                      | Type.Primitive _ -> (
                          match
                            Source_type_reference.builtin member.member_type
                              member.member_pointers
                          with
                          | Ok expected ->
                              not
                                (Type.equal
                                   (Type_reference.resolved_type expected)
                                   (Type_reference.resolved_type return_type))
                          | Error _ -> true))
                    || not
                         (Type.equal storage_type
                            (Type_reference.resolved_type field_type_reference))
                  then
                    Error
                      "callback member substituted its original signature or \
                       return type"
                  else Ok signature
            in
            let* field_count =
              declared_array_size ~table:record.table
                ~namespace:progress.progress_namespace
                ~command:
                  progress.progress_source.aggregate_header.declaration_command
                ~name:member.member_name ~dimensions:member.member_dimensions
                ~checked 1L
            in
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
            Ok
              {
                record with
                byte_size;
                aggregate_fields =
                  Option.map
                    (fun fields ->
                      {
                        field_phase = phase;
                        field_namespace = progress.progress_namespace;
                        field_publication = progress.progress_publication;
                        field_type =
                          Type_reference.resolved_type field_type_reference;
                        field_type_reference;
                        field_callback_signature;
                        field_offset =
                          (match kind with
                          | Ast.Class_aggregate -> record.byte_size
                          | Ast.Union_aggregate -> union_base);
                        field_element_size = element_size;
                        field_count;
                        field_counts = List.map dimension_count checked;
                        field_record = selected_record;
                        field_dimensions =
                          List.concat_map dimension_runtime_dependencies checked;
                        field_offsets =
                          List.concat_map dimension_offset_dependencies checked;
                        field_metadata = [];
                      }
                      :: fields)
                    record.aggregate_fields;
                runtime_dimensions =
                  record.runtime_dimensions
                  @ Option.fold ~none:[]
                      ~some:(fun record -> record.runtime_dimensions)
                      selected_record
                  @ List.concat_map dimension_runtime_dependencies checked;
                runtime_offsets =
                  record.runtime_offsets
                  @ Option.fold ~none:[]
                      ~some:(fun record -> record.runtime_offsets)
                      selected_record
                  @ List.concat_map dimension_offset_dependencies checked;
              }
          in
          Ok (record, scopes)
      | Parser.Aggregate_member_metadata_reached original ->
          let* metadata =
            match progress.progress_metadata with
            | metadata :: _
              when metadata.metadata_phase == phase
                   && metadata.metadata_source == original.metadata ->
                Ok metadata
            | _ -> Error "member metadata lacks its original prepared value"
          in
          let* record = progress.progress_record in
          let* fields =
            match record.aggregate_fields with
            | Some (field :: rest)
              when aggregate_field_name field == original.member_name ->
                Ok
                  ({
                     field with
                     field_metadata = metadata :: field.field_metadata;
                   }
                  :: rest)
            | _ ->
                Error "member metadata substituted its original placed member"
          in
          Ok (Ok { record with aggregate_fields = Some fields }, scopes)
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
    match phase.phase_written_position with
    | None -> Ok ()
    | Some _ when Result.is_error progress.progress_record -> Ok ()
    | Some source ->
        let* record = progress.progress_record in
        let value =
          match scopes with
          | (Ast.Union_aggregate, base) :: _ -> base
          | _ -> record.byte_size
        in
        let positions = progress.progress_compiler_positions.positions in
        if Position_sources.mem positions source then
          Error "compiler position write already has original layout evidence"
        else (
          Position_sources.add positions source
            (Some
               {
                 position_source = source;
                 position_value = value;
                 position_dimensions = record.runtime_dimensions;
                 position_dependencies = record.runtime_offsets;
               });
          Ok ())

let complete_aggregate ?(callbacks = fun _ -> None) ?progress
    ?(dimensions = fun _ -> None) ~table ~namespace publication receipt =
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
            || Option.is_none source.aggregate_header.binding
               && Option.is_some source.aggregate_backing
               && not progress.progress_backing_attached
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
        when forward.name == source.aggregate_name
             && Option.equal ( == ) forward.backing source.aggregate_backing ->
          Ok 0L
      | Ast.Aggregate_definition definition
        when definition.name == source.aggregate_name
             && Option.equal ( == ) definition.backing source.aggregate_backing
        ->
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
            Ok (List.map dimension_count checked)
          in
          let offsets expression =
            match
              Option.bind progress (fun progress ->
                  List.find_opt
                    (fun offset -> offset.offset_expression == expression)
                    progress.progress_offsets)
            with
            | Some offset -> Ok offset.offset_value
            | None ->
                Error "aggregate offset lacks its original checked preparation"
          in
          let initial_size =
            Option.bind progress (fun progress ->
                Option.map
                  (fun base -> base.inherited_record.byte_size)
                  progress.progress_base)
          in
          let members type_specifier (member : Ast.aggregate_member_declarator)
              =
            Option.bind progress (fun progress ->
                List.find_map
                  (fun selected ->
                    match selected.member_phase.phase_step with
                    | Parser.Aggregate_member_prepared original
                      when original.member_type == type_specifier
                           && original.member_name == member.member_name
                           && original.member_pointers
                              == member.member_pointer_layers
                           && original.member_dimensions
                              == member.member_array_dimensions ->
                        Some (selected.member_type, selected.member_size)
                    | _ -> None)
                  progress.progress_members)
          in
          let metadata (member : Ast.aggregate_member_declarator) =
            let prepared =
              Option.bind progress (fun progress ->
                  Result.to_option progress.progress_record)
              |> Option.map aggregate_fields
              |> Option.value ~default:[]
              |> List.find_opt (fun field ->
                  aggregate_field_name field == member.member_name)
            in
            match prepared with
            | Some field
              when List.length field.field_metadata
                   = List.length member.member_metadata
                   && List.for_all2
                        (fun value source -> value.metadata_source == source)
                        (List.rev field.field_metadata)
                        member.member_metadata -> Ok ()
            | None when member.member_metadata = [] -> Ok ()
            | _ ->
                Error "member metadata differs from its original source chain"
          in
          Source_aggregate_layout.layout ~members ~callbacks ~metadata
            ?initial_size ~offsets ~dimensions:member_dimensions ~table
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
        object_class = None;
        byte_size;
        internal = false;
        runtime_dimensions =
          Option.fold ~none:[]
            ~some:(fun progress ->
              match progress.progress_record with
              | Ok record -> record.runtime_dimensions
              | Error _ -> [])
            progress;
        runtime_offsets =
          Option.fold ~none:[]
            ~some:(fun progress ->
              match progress.progress_record with
              | Ok record -> record.runtime_offsets
              | Error _ -> [])
            progress;
        aggregate_owner = Some (namespace, publication, Some receipt);
        aggregate_stamp = None;
        aggregate_base_snapshot =
          Option.bind progress (fun progress ->
              Option.map
                (fun base -> base.inherited_record)
                progress.progress_base);
        aggregate_fields =
          Option.bind progress (fun progress ->
              match progress.progress_record with
              | Ok record -> record.aggregate_fields
              | Error _ -> None);
        aggregate_initializer_backing =
          Option.bind progress (fun progress ->
              match receipt.aggregate_item with
              | Ast.Aggregate_forward_declaration _ ->
                  progress.progress_initializer_backing
              | _ -> (
                  match progress.progress_record with
                  | Ok record -> record.aggregate_initializer_backing
                  | Error _ -> None));
        aggregate_initializer_body =
          Option.bind progress (fun progress ->
              match progress.progress_record with
              | Ok record -> record.aggregate_initializer_body
              | Error _ -> None);
        aggregate_initializer_records =
          Option.bind progress (fun progress ->
              match progress.progress_record with
              | Ok record -> record.aggregate_initializer_records
              | Error _ -> None);
      }

let retain_inherited_metadata ~table ~namespace definition record =
  match record.aggregate_owner with
  | Some
      ( owner_namespace,
        publication,
        Some { aggregate_item = Ast.Aggregate_definition original; _ } )
    when owner_namespace == namespace
         && record.table == table && original == definition
         && Option.is_some original.base
         && Option.is_none record.aggregate_stamp
         && Declaration_collection.namespace_owns_table namespace table
         && Declaration_collection.namespace_owns_publication namespace
              publication ->
      Ok { metadata_record = record; metadata_definition = original }
  | _ ->
      Error "inherited metadata requires its original completed source layout"

let inherited_metadata_owns_definition ~table ~scope definition metadata =
  metadata.metadata_definition == definition
  && metadata.metadata_record.table == table
  &&
  match metadata.metadata_record.aggregate_owner with
  | Some (namespace, _, _) ->
      Declaration_collection.namespace_scope namespace == scope
  | None -> false

let retain_aggregate_value_header ~table ~namespace definition record =
  match record.aggregate_owner with
  | Some
      ( owner_namespace,
        publication,
        Some { aggregate_item = Ast.Aggregate_definition original; _ } )
    when owner_namespace == namespace
         && record.table == table && original == definition
         && Option.is_none record.aggregate_stamp
         && Declaration_collection.namespace_owns_table namespace table
         && Declaration_collection.namespace_owns_publication namespace
              publication ->
      Ok { metadata_record = record; metadata_definition = original }
  | _ -> Error "aggregate value header requires its original completed source"

let aggregate_value_header_source ~table ~scope metadata =
  if
    not
      (inherited_metadata_owns_definition ~table ~scope
         metadata.metadata_definition metadata)
  then None
  else
    match metadata.metadata_record.aggregate_owner with
    | Some (_, publication, Some _) ->
        Option.map
          (fun symbol -> (metadata.metadata_definition, symbol))
          (Declaration_collection.publication_aggregate_identity publication)
    | _ -> None

let retain_aggregate_import = retain_aggregate_value_header

let aggregate_import_source ~table ~scope metadata =
  match aggregate_value_header_source ~table ~scope metadata with
  | None -> None
  | Some (definition, _) -> (
      match metadata.metadata_record.aggregate_owner with
      | Some (_, publication, Some _) ->
          Some (definition, publication, metadata.metadata_record.byte_size)
      | _ -> None)

let inherited_metadata_storage_selection ~table ~scope metadata =
  let record = metadata.metadata_record in
  let canonical record publication =
    Option.value
      (Declaration_collection.publication_aggregate_identity publication)
      ~default:record.symbol
  in
  if
    not
      (inherited_metadata_owns_definition ~table ~scope
         metadata.metadata_definition metadata)
  then None
  else
    match (record.aggregate_owner, record.aggregate_base_snapshot) with
    | Some (namespace, publication, _), Some base -> (
        match base.aggregate_owner with
        | Some
            ( base_namespace,
              base_publication,
              Some { aggregate_item = Ast.Aggregate_definition definition; _ }
            )
          when base.table == table
               && base_namespace == namespace
               && Option.is_none base.aggregate_stamp ->
            Some
              ( metadata.metadata_definition,
                canonical record publication,
                record.byte_size,
                definition,
                canonical base base_publication,
                base.byte_size )
        | _ -> None)
    | _ -> None

let rebind_primitive ~table ~symbol record =
  if
    Option.is_none record.primitive
    || (not (Symbol_table.owns_symbol table symbol))
    || Symbol.kind symbol <> Symbol.kind record.symbol
    || Symbol.name symbol <> Symbol.name record.symbol
    || Symbol.origin symbol <> Symbol.origin record.symbol
  then Error "forked primitive compiler record has a foreign symbol"
  else Ok { record with table; symbol }

let sizeof_class_of_type type_ =
  if Type.pointer_depth type_ > 0 then Pointer_class else Declared_class type_

let select_global_aggregate ~table ~namespace ~publication ~selected_aggregate
    (record : t) =
  let* source =
    match Declaration_collection.publication_source_global publication with
    | Some source
      when Parser.global_publication_is_current source
           && Declaration_collection.namespace_owns_table namespace table
           && Declaration_collection.namespace_owns_publication namespace
                publication -> Ok source
    | _ ->
        Error "global class allocation requires its original live publication"
  in
  let* () =
    Source_type_reference.validate_selected_aggregate ~table ~namespace
      selected_aggregate
  in
  let* type_ =
    Source_type_reference.selected_header_class selected_aggregate
      source.global_header.type_specifier source.global_pointer_layers
  in
  if
    Type.pointer_depth (Type_reference.resolved_type type_) <> 0
    || Option.is_some source.global_function_pointer
  then Error "global class allocation requires an object value"
  else
    match record.aggregate_owner with
    | Some (owner, selected, _)
      when owner == namespace && record.table == table
           && record.symbol
              == Declaration_collection.publication_symbol selected
           && aggregate_snapshot_is_current record
           && Declaration_collection.namespace_owns_publication namespace
                selected
           && Option.fold ~none:false ~some:(( == ) selected)
                (Declaration_collection.current_aggregate_publication namespace
                   selected)
           && Option.fold ~none:false
                ~some:
                  (( == )
                     (Source_type_reference.selected_base_symbol
                        selected_aggregate))
                (Declaration_collection.publication_aggregate_identity selected)
           && Option.fold ~none:false
                ~some:(fun original ->
                  original.Parser.aggregate_entry == record.entry
                  && original.aggregate_environment == source.global_environment)
                (Declaration_collection.publication_source_aggregate selected)
      ->
        Ok
          {
            global_object_table = table;
            global_object_namespace = namespace;
            global_object_publication = publication;
            global_object_source = source;
            global_object_type = type_;
            global_object_record = record;
          }
    | _ ->
        Error "global class allocation lacks its exact selected class snapshot"

let global_aggregate_record allocation = allocation.global_object_record
let global_aggregate_size allocation = allocation.global_object_record.byte_size

let global_aggregate_type allocation =
  Type_reference.resolved_type allocation.global_object_type

let published_scalar ?aggregate_allocation ?(dimensions = []) ~table ~namespace
    publication =
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
        let* byte_size, object_class =
          match source.global_function_pointer with
          | Some _ ->
              Ok (Int64.of_int Primitive_type.pointer_byte_size, Pointer_class)
          | None -> (
              match aggregate_allocation with
              | Some allocation
                when allocation.global_object_table == table
                     && allocation.global_object_namespace == namespace
                     && allocation.global_object_publication == publication
                     && allocation.global_object_source == source ->
                  Ok
                    ( global_aggregate_size allocation,
                      Declared_class (global_aggregate_type allocation) )
              | Some _ ->
                  Error
                    "global allocation snapshot belongs to another original \
                     object"
              | None ->
                  let* type_reference =
                    Source_type_reference.builtin
                      source.global_header.type_specifier
                      source.global_pointer_layers
                  in
                  let type_ = Type_reference.resolved_type type_reference in
                  let* byte_size = scalar_size type_ in
                  Ok (byte_size, sizeof_class_of_type type_))
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
            object_class = Some object_class;
            byte_size;
            internal = false;
            runtime_dimensions =
              Option.fold ~none:[]
                ~some:(fun allocation ->
                  allocation.global_object_record.runtime_dimensions)
                aggregate_allocation
              @ List.concat_map dimension_runtime_dependencies dimensions;
            runtime_offsets =
              Option.fold ~none:[]
                ~some:(fun allocation ->
                  allocation.global_object_record.runtime_offsets)
                aggregate_allocation
              @ List.concat_map dimension_offset_dependencies dimensions;
            aggregate_owner = None;
            aggregate_stamp = None;
            aggregate_base_snapshot = None;
            aggregate_fields = None;
            aggregate_initializer_backing = None;
            aggregate_initializer_body = None;
            aggregate_initializer_records = None;
          }

let declare_global ?aggregate_allocation ?callback
    ?(selected_aggregate : Function_type_resolution.selected_aggregate_resolver =
      fun _ -> None) ~dimensions ~table ~namespace ~predecessor ~previous_global
    publication =
  let* _ =
    published_scalar ?aggregate_allocation ~dimensions ~table ~namespace
      publication
  in
  let source =
    Option.get (Declaration_collection.publication_source_global publication)
  in
  if
    Option.is_some source.global_header.binding
    || List.exists
         (fun (modifier : Ast.declaration_modifier) ->
           modifier.kind <> Ast.Public
           && (Option.is_none source.global_function_pointer
              || modifier.kind <> Ast.Argument_pop
                 && modifier.kind <> Ast.No_argument_pop
                 && modifier.kind <> Ast.Has_error_code))
         source.global_header.modifiers
  then Error "partial storage requires an ordinary code-heap definition"
  else
    let* declared_callback =
      match (source.global_function_pointer, callback) with
      | None, None -> Ok None
      | Some original, Some (header, pointer)
        when header.Parser.callback_pointer == original
             && header.callback_signature_publication.callback_command
                == source.global_header.declaration_command
             && Option.fold ~none:false ~some:(( == ) original)
                  (Function_type_resolution.function_pointer_source pointer) ->
          let* _ = Source_type_reference.callback_storage ~header original in
          let* () =
            Function_type_resolution.validate_source_callback_types ~table
              ~namespace ~selected_aggregate pointer
          in
          Ok (Some (header, pointer))
      | _ ->
          Error
            "declared callback storage lacks its exact completed source header"
    in
    let* declared_type =
      match declared_callback with
      | Some (header, _) ->
          Source_type_reference.callback_return ~table ~namespace
            ~selected_aggregate ~header source.global_header.type_specifier
            source.global_pointer_layers
      | None -> (
          match aggregate_allocation with
          | Some allocation -> Ok allocation.global_object_type
          | None ->
              Source_type_reference.builtin source.global_header.type_specifier
                source.global_pointer_layers)
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
        declared_callback;
        declared_dimensions = dimensions;
        declared_aggregate = aggregate_allocation;
        declared_completion = None;
      }

let declared_global_symbol declaration =
  Declaration_collection.publication_symbol declaration.declared_publication

let declared_global_source declaration = declaration.declared_source
let declared_global_type declaration = declaration.declared_type
let declared_global_aggregate declaration = declaration.declared_aggregate

let declared_global_callback_pointer declaration =
  Option.map snd declaration.declared_callback

let declared_global_storage_type declaration =
  match declaration.declared_source.global_function_pointer with
  | None -> Ok (Type_reference.resolved_type declaration.declared_type)
  | Some pointer when List.length pointer.Ast.indirection_layers = 1 ->
      Type.make_primitive ~form:Type.Internal_storage
        ~primitive:Primitive_type.I64 ~pointer_depth:1
  | Some _ ->
      Error "declared callback storage requires one original indirection layer"

let declared_global_dimensions declaration =
  List.map dimension_count declaration.declared_dimensions

let declared_dimension_preparation dimension = dimension.prepared

let declared_global_runtime_dependencies declaration =
  Option.fold ~none:[]
    ~some:(fun allocation -> allocation.global_object_record.runtime_dimensions)
    declaration.declared_aggregate
  @ List.concat_map
      (fun dimension -> dimension.prepared.runtime_dependencies)
      declaration.declared_dimensions

let declared_global_offset_dependencies declaration =
  Option.fold ~none:[]
    ~some:(fun allocation -> allocation.global_object_record.runtime_offsets)
    declaration.declared_aggregate
  @ List.concat_map
      (fun dimension -> dimension.prepared.offset_dependencies)
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
    || (not
          (match
             ( declaration.declared_source.global_function_pointer,
               Global.global_declarator_kind global )
           with
          | None, Global.Object -> true
          | Some original, Global.Function_pointer pointer ->
              Option.fold ~none:false ~some:(( == ) original)
                (Function_type_resolution.function_pointer_source pointer)
          | _ -> false))
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

let retained_global_class global =
  match Global_type_resolution.global_declarator_kind global with
  | Global_type_resolution.Function_pointer _ ->
      Ok (Int64.of_int Primitive_type.pointer_byte_size, Pointer_class)
  | Global_type_resolution.Object ->
      let type_ =
        Global_type_resolution.global_type_reference global
        |> Type_reference.resolved_type
      in
      let* byte_size = scalar_size type_ in
      Ok (byte_size, sizeof_class_of_type type_)

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
    let* byte_size, object_class = retained_global_class global in
    Ok
      {
        table;
        entry;
        symbol;
        primitive = None;
        object_class = Some object_class;
        byte_size;
        internal = false;
        runtime_dimensions = [];
        runtime_offsets = [];
        aggregate_owner = None;
        aggregate_stamp = None;
        aggregate_base_snapshot = None;
        aggregate_fields = None;
        aggregate_initializer_backing = None;
        aggregate_initializer_body = None;
        aggregate_initializer_records = None;
      }

let return_class_size ~table ~namespace ~type_ ~(aggregate : t option) =
  if not (Declaration_collection.namespace_owns_table namespace table) then
    Error "return class belongs to another semantic namespace"
  else if Type.pointer_depth type_ > 0 then
    Ok (Int64.of_int Primitive_type.pointer_byte_size)
  else
    match Type.base type_ with
    | Type.Primitive (_, primitive) ->
        Ok (Int64.of_int (Primitive_type.info primitive).byte_size)
    | Type.Aggregate symbol -> (
        match aggregate with
        | Some record
          when record.table == table
               && Symbol_table.owns_symbol table symbol
               && Option.fold ~none:true
                    ~some:(fun (stamp, version) ->
                      stamp.current_stamp == version)
                    record.aggregate_stamp
               && Option.fold ~none:false
                    ~some:(fun (owner, publication, _) ->
                      owner == namespace
                      && Declaration_collection.namespace_owns_publication
                           namespace publication
                      && record.symbol
                         == Declaration_collection.publication_symbol
                              publication
                      && Option.fold ~none:false ~some:(( == ) publication)
                           (Declaration_collection.current_aggregate_publication
                              namespace publication)
                      && Option.fold ~none:false ~some:(( == ) symbol)
                           (Declaration_collection
                            .publication_aggregate_identity publication))
                    record.aggregate_owner -> Ok record.byte_size
        | _ ->
            Error
              "return class size lacks its exact current retained aggregate \
               layout")

let primitive_sizeof_class ~internal primitive =
  let has_internal_spelling =
    Primitive_type.of_storage_spelling
      (Primitive_type.info primitive).storage_spelling
    = Some primitive
  in
  let form =
    if internal && has_internal_spelling then Type.Internal_storage
    else Type.Public_spelling
  in
  Type.make_primitive ~form ~primitive ~pointer_depth:0
  |> Result.map sizeof_class_of_type

let sizeof_record_class record =
  match record.aggregate_owner with
  | Some (_, publication, _) -> (
      match
        Declaration_collection.publication_aggregate_identity publication
      with
      | Some symbol ->
          Type.make_aggregate ~symbol ~pointer_depth:0
          |> Result.map sizeof_class_of_type
      | None -> Error "sizeof class lacks its original canonical identity")
  | None -> (
      match (record.primitive, record.object_class) with
      | Some primitive, _ ->
          primitive_sizeof_class ~internal:record.internal primitive
      | None, Some class_ -> Ok class_
      | None, None -> Ok Unavailable_class)

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
      when entry == record.entry ->
        let* root_class = sizeof_record_class record in
        Ok { owner = Hash_record record; root; root_class; members_rev = [] }
    | _ -> Error "sizeof read does not select this exact compiler record"

let read_local_sizeof ~dimensions ~aggregate ~table ~namespace
    ~function_publication ~(root : Parser.query_root) =
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
          match function_pointer with
          | Some (pointer : Ast.function_pointer_declarator) ->
              let* _ = Source_type_reference.pointer_depth pointer_layers in
              let* depth =
                Source_type_reference.pointer_depth pointer.indirection_layers
              in
              if depth = 0 then
                Error "selected function pointer lacks original indirection"
              else
                (* PrsType (PrsVar.HC:350-356) assigns the RT_PTR companion
                 class, independently of the signature's return type. The
                 original declarator is retained by this local publication. *)
                Ok
                  ( Int64.of_int Primitive_type.pointer_byte_size,
                    false,
                    None,
                    Pointer_class )
          | None -> (
              let* pointer_depth =
                Source_type_reference.pointer_depth pointer_layers
              in
              if pointer_depth > 0 then
                (* PrsType (PrsVar.HC:298-308) advances an ordinary pointer
                   declaration to its pointer companion while consuming the
                   original '*' children. sizeof therefore needs no pointee
                   layout or value load for this retained type query. *)
                Ok
                  ( Int64.of_int Primitive_type.pointer_byte_size,
                    false,
                    None,
                    Pointer_class )
              else
                match type_specifier with
                | Ast.Named_type_specifier _ -> (
                    if not (Parser.query_root_is_current root) then
                      Error
                        "local class sizeof requires its original live query"
                    else
                      match aggregate with
                      | None ->
                          Error
                            "local class sizeof lacks its selected aggregate \
                             layout"
                      | Some (selected, record) ->
                          let* () =
                            Source_type_reference.validate_selected_aggregate
                              ~table ~namespace selected
                          in
                          let* reference =
                            Source_type_reference.selected_header_class selected
                              type_specifier pointer_layers
                          in
                          let* byte_size =
                            return_class_size ~table ~namespace
                              ~type_:(Type_reference.resolved_type reference)
                              ~aggregate:(Some record)
                          in
                          (* PrsSizeOf reads member_class->size at this query,
                             independently of the earlier local allocation or
                             argument slot. The saved value remains immutable. *)
                          Ok
                            ( byte_size,
                              false,
                              Some record,
                              sizeof_class_of_type
                                (Type_reference.resolved_type reference) ))
                | Ast.Primitive_type_specifier _ | Ast.Internal_type_specifier _
                  ->
                    let* type_reference =
                      Source_type_reference.builtin type_specifier
                        pointer_layers
                    in
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
                    Ok (byte_size, internal, None, sizeof_class_of_type type_))
        in
        let* byte_size, internal, aggregate_record, root_class =
          match source.local_source with
          | Parser.Local_parameter parameter ->
              declared_size parameter.type_specifier parameter.pointer_layers
                parameter.function_pointer
          | Parser.Local_variable local ->
              let* byte_size, internal, aggregate_record, root_class =
                declared_size local.local_type_specifier
                  local.local_pointer_layers local.local_function_pointer
              in
              let* byte_size =
                declared_array_size ~table ~namespace
                  ~command:source.local_command ~name:local.local_name
                  ~dimensions:local.local_array_dimensions ~checked:dimensions
                  byte_size
              in
              Ok (byte_size, internal, aggregate_record, root_class)
          | Parser.Variadic_count _ ->
              let* root_class =
                primitive_sizeof_class ~internal:true Primitive_type.I64
              in
              Ok
                ( Int64.of_int (Primitive_type.info Primitive_type.I64).byte_size,
                  true,
                  None,
                  root_class )
          | Parser.Variadic_vector _ ->
              (* PrsVar.HC:392-403 gives argv an internal I64 class and a
               127-element extent, independently of its eight-byte slot. *)
              let* root_class =
                primitive_sizeof_class ~internal:true Primitive_type.I64
              in
              Ok
                ( Int64.of_int
                    (127 * (Primitive_type.info Primitive_type.I64).byte_size),
                  true,
                  None,
                  root_class )
        in
        Ok
          {
            owner =
              Local_record
                {
                  table;
                  namespace;
                  source;
                  byte_size;
                  internal;
                  runtime_dimensions =
                    merge_dimension_dependencies
                      (Option.fold ~none:[]
                         ~some:(fun (record : t) -> record.runtime_dimensions)
                         aggregate_record)
                      (List.concat_map dimension_runtime_dependencies dimensions);
                  runtime_offsets =
                    merge_offset_dependencies
                      (Option.fold ~none:[]
                         ~some:(fun (record : t) -> record.runtime_offsets)
                         aggregate_record)
                      (List.concat_map dimension_offset_dependencies dimensions);
                };
            root;
            root_class;
            members_rev = [];
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
    | Ast.Sizeof_expression expression ->
        let members =
          List.rev_map (fun member -> member.member_source) read.members_rev
        in
        if
          List.length members = List.length receipt.query_members
          && List.for_all2 ( == ) members receipt.query_members
          && List.length members = List.length expression.sizeof_members
          && List.for_all2
               (fun member source ->
                 match member.Parser.query_member_node with
                 | Parser.Sizeof_member original -> original == source
                 | Parser.Offset_member _ -> false)
               members expression.sizeof_members
        then Ok ()
        else Error "sizeof completion lacks its original ordered member reads"
    | _ -> Error "sizeof requires its original checked member reads"

let sizeof_value read ~pointer =
  if pointer then Int64.of_int Primitive_type.pointer_byte_size
  else
    match read.members_rev with
    | member :: _ -> member.member_byte_size
    | [] -> (
        match read.owner with
        | Hash_record record -> record.byte_size
        | Local_record local -> local.byte_size)

let sizeof_selected_class read =
  match read.members_rev with
  | member :: _ -> member.member_class
  | [] -> read.root_class

let sizeof_primitive read =
  match read.members_rev with
  | { member_class = Declared_class type_; _ } :: _ -> (
      match Type.base type_ with
      | Type.Primitive (_, primitive) -> Some primitive
      | Type.Aggregate _ -> None)
  | _ :: _ -> None
  | [] -> (
      match read.owner with
      | Hash_record record -> record.primitive
      | Local_record _ -> None)

let sizeof_is_internal read =
  match read.members_rev with
  | { member_class = Declared_class type_; _ } :: _ -> (
      match Type.base type_ with
      | Type.Primitive (Type.Internal_storage, _) -> true
      | Type.Primitive (Type.Public_spelling, primitive) ->
          (Primitive_type.info primitive).declaration_form
          = Primitive_type.Internal_type
      | Type.Aggregate _ -> false)
  | _ :: _ -> false
  | [] -> (
      match read.owner with
      | Hash_record record -> record.internal
      | Local_record local -> local.internal)

let sizeof_runtime_dependencies read =
  match read.members_rev with
  | member :: _ -> member.member_dimensions
  | [] -> (
      match read.owner with
      | Hash_record record -> record.runtime_dimensions
      | Local_record local -> local.runtime_dimensions)

let sizeof_runtime_offsets read =
  match read.members_rev with
  | member :: _ -> member.member_offsets
  | [] -> (
      match read.owner with
      | Hash_record record -> record.runtime_offsets
      | Local_record local -> local.runtime_offsets)

let start_sizeof_member ~table ~namespace ~(source : Parser.query_member_start)
    read =
  let owner_namespace =
    match read.owner with
    | Hash_record record ->
        Option.map (fun (namespace, _, _) -> namespace) record.aggregate_owner
    | Local_record local -> Some local.namespace
  in
  if
    sizeof_table read != table
    || (not (Declaration_collection.namespace_owns_table namespace table))
    || Option.fold ~none:false ~some:(( != ) namespace) owner_namespace
    || source.member_start_root != read.root
    || source.member_start_ordinal <> List.length read.members_rev
    || not (Parser.query_member_start_is_current source)
  then Error "sizeof dot has foreign, expired or out-of-order source evidence"
  else if sizeof_is_internal read then
    Error "sizeof internal type cannot select a member"
  else
    Ok { start_source = source; start_namespace = namespace; start_read = read }

let read_sizeof_member ~table ~namespace ~aggregates
    ~(source : Parser.query_member) start =
  let read = start.start_read in
  if
    sizeof_table read != table
    || start.start_namespace != namespace
    || (not (Declaration_collection.namespace_owns_table namespace table))
    || source.query_member_root != read.root
    || source.query_member_start != start.start_source
    || source.query_member_ordinal <> List.length read.members_rev
    || not (Parser.query_member_is_current source)
  then
    Error "sizeof member has foreign, expired or out-of-order source evidence"
  else
    let* name =
      match source.query_member_node with
      | Parser.Sizeof_member member
        when member.sizeof_member_dot == start.start_source.member_start_dot ->
          Ok member.sizeof_member_name.spelling
      | _ -> Error "sizeof member does not retain its original dot and name"
    in
    let current_aggregate type_ symbol =
      match aggregates symbol with
      | None -> Error "sizeof member lacks its selected current class record"
      | Some record ->
          let* _ =
            return_class_size ~table ~namespace ~type_ ~aggregate:(Some record)
          in
          Ok record
    in
    let rec find_member visited type_ =
      match Type.base type_ with
      | Type.Primitive (Type.Internal_storage, _) ->
          Error "sizeof internal type has no members"
      | Type.Primitive (Type.Public_spelling, primitive) -> (
          match Primitive_type.public_union_members primitive with
          | None -> Error "sizeof internal type has no members"
          | Some members ->
              let* field =
                match
                  List.find_opt
                    (fun (field :
                           Generated.Primitive_raw_types.public_union_member) ->
                      field.name = name)
                    members
                with
                | Some field -> Ok field
                | None ->
                    Error ("sizeof class has no member named `" ^ name ^ "`")
              in
              (* These spellings come from the pinned member declarations, not
                 the current source environment. Later type replacements cannot
                 change the class selected when the builtin union was seeded. *)
              let form, primitive =
                match
                  Primitive_type.of_storage_spelling field.type_spelling
                with
                | Some primitive -> (Type.Internal_storage, Some primitive)
                | None ->
                    ( Type.Public_spelling,
                      Primitive_type.of_spelling field.type_spelling )
              in
              let* primitive =
                match primitive with
                | Some primitive -> Ok primitive
                | None -> Error "sizeof builtin member lacks its pinned class"
              in
              let* type_ =
                Type.make_primitive ~form ~primitive ~pointer_depth:0
              in
              Ok ((type_, Int64.of_int field.array_count, [], []), []))
      | Type.Aggregate symbol -> (
          if List.exists (( == ) symbol) visited then
            Error "sizeof member lookup reached a repeated inherited class"
          else
            let* record = current_aggregate type_ symbol in
            let* fields =
              match record.aggregate_fields with
              | Some fields -> Ok fields
              | None ->
                  Error "sizeof class lacks its original member layout phases"
            in
            let field =
              (* MemberAdd leaves an earlier pad/reserved/_anon_ match above
                 later duplicates. These phase entries are stored newest first. *)
              List.fold_left
                (fun found field ->
                  match field.field_phase.phase_step with
                  | Parser.Aggregate_member_prepared original
                    when original.member_name.spelling = name -> Some field
                  | _ -> found)
                None fields
            in
            match field with
            | Some field ->
                Ok
                  ( ( field.field_type,
                      field.field_count,
                      field.field_dimensions,
                      field.field_offsets ),
                    [ record ] )
            | None -> (
                match record.aggregate_base_snapshot with
                | None ->
                    Error ("sizeof class has no member named `" ^ name ^ "`")
                | Some base -> (
                    let* base_class = sizeof_record_class base in
                    match base_class with
                    | Declared_class type_ ->
                        let* field, records =
                          find_member (symbol :: visited) type_
                        in
                        Ok (field, record :: records)
                    | Pointer_class | Unavailable_class ->
                        Error "sizeof base lacks its selected class identity")))
    in
    let* (field_type, field_count, field_dimensions, field_offsets), records =
      match sizeof_selected_class read with
      | Declared_class type_ -> find_member [] type_
      | Pointer_class -> Error "sizeof pointer class has no members"
      | Unavailable_class ->
          Error "sizeof member requires its original selected class metadata"
    in
    let* element_size, selected_record =
      if Type.pointer_depth field_type > 0 then
        Ok (Int64.of_int Primitive_type.pointer_byte_size, None)
      else
        match Type.base field_type with
        | Type.Primitive _ ->
            let* size = scalar_size field_type in
            Ok (size, None)
        | Type.Aggregate symbol ->
            let* record = current_aggregate field_type symbol in
            Ok (record.byte_size, Some record)
    in
    let records = Option.to_list selected_record @ records in
    let member =
      {
        member_source = source;
        member_class = sizeof_class_of_type field_type;
        (* PrsSizeOf reads the live selected class size, not the extent saved
           when this member was placed in its containing object. *)
        member_byte_size = Int64.mul element_size field_count;
        member_dimensions =
          merge_dimension_dependencies
            (sizeof_runtime_dependencies read)
            (field_dimensions
            @ List.concat_map (fun record -> record.runtime_dimensions) records
            );
        member_offsets =
          merge_offset_dependencies
            (sizeof_runtime_offsets read)
            (field_offsets
            @ List.concat_map (fun record -> record.runtime_offsets) records);
      }
    in
    Ok { read with members_rev = member :: read.members_rev }

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
  | Some read -> sizeof_runtime_dependencies read

let query_runtime_offsets selection =
  match selection.sizeof_read with
  | None -> []
  | Some read -> sizeof_runtime_offsets read

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

let aggregate_offset_is_current ~table ~namespace progress
    (phase : Parser.aggregate_phase) =
  progress.progress_namespace == namespace
  && Declaration_collection.namespace_owns_table namespace table
  && (not progress.progress_finished)
  && (not progress.progress_body_finished)
  && phase.phase_aggregate == progress.progress_source
  && Parser.aggregate_phase_is_current phase
  && same_phase phase.phase_predecessor progress.progress_phase
  && (not (same_phase (Some phase) progress.progress_offset_attempt))
  &&
  match phase.phase_step with
  | Parser.Aggregate_offset_reached _ -> true
  | _ -> false

let member_metadata_is_current ~table ~namespace progress
    (phase : Parser.aggregate_phase) =
  progress.progress_namespace == namespace
  && Declaration_collection.namespace_owns_table namespace table
  && (not progress.progress_finished)
  && (not progress.progress_body_finished)
  && phase.phase_aggregate == progress.progress_source
  && Parser.aggregate_phase_is_current phase
  && same_phase phase.phase_predecessor progress.progress_phase
  && (not (same_phase (Some phase) progress.progress_metadata_attempt))
  &&
  match phase.phase_step with
  | Parser.Aggregate_member_metadata_reached _ -> true
  | _ -> false

let resolve_position_reads progress phase =
  let rec collect rev = function
    | [] -> Ok (List.rev rev)
    | (expression, Some source) :: rest -> (
        match
          Position_sources.find_opt
            progress.progress_compiler_positions.positions source
        with
        | Some (Some position) when position.position_source == source ->
            collect ((expression, position) :: rev) rest
        | Some None ->
            Error
              "aggregate position requires original function/frame \
               compiler-state writes"
        | _ ->
            Error
              "aggregate position lacks its original shared compiler-state \
               write")
    | (_, None) :: _ ->
        Error
          "aggregate position requires original function/frame compiler-state \
           writes"
  in
  collect [] phase.Parser.phase_position_reads

let offset_dimension_dependencies record queries positions =
  merge_dimension_dependencies record.runtime_dimensions
    (List.concat_map query_runtime_dependencies queries
    @ List.concat_map (fun (_, p) -> p.position_dimensions) positions)

let offset_input_dependencies record queries positions =
  merge_offset_dependencies record.runtime_offsets
    (List.concat_map query_runtime_offsets queries
    @ List.concat_map (fun (_, p) -> p.position_dependencies) positions)

let aggregate_offset_dimension_dependencies ~table ~namespace ~queries progress
    phase =
  if not (aggregate_offset_is_current ~table ~namespace progress phase) then
    Error
      "aggregate dependencies require their original unconsumed offset phase"
  else
    let* record = progress.progress_record in
    let* positions = resolve_position_reads progress phase in
    Ok (offset_dimension_dependencies record queries positions)

let aggregate_offset_dependencies ~table ~namespace ~queries progress phase =
  if not (aggregate_offset_is_current ~table ~namespace progress phase) then
    Error
      "aggregate dependencies require their original unconsumed offset phase"
  else
    let* record = progress.progress_record in
    let* positions = resolve_position_reads progress phase in
    Ok (offset_input_dependencies record queries positions)

let member_metadata_dependencies ~table ~namespace ~queries progress phase =
  if not (member_metadata_is_current ~table ~namespace progress phase) then
    Error "metadata dependencies require their original unconsumed member phase"
  else
    let* record = progress.progress_record in
    let* positions = resolve_position_reads progress phase in
    let* () =
      match phase.phase_step with
      | Parser.Aggregate_member_metadata_reached original -> (
          match original.metadata.member_metadata_value with
          | Ast.Member_metadata_expression expression ->
              validate_query_manifest ~table ~expression queries
          | Ast.Member_metadata_string _ when queries = [] -> Ok ()
          | _ -> Error "string metadata has foreign query reads")
      | _ -> assert false
    in
    Ok
      ( offset_dimension_dependencies record queries positions,
        offset_input_dependencies record queries positions )

let member_metadata_positions ~table ~namespace progress phase =
  if not (member_metadata_is_current ~table ~namespace progress phase) then
    Error "metadata positions require their original unconsumed member phase"
  else resolve_position_reads progress phase

let begin_runtime_member_metadata ~table ~namespace ~queries progress phase =
  let* dimensions, offsets =
    member_metadata_dependencies ~table ~namespace ~queries progress phase
  in
  let* record = progress.progress_record in
  match phase.Parser.phase_step with
  | Parser.Aggregate_member_metadata_reached original ->
      let* () =
        match
          (record.aggregate_fields, original.metadata.member_metadata_value)
        with
        | Some (field :: _), Ast.Member_metadata_expression _
          when aggregate_field_name field == original.member_name -> Ok ()
        | _ ->
            Error
              "runtime metadata requires its original placed member and \
               expression"
      in
      let* () =
        if
          List.for_all
            (fun query ->
              query.receipt.query_root.query_command
              == progress.progress_source.aggregate_header.declaration_command
              && query.receipt.query_root.query_environment
                 == progress.progress_source.aggregate_environment)
            queries
        then Ok ()
        else Error "runtime metadata has foreign source query reads"
      in
      progress.progress_metadata_attempt <- Some phase;
      Ok
        {
          runtime_metadata_progress = progress;
          runtime_metadata_phase = phase;
          runtime_metadata_table = table;
          runtime_metadata_source = original.metadata;
          runtime_metadata_finished = false;
          runtime_metadata_dimensions = dimensions;
          runtime_metadata_offsets = offsets;
        }
  | _ -> assert false

let runtime_member_metadata_is_current value =
  let progress = value.runtime_metadata_progress in
  (not value.runtime_metadata_finished)
  && (not progress.progress_finished)
  && (not progress.progress_body_finished)
  && Parser.aggregate_phase_is_current value.runtime_metadata_phase
  && same_phase progress.progress_phase
       value.runtime_metadata_phase.phase_predecessor
  && same_phase progress.progress_metadata_attempt
       (Some value.runtime_metadata_phase)

let finish_runtime_member_metadata preparation ~value ~work =
  if (not (runtime_member_metadata_is_current preparation)) || work < 0 then
    Error
      "runtime metadata completion is foreign, expired, repeated or has \
       invalid work"
  else (
    preparation.runtime_metadata_finished <- true;
    let progress = preparation.runtime_metadata_progress in
    let metadata =
      {
        metadata_table = preparation.runtime_metadata_table;
        metadata_namespace = progress.progress_namespace;
        metadata_phase = preparation.runtime_metadata_phase;
        metadata_source = preparation.runtime_metadata_source;
        metadata_value = Metadata_word value;
        metadata_work = work;
      }
    in
    progress.progress_metadata <- metadata :: progress.progress_metadata;
    Ok metadata)

let begin_runtime_aggregate_offset ~table ~namespace ~queries progress phase =
  if not (aggregate_offset_is_current ~table ~namespace progress phase) then
    Error "runtime offset requires its original unconsumed phase and aggregate"
  else (
    progress.progress_offset_attempt <- Some phase;
    let* record = progress.progress_record in
    let* positions = resolve_position_reads progress phase in
    match phase.Parser.phase_step with
    | Parser.Aggregate_offset_reached expression ->
        let* () = validate_query_manifest ~table ~expression queries in
        if
          List.exists
            (fun query ->
              query.receipt.query_root.query_command
              != progress.progress_source.aggregate_header.declaration_command
              || query.receipt.query_root.query_environment
                 != progress.progress_source.aggregate_environment)
            queries
        then Error "runtime offset has foreign source query reads"
        else
          Ok
            {
              runtime_progress = progress;
              runtime_phase = phase;
              runtime_table = table;
              runtime_expression = expression;
              runtime_finished = false;
              runtime_dimension_dependencies =
                offset_dimension_dependencies record queries positions;
              runtime_offset_dependencies =
                offset_input_dependencies record queries positions;
            }
    | _ -> Error "runtime offset requires its original expression phase")

let aggregate_offset_positions ~table ~namespace progress phase =
  if not (aggregate_offset_is_current ~table ~namespace progress phase) then
    Error "aggregate position requires its original unconsumed offset phase"
  else resolve_position_reads progress phase

let runtime_aggregate_offset_is_current preparation =
  let progress = preparation.runtime_progress in
  (not preparation.runtime_finished)
  && (not progress.progress_finished)
  && (not progress.progress_body_finished)
  && Parser.aggregate_phase_is_current preparation.runtime_phase
  && same_phase (Some preparation.runtime_phase)
       progress.progress_offset_attempt
  && same_phase preparation.runtime_phase.phase_predecessor
       progress.progress_phase

let finish_runtime_aggregate_offset preparation ~value ~work =
  if work < 0 || not (runtime_aggregate_offset_is_current preparation) then
    Error "runtime offset result has another, expired or consumed preparation"
  else (
    preparation.runtime_finished <- true;
    let progress = preparation.runtime_progress in
    let offset =
      {
        offset_table = preparation.runtime_table;
        offset_namespace = progress.progress_namespace;
        offset_phase = preparation.runtime_phase;
        offset_expression = preparation.runtime_expression;
        offset_value = value;
        offset_work = work;
        offset_runtime = true;
        offset_dimensions = preparation.runtime_dimension_dependencies;
        offset_dependencies = preparation.runtime_offset_dependencies;
      }
    in
    progress.progress_offsets <- offset :: progress.progress_offsets;
    Ok offset)

let prepare_aggregate_member_metadata ~table ~namespace ~max_work ~queries
    progress (phase : Parser.aggregate_phase) =
  let module Numeric = Closed_numeric_expression in
  let work = ref 0 in
  let result =
    if
      max_work < 0
      || not (member_metadata_is_current ~table ~namespace progress phase)
    then
      Error
        "member metadata preparation is foreign, expired, repeated or out of \
         order"
    else
      match phase.phase_step with
      | Parser.Aggregate_member_metadata_reached original ->
          let* record = progress.progress_record in
          let* () =
            match record.aggregate_fields with
            | Some (field :: _)
              when aggregate_field_name field == original.member_name -> Ok ()
            | _ -> Error "member metadata requires its original placed member"
          in
          progress.progress_metadata_attempt <- Some phase;
          let* metadata_value =
            match original.metadata.member_metadata_value with
            | Ast.Member_metadata_string string ->
                if queries = [] && phase.phase_position_reads = [] then
                  Ok (Metadata_string string.member_metadata_string_value)
                else Error "string metadata has foreign expression reads"
            | Ast.Member_metadata_expression expression ->
                let* () = validate_query_manifest ~table ~expression queries in
                let* () =
                  if
                    List.for_all
                      (fun query ->
                        query.receipt.query_root.query_command
                        == progress.progress_source.aggregate_header
                             .declaration_command
                        && query.receipt.query_root.query_environment
                           == progress.progress_source.aggregate_environment)
                      queries
                  then Ok ()
                  else
                    Error
                      "member metadata queries have another original command"
                in
                let* positions = resolve_position_reads progress phase in
                let numeric =
                  Numeric.of_ast ~allow_floating:true
                    ~position_value:(fun source ->
                      List.find_opt (fun (node, _) -> node == source) positions
                      |> Option.map (fun (_, position) ->
                          position.position_value))
                    ~query_expression ~queries expression
                in
                let consume () =
                  if !work >= max_work then
                    Error
                      (Numeric.make_error "HCIRVM0007"
                         (Numeric.Invalid_input
                            "member metadata preparation work limit")
                         "the bounded member metadata preparation work limit \
                          was exhausted")
                  else (
                    incr work;
                    Ok ())
                in
                Numeric.evaluate_expression ~consume
                  ~query_origin:(fun query -> query.origin)
                  ~query_value:query_constant ~context:Numeric.Member_metadata
                  ~current_position:record.byte_size numeric
                |> Result.map (fun value -> Metadata_word value)
                |> Result.map_error Numeric.error_to_string
          in
          let metadata =
            {
              metadata_table = table;
              metadata_namespace = namespace;
              metadata_phase = phase;
              metadata_source = original.metadata;
              metadata_value;
              metadata_work = !work;
            }
          in
          progress.progress_metadata <- metadata :: progress.progress_metadata;
          Ok metadata
      | _ -> Error "member metadata requires its original parser phase"
  in
  (result, !work)

let prepare_aggregate_offset ~table ~namespace ~max_work ~queries progress
    (phase : Parser.aggregate_phase) =
  let module Numeric = Closed_numeric_expression in
  let work = ref 0 in
  let result =
    if
      (not (aggregate_offset_is_current ~table ~namespace progress phase))
      || max_work < 0
    then
      Error
        "aggregate offset preparation is foreign, expired, repeated or out of \
         order"
    else
      match phase.phase_step with
      | Parser.Aggregate_offset_reached expression ->
          progress.progress_offset_attempt <- Some phase;
          let* record = progress.progress_record in
          let* positions = resolve_position_reads progress phase in
          let* () = validate_query_manifest ~table ~expression queries in
          let* () =
            if
              List.for_all
                (fun query ->
                  query.receipt.query_root.query_command
                  == progress.progress_source.aggregate_header
                       .declaration_command
                  && query.receipt.query_root.query_environment
                     == progress.progress_source.aggregate_environment)
                queries
            then Ok ()
            else
              Error
                "aggregate offset queries require their original closed \
                 command reads"
          in
          let current_position =
            match progress.progress_scopes with
            | (Ast.Union_aggregate, base) :: _ -> base
            | _ -> record.byte_size
          in
          let numeric =
            Numeric.of_ast ~allow_floating:true
              ~position_value:(fun source ->
                List.find_opt
                  (fun (original, _) -> original == source)
                  positions
                |> Option.map (fun (_, position) -> position.position_value))
              ~query_expression ~queries expression
          in
          let consume () =
            if !work >= max_work then
              Error
                (Numeric.make_error "HCIRVM0007"
                   (Numeric.Invalid_input
                      "aggregate offset preparation work limit")
                   "the bounded aggregate offset preparation work limit was \
                    exhausted")
            else (
              incr work;
              Ok ())
          in
          let* value =
            Numeric.evaluate_expression ~consume
              ~query_origin:(fun query -> query.origin)
              ~query_value:query_constant ~context:Numeric.Aggregate_offset
              ~current_position numeric
            |> Result.map_error Numeric.error_to_string
          in
          let offset =
            {
              offset_table = table;
              offset_namespace = namespace;
              offset_phase = phase;
              offset_expression = expression;
              offset_value = value;
              offset_work = !work;
              offset_runtime = false;
              offset_dimensions =
                offset_dimension_dependencies record queries positions;
              offset_dependencies =
                offset_input_dependencies record queries positions;
            }
          in
          progress.progress_offsets <- offset :: progress.progress_offsets;
          Ok offset
      | _ ->
          Error
            "aggregate offset preparation requires its original expression \
             phase"
  in
  (result, !work)

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
              | Numeric.Current_position_expression _
              | Numeric.Captured_position_expression _ ->
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
            merge_dimension_dependencies []
              (List.concat_map query_runtime_dependencies queries);
          offset_dependencies = List.concat_map query_runtime_offsets queries;
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
        merge_dimension_dependencies [ proposal ]
          (List.concat_map query_runtime_dependencies queries);
      offset_dependencies = List.concat_map query_runtime_offsets queries;
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
    | Numeric.Current_position_expression _
    | Numeric.Captured_position_expression _ ->
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

let global_extent_offset_dependencies extent =
  List.concat_map
    (fun dimension ->
      match dimension.extent_evaluation with
      | Prepared_extent prepared -> dimension_offset_dependencies prepared
      | Legacy_extent queries ->
          Option.fold ~none:[]
            ~some:(List.concat_map query_runtime_offsets)
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
      else
        let* byte_size, object_class = retained_global_class global in
        Ok
          {
            table;
            entry;
            symbol;
            primitive = None;
            object_class = Some object_class;
            byte_size = Int64.mul byte_size extent.global_extent_count;
            internal = false;
            runtime_dimensions = global_extent_runtime_dependencies extent;
            runtime_offsets = global_extent_offset_dependencies extent;
            aggregate_owner = None;
            aggregate_stamp = None;
            aggregate_base_snapshot = None;
            aggregate_fields = None;
            aggregate_initializer_backing = None;
            aggregate_initializer_body = None;
            aggregate_initializer_records = None;
          }

let record_callback_position positions ~parameters receipt =
  let module P = Frontend.Parser in
  let owner = receipt.P.callback_position_signature in
  let sources = P.context_sources owner.callback_command.command_context in
  let same a b =
    match (a, b) with
    | None, None -> true
    | Some a, Some b -> a == b
    | _ -> false
  in
  let rec valid index previous = function
    | [] -> same previous receipt.callback_position_predecessor
    | member :: rest ->
        let publication = member.P.callback_parameter_publication in
        publication.callback_parameter_signature == owner
        && publication.callback_parameter_index = index
        && same publication.callback_parameter_predecessor previous
        && valid (index + 1) (Some member) rest
  in
  if
    (not (compiler_positions_own_sources positions sources))
    || (not (P.callback_position_is_current receipt))
    || (not (valid 0 None parameters))
    || Position_sources.mem positions.positions receipt.callback_position_source
  then
    Error "anonymous position requires its original active ordered member write"
  else
    let count = Int64.of_int (List.length parameters) in
    if count > Int64.div Int64.max_int 8L then
      Error "anonymous fixed-member size exceeds the compiler word"
    else (
      Position_sources.add positions.positions receipt.callback_position_source
        (Some
           {
             position_source = receipt.callback_position_source;
             position_value = Int64.mul 8L count;
             position_dimensions = [];
             position_dependencies = [];
           });
      Ok ())

let resolve_default_position_reads positions ~sources reads =
  if not (compiler_positions_own_sources positions sources) then
    Error "default position belongs to another original source manager"
  else
    let rec collect rev = function
      | [] -> Ok (List.rev rev)
      | (expression, Some source) :: rest -> (
          match Position_sources.find_opt positions.positions source with
          | Some (Some position) when position.position_source == source ->
              collect ((expression, position) :: rev) rest
          | _ ->
              Error "default position lacks its original compiler-state write")
      | (_, None) :: _ ->
          Error "default position requires an original compiler-state write"
    in
    collect [] reads
