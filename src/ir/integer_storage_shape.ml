type t = {
  scalar : Integer_scalar_storage.t;
  dimensions : int64 list;
  strides : int64 list;
  element_count : int;
  byte_size : int;
  element_size : int;
  cell_count : int;
  aggregate_owner : (Sema.Symbol.t * Sema.Type.t) option;
  initializer_type : Sema.Initializer_type.t option;
}

type error = Unsupported_type | Invalid_extent | Overflow

let dimensions shape = shape.dimensions
let strides shape = shape.strides
let element_count shape = shape.element_count
let byte_size shape = shape.byte_size
let scalar shape = shape.scalar
let element_size shape = shape.element_size
let cell_count shape = shape.cell_count
let initializer_type shape = shape.initializer_type
let is_aggregate shape = Option.is_some shape.aggregate_owner

let matches shape ~symbol ~type_ ~dimensions =
  shape.dimensions = dimensions
  &&
  match shape.aggregate_owner with
  | None -> false
  | Some (owner, original) -> owner == symbol && Sema.Type.equal original type_

let padded_byte_size shape =
  if shape.byte_size > Int.max_int - 7 then None
  else Some ((shape.byte_size + 7) land lnot 7)

let create_with_scalar ~width ~scalar ~aggregate_owner ~initializer_type
    ~dimensions =
  if width < 0 || (width = 0 && Option.is_none aggregate_owner) then
    Error Unsupported_type
  else
    let ( let* ) = Result.bind in
    let limit = Int64.of_int Int.max_int in
    let rec collect = function
      | [] -> Ok (1L, Int64.of_int width, [])
      | count :: rest ->
          if count <= 0L then Error Invalid_extent
          else
            let* elements, bytes, strides = collect rest in
            if
              count > Int64.div limit elements
              || (bytes > 0L && count > Int64.div limit bytes)
            then Error Overflow
            else
              Ok
                ( Int64.mul count elements,
                  Int64.mul count bytes,
                  bytes :: strides )
    in
    let* elements, bytes, strides = collect dimensions in
    let cells = if Option.is_some aggregate_owner then bytes else elements in
    if cells > Int64.of_int Sys.max_array_length then Error Overflow
    else
      Ok
        {
          scalar;
          dimensions;
          strides;
          element_count = Int64.to_int elements;
          byte_size = Int64.to_int bytes;
          element_size = width;
          cell_count = Int64.to_int cells;
          aggregate_owner;
          initializer_type;
        }

let create ~type_ ~dimensions =
  match
    ( Integer_scalar_storage.public_byte_size type_,
      Integer_scalar_storage.of_type type_ )
  with
  | Some width, Some scalar ->
      create_with_scalar ~width ~scalar ~aggregate_owner:None
        ~initializer_type:
          (Sema.Initializer_type.of_type type_ |> Result.to_option)
        ~dimensions
  | _ -> Error Unsupported_type

let aggregate_shape ~initializer_type ~symbol ~type_ ~element_size ~dimensions =
  if element_size < 0L || element_size > Int64.of_int Int.max_int then
    Error Invalid_extent
  else
    create_with_scalar
      ~width:(Int64.to_int element_size)
      ~scalar:Automatic_aggregate_storage.byte_scalar
      ~aggregate_owner:(Some (symbol, type_))
      ~initializer_type ~dimensions

let create_initializer_aggregate ~allocation ~dimensions () =
  let type_ = Sema.Compiler_record.automatic_aggregate_type allocation in
  match Sema.Type.base type_ with
  | Sema.Type.Aggregate symbol when Sema.Type.pointer_depth type_ = 0 ->
      aggregate_shape ~symbol ~type_
        ~initializer_type:
          (Sema.Initializer_type.of_record ~type_
             (Sema.Compiler_record.automatic_aggregate_record allocation)
          |> Result.to_option)
        ~element_size:(Sema.Compiler_record.automatic_aggregate_size allocation)
        ~dimensions
  | _ -> Error Unsupported_type

let create_static_allocation ~allocation ~symbol ~dimensions =
  match Sema.Compiler_record.static_allocation_aggregate allocation with
  | Some aggregate ->
      let module Record = Sema.Compiler_record in
      let namespace = Record.static_allocation_namespace allocation in
      let type_ = Record.automatic_aggregate_type aggregate in
      let checked_dimensions =
        Record.static_allocation_dimensions allocation
        |> List.map Record.dimension_count
      in
      if
        checked_dimensions <> dimensions
        || Record.automatic_aggregate_source aggregate
           != Record.static_allocation_receipt allocation
        || Result.is_error
             (Record.validate_automatic_aggregate
                ~table:(Record.static_allocation_table allocation)
                ~parent:(Sema.Declaration_collection.namespace_scope namespace)
                ~function_symbol:
                  (Sema.Declaration_collection.publication_symbol
                     (Record.static_allocation_publication allocation))
                ~local_symbol:symbol
                ~local_name:(Record.automatic_aggregate_local_name aggregate)
                ~checked_type:type_ aggregate)
      then Error Unsupported_type
      else
        aggregate_shape ~symbol ~type_
          ~initializer_type:
            (Sema.Initializer_type.of_record ~type_
               (Record.automatic_aggregate_record aggregate)
            |> Result.to_option)
          ~element_size:(Record.automatic_aggregate_size aggregate)
          ~dimensions
  | None -> Error Unsupported_type

let create_declared_global declaration =
  match Sema.Compiler_record.declared_global_aggregate declaration with
  | Some allocation ->
      aggregate_shape
        ~symbol:(Sema.Compiler_record.declared_global_symbol declaration)
        ~type_:(Sema.Compiler_record.global_aggregate_type allocation)
        ~initializer_type:
          (Sema.Initializer_type.of_record
             ~type_:(Sema.Compiler_record.global_aggregate_type allocation)
             (Sema.Compiler_record.global_aggregate_record allocation)
          |> Result.to_option)
        ~element_size:(Sema.Compiler_record.global_aggregate_size allocation)
        ~dimensions:
          (Sema.Compiler_record.declared_global_dimensions declaration)
  | None -> (
      match Sema.Compiler_record.declared_global_storage_type declaration with
      | Ok type_ ->
          create ~type_
            ~dimensions:
              (Sema.Compiler_record.declared_global_dimensions declaration)
      | Error _ -> Error Unsupported_type)

let create_global ?headers ~members ~global ~dimensions () =
  let module Global = Sema.Global_type_resolution in
  let module Members = Sema.Aggregate_member_index in
  let type_ =
    Global.global_type_reference global |> Sema.Type_reference.resolved_type
  in
  let symbol = Global.global_symbol global in
  match Sema.Type.base type_ with
  | Sema.Type.Aggregate aggregate when Sema.Type.pointer_depth type_ = 0 -> (
      match Members.find_aggregate members aggregate with
      | Some selected
        when Members.owns_symbol members symbol
             && Members.aggregate_symbol selected == aggregate
             && Members.aggregate_item_index selected
                <= Global.global_item_index global
             && Global.global_declarator_kind global = Global.Object ->
          aggregate_shape ~symbol ~type_
            ~initializer_type:
              (Option.bind headers (fun headers ->
                   Sema.Initializer_type.of_members ~headers ~members
                     ~before_item_index:(Global.global_item_index global)
                     ~type_
                   |> Result.to_option))
            ~element_size:(Members.aggregate_size selected)
            ~dimensions
      | _ -> Error Unsupported_type)
  | _ -> create ~type_ ~dimensions

let create_static ~frame ~location =
  let module Frame = Sema.Function_frame_layout in
  let type_ = Frame.location_checked_type location in
  let dimensions =
    Frame.location_dimensions location |> List.map Frame.dimension_value
  in
  let symbol = Frame.location_symbol location in
  match Sema.Type.base type_ with
  | Sema.Type.Aggregate _ when Sema.Type.pointer_depth type_ = 0 ->
      if
        Frame.location_kind location <> Frame.Static_local
        || Frame.location_declarator_shape location <> Frame.Object
        || (not
              (Option.fold ~none:false ~some:(( == ) location)
                 (Frame.find_location frame symbol)))
        || (not (Frame.location_source_dimensions_checked location))
        || Option.is_some (Frame.location_frame_slot location)
      then Error Unsupported_type
      else
        let shape =
          aggregate_shape ~symbol ~type_
            ~initializer_type:
              (Option.bind (Frame.location_aggregate_allocation location)
                 (fun allocation ->
                   Sema.Initializer_type.of_record ~type_
                     (Sema.Compiler_record.automatic_aggregate_record allocation)
                   |> Result.to_option))
            ~element_size:(Frame.location_element_size location)
            ~dimensions
        in
        Result.bind shape (fun shape ->
            if
              Int64.of_int shape.byte_size
              = Frame.location_allocated_size location
            then Ok shape
            else Error Invalid_extent)
  | _ -> create ~type_ ~dimensions
