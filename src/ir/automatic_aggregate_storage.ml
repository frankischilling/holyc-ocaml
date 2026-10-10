module Frame = Sema.Function_frame_layout
module Type = Sema.Type

type t = { element_size : int; byte_size : int }

let byte_type =
  Type.make_primitive ~form:Type.Internal_storage
    ~primitive:Sema.Primitive_type.U8 ~pointer_depth:0
  |> Result.get_ok

let byte_scalar = Integer_scalar_storage.of_type byte_type |> Option.get
let element_size storage = storage.element_size
let byte_size storage = storage.byte_size

let aggregate_pointer type_ =
  Type.pointer_depth type_ = 1
  &&
  match Type.base type_ with
  | Type.Aggregate _ -> true
  | _ -> false

let of_location location =
  let type_ = Frame.location_checked_type location in
  let bytes = Frame.location_element_size location in
  let dimensions = Frame.location_dimensions location in
  let shape_matches =
    match (Frame.location_value_shape location, dimensions) with
    | Frame.Scalar, [] -> true
    | Frame.Array, _ :: _ -> true
    | _ -> false
  in
  let extent =
    List.fold_left
      (fun extent dimension ->
        Option.bind extent (fun bytes ->
            let count = Frame.dimension_value dimension in
            if
              Frame.dimension_kind dimension <> Frame.Source_extent
              || bytes <= 0L || count <= 0L
              || count > Int64.div (Int64.of_int Int.max_int) bytes
            then None
            else Some (Int64.mul bytes count)))
      (Some bytes) dimensions
  in
  match (Type.base type_, Frame.location_frame_slot location) with
  | Type.Aggregate _, Some slot
    when Type.pointer_depth type_ = 0
         && Frame.location_kind location = Frame.Automatic_local
         && Frame.location_declarator_shape location = Frame.Object
         && shape_matches
         && Frame.location_source_dimensions_checked location
         && Option.is_none (Frame.location_callback_pointer location)
         && bytes > 0L
         && bytes <= Int64.of_int Int.max_int
         && extent = Some (Frame.location_allocated_size location)
         && Frame.frame_slot_size slot = Frame.location_allocated_size location
    ->
      Some
        {
          element_size = Int64.to_int bytes;
          byte_size = Int64.to_int (Frame.frame_slot_size slot);
        }
  | _ -> None
