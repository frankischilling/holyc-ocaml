module Frame = Sema.Function_frame_layout
module Type = Sema.Type

type t = { byte_size : int }

let byte_type =
  Type.make_primitive ~form:Type.Internal_storage
    ~primitive:Sema.Primitive_type.U8 ~pointer_depth:0
  |> Result.get_ok

let byte_scalar = Integer_scalar_storage.of_type byte_type |> Option.get
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
  match (Type.base type_, Frame.location_frame_slot location) with
  | Type.Aggregate _, Some slot
    when Type.pointer_depth type_ = 0
         && Frame.location_kind location = Frame.Automatic_local
         && Frame.location_declarator_shape location = Frame.Object
         && Frame.location_value_shape location = Frame.Scalar
         && Frame.location_dimensions location = []
         && Frame.location_source_dimensions_checked location
         && Option.is_none (Frame.location_callback_pointer location)
         && bytes > 0L
         && bytes <= Int64.of_int Int.max_int
         && Frame.location_allocated_size location = bytes
         && Frame.frame_slot_size slot = bytes ->
      Some { byte_size = Int64.to_int bytes }
  | _ -> None
