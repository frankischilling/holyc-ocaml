module Frame = Sema.Function_frame_layout
module Local = Sema.Local_type_resolution
module Type = Sema.Type

let is_storage_free location =
  let type_ = Frame.location_checked_type location in
  match
    ( Type.base type_,
      Frame.location_local_source location,
      Frame.location_frame_slot location,
      Frame.location_type_reference location )
  with
  | ( Type.Primitive (_, Sema.Primitive_type.U0),
      Some local,
      Some slot,
      Some reference ) -> (
      Type.pointer_depth type_ = 0
      && Frame.location_kind location = Frame.Automatic_local
      && Frame.location_declarator_shape location = Frame.Object
      && Frame.location_value_shape location = Frame.Scalar
      && Frame.location_dimensions location = []
      && Frame.location_element_size location = 0L
      && Frame.location_allocated_size location = 0L
      && Frame.frame_slot_size slot = 0L
      && Frame.location_alignment location = 1
      && Option.is_none (Frame.location_callback_pointer location)
      && Local.local_storage local = Local.Automatic
      && Local.local_symbol local == Frame.location_symbol location
      && Local.local_type_reference local == reference
      && Local.local_array_dimensions local = []
      (* PrsVar reparses initialized locals as assignments. Those can alias an
         original register or frame location even when the declared size is zero. *)
      && Option.is_none (Local.local_initializer local)
      &&
      match Frame.location_register_selection location with
      | Sema.Register_request.Unspecified | Sema.Register_request.Disabled ->
          true
      | Sema.Register_request.Allocatable | Sema.Register_request.Explicit _ ->
          false)
  | _ -> false
