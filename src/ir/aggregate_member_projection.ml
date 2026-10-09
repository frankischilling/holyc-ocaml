module Members = Sema.Aggregate_member_index
module Type = Sema.Type

type t = {
  lookup : Members.lookup;
  base_pointer : Type.t;
  pointer_type : Type.t;
  offset : int64;
  strides : int64 list;
}

let create ~lookup ~base_pointer ~pointer_type =
  let member = Members.lookup_member lookup in
  let member_type = Members.member_type member in
  let layout = Members.member_layout member in
  let exact_aggregate =
    Automatic_aggregate_storage.aggregate_pointer base_pointer
    &&
    match Type.base base_pointer with
    | Type.Aggregate symbol -> symbol == Members.lookup_queried_aggregate lookup
    | _ -> false
  in
  let exact_member =
    Option.fold ~none:false ~some:(Type.equal pointer_type)
      (Type.pointer_to member_type |> Result.to_option)
    && Type.equal member_type
         (Sema.Type_reference.resolved_type
            (Members.member_type_reference member))
  in
  let scalar_member =
    Type.pointer_depth member_type = 0
    && (Option.is_some (Integer_scalar_storage.of_type member_type)
       ||
       match Type.base member_type with
       | Type.Aggregate _ -> true
       | _ -> false)
  in
  if
    (not exact_aggregate) || (not exact_member) || (not scalar_member)
    || Members.member_is_function_pointer member
  then
    Error
      "member projection does not match its selected aggregate and scalar field"
  else
    let checked_strides =
      List.fold_left
        (fun state dimension ->
          Result.bind state (fun (bytes, strides) ->
              if
                bytes <= 0L || dimension < 0L
                || (dimension > 0L && bytes > Int64.div Int64.max_int dimension)
              then Error "member array strides exceed the hosted address range"
              else Ok (Int64.mul bytes dimension, bytes :: strides)))
        (Ok (layout.element_size, []))
        (List.rev layout.dimensions)
    in
    Result.map
      (fun (_, strides) ->
        { lookup; base_pointer; pointer_type; offset = layout.offset; strides })
      checked_strides

let matches projection ~base_pointer ~pointer_type ~offset =
  Type.equal projection.base_pointer base_pointer
  && Type.equal projection.pointer_type pointer_type
  && projection.offset = offset

let offset projection = projection.offset
let strides projection = projection.strides

let member_symbol projection =
  Members.member_symbol (Members.lookup_member projection.lookup)
