module P = Primitive_type

let scalar_info type_ =
  match Type.base type_ with
  | Type.Primitive (form, primitive) when Type.pointer_depth type_ = 0 ->
      let info = P.info primitive in
      if info.category = P.Integer && info.byte_size > 0 then Some (form, info)
      else None
  | _ -> None

let internal primitive =
  Type.make_primitive ~form:Type.Internal_storage ~primitive ~pointer_depth:0
  |> Result.get_ok

let forward type_ =
  match scalar_info type_ with
  | Some (_, info) -> internal info.primitive
  | None -> type_

let declared type_ =
  match scalar_info type_ with
  | Some (_, info) when info.declaration_form = P.Internal_type ->
      internal info.primitive
  | _ -> type_

let negate operand =
  match scalar_info operand with
  | Some (Type.Internal_storage, info) when info.raw_is_unsigned -> (
      match
        List.find_opt
          (fun primitive -> (P.info primitive).raw_id = info.raw_id - 1)
          P.all
      with
      | Some primitive -> internal primitive
      | None -> forward operand)
  | _ -> forward operand
