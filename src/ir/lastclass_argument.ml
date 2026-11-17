module Typed = Sema.Function_call_expression_result
module Headers = Sema.Function_type_resolution

type t = {
  mode : Sema.Global_resolution.compilation_mode;
  parameter : Headers.parameter;
  substitution : Typed.lastclass_substitution;
}

let create ~mode ~parameter substitution =
  match Headers.parameter_default parameter with
  | Some (Headers.Lastclass_default _) ->
      if
        Option.is_some (Typed.lastclass_previous_result substitution)
        && (Option.is_none (Typed.lastclass_class_name substitution)
           || Option.is_none (Typed.lastclass_class_type substitution))
      then Error "lastclass requires the preceding argument's resolved class"
      else if
        mode = Sema.Global_resolution.Aot
        && Option.is_none (Typed.lastclass_class_name substitution)
      then
        Error
          "AOT lastclass has no preceding class for its original StrLen/StrNew \
           materialization"
      else Ok { mode; parameter; substitution }
  | _ -> Error "lastclass substitution has another selected parameter"

let matches left right =
  left.mode = right.mode
  && left.parameter == right.parameter
  && left.substitution == right.substitution

let is_aot value = value.mode = Sema.Global_resolution.Aot
let name value = Typed.lastclass_class_name value.substitution

let shared_class value =
  if is_aot value then None else Typed.lastclass_class_type value.substitution

let callback_pointer value =
  match Headers.parameter_declarator_kind value.parameter with
  | Headers.Object -> None
  | Headers.Function_pointer pointer -> Some pointer

let type_ value =
  if is_aot value then
    Sema.Type.make_primitive ~form:Internal_storage ~primitive:U8
      ~pointer_depth:1
    |> Result.get_ok
  else
    match callback_pointer value with
    | Some pointer ->
        Headers.function_pointer_storage_type pointer |> Result.get_ok
    | None ->
        value.parameter |> Headers.parameter_type_reference
        |> Sema.Type_reference.resolved_type

let has_name value = Option.is_some (name value)
