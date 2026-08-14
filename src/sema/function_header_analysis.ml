type evaluated_default = Bits of int64 | String_bytes of string

type parameter_input = {
  parameter : Function_type_resolution.parameter;
  comparison_default : evaluated_default option;
}

type function_input = {
  declaration : Function_resolution.resolved_declaration;
  parameters : parameter_input list;
}

type warning_kind = Return_type_mismatch | Argument_list_mismatch

type warning = {
  kind : warning_kind;
  code : string;
  declaration : Function_resolution.resolved_declaration;
  replaced_header : Function_resolution.declaration_site;
  origin : Symbol.origin;
}

type comparison = {
  declaration : Function_resolution.resolved_declaration;
  replaced_header : Function_resolution.declaration_site;
  option_enabled : bool;
  return_types_match : bool option;
  arguments_match : bool option;
  warnings : warning list;
}

type t = {
  source_functions : Function_resolution.t;
  comparisons : comparison list;
  warnings : warning list;
}

type error_kind = Invalid_input of string
type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let invalid_input ?origin message =
  { code = "HCSEMA0036"; kind = Invalid_input message; origin }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with Invalid_input message -> message

let error_to_string error = error.code ^ ": " ^ error_message error
let parameter_input_parameter (input : parameter_input) = input.parameter

let parameter_input_comparison_default (input : parameter_input) =
  input.comparison_default

let function_input_declaration (input : function_input) = input.declaration
let function_input_parameters (input : function_input) = input.parameters
let source_functions (result : t) = result.source_functions
let comparisons (result : t) = result.comparisons
let warnings (result : t) = result.warnings
let comparison_declaration (comparison : comparison) = comparison.declaration

let comparison_replaced_header (comparison : comparison) =
  comparison.replaced_header

let comparison_option_enabled (comparison : comparison) =
  comparison.option_enabled

let comparison_return_types_match (comparison : comparison) =
  comparison.return_types_match

let comparison_arguments_match (comparison : comparison) =
  comparison.arguments_match

let comparison_warnings (comparison : comparison) = comparison.warnings
let warning_kind (warning : warning) = warning.kind
let warning_code (warning : warning) = warning.code
let warning_declaration (warning : warning) = warning.declaration
let warning_replaced_header (warning : warning) = warning.replaced_header
let warning_origin (warning : warning) = warning.origin

let warning_kind_name = function
  | Return_type_mismatch -> "return-type-mismatch"
  | Argument_list_mismatch -> "argument-list-mismatch"

let declaration_function declaration =
  declaration |> Function_resolution.resolved_declaration_site
  |> Function_resolution.declaration_site_function

let function_symbol declaration =
  declaration |> declaration_function
  |> Function_type_resolution.function_symbol

let warning_message (warning : warning) =
  let name = warning.declaration |> function_symbol |> Symbol.name in
  match warning.kind with
  | Return_type_mismatch ->
      Printf.sprintf
        "function %S return type does not match the replaced header" name
  | Argument_list_mismatch ->
      Printf.sprintf
        "function %S argument list does not match the replaced header" name

let make_parameter_input ~parameter ~evaluated_default =
  let origin = Function_type_resolution.parameter_origin parameter in
  match
    ( Function_type_resolution.parameter_default parameter,
      evaluated_default )
  with
  | None, None -> Ok { parameter; comparison_default = None }
  | None, Some _ ->
      Error
        (invalid_input ~origin
           "a function parameter without a default has an evaluated value")
  | Some (Function_type_resolution.Lastclass_default _), None ->
      Ok { parameter; comparison_default = Some (Bits 0L) }
  | Some (Function_type_resolution.Lastclass_default _), Some _ ->
      Error
        (invalid_input ~origin
           "a lastclass default uses its zero-initialized record payload and \
            cannot accept an evaluated value")
  | Some (Function_type_resolution.Expression_default _), None ->
      Error
        (invalid_input ~origin
           "an ordinary function default requires a compile-time evaluated \
            value")
  | ( Some
        (Function_type_resolution.Expression_default
          { contains_string_literal = true; _ }),
      Some (String_bytes _ as value) ) ->
      Ok { parameter; comparison_default = Some value }
  | ( Some
        (Function_type_resolution.Expression_default
          { contains_string_literal = false; _ }),
      Some (Bits _ as value) ) ->
      Ok { parameter; comparison_default = Some value }
  | ( Some
        (Function_type_resolution.Expression_default
          { contains_string_literal = true; _ }),
      Some (Bits _) ) ->
      Error
        (invalid_input ~origin
           "a string-backed function default requires evaluated string bytes")
  | ( Some
        (Function_type_resolution.Expression_default
          { contains_string_literal = false; _ }),
      Some (String_bytes _) ) ->
      Error
        (invalid_input ~origin
           "a scalar function default requires an evaluated 64-bit payload")

let make_function_input ~declaration parameters =
  let expected =
    declaration |> declaration_function
    |> Function_type_resolution.function_signature
    |> Function_type_resolution.signature_parameters
  in
  let origin = declaration |> function_symbol |> Symbol.origin in
  let rec validate expected inputs =
    match (expected, inputs) with
    | [], [] -> Ok { declaration; parameters }
    | parameter :: expected_rest, input :: input_rest ->
        if parameter != input.parameter then
          Error
            (invalid_input ~origin
               "function header inputs do not match the declaration's fixed \
                parameters")
        else validate expected_rest input_rest
    | [], _ :: _ | _ :: _, [] ->
        Error
          (invalid_input ~origin
             "function header input count does not match the declaration")
  in
  validate expected parameters

type class_key =
  | Primitive_class of
      Type.primitive_form * Primitive_type.t * int
  | Aggregate_class of Symbol.t * int

type member = {
  name : string;
  class_key : class_key;
  default : evaluated_default option;
}

let class_key_of_type type_ =
  let pointer_depth = Type.pointer_depth type_ in
  match Type.base type_ with
  | Type.Primitive (form, primitive) ->
      Primitive_class (form, primitive, pointer_depth)
  | Type.Aggregate symbol -> Aggregate_class (symbol, pointer_depth)

let class_key_of_parameter parameter =
  match Function_type_resolution.parameter_declarator_kind parameter with
  | Function_type_resolution.Object ->
      parameter |> Function_type_resolution.parameter_type_reference
      |> Type_reference.resolved_type |> class_key_of_type
  | Function_type_resolution.Function_pointer pointer ->
      Primitive_class
        ( Type.Internal_storage,
          Primitive_type.I64,
          pointer
          |> Function_type_resolution.function_pointer_indirection_origins
          |> List.length )

let same_class_key left right =
  match (left, right) with
  | ( Primitive_class (left_form, left_primitive, left_depth),
      Primitive_class (right_form, right_primitive, right_depth) ) ->
      left_form = right_form
      && Primitive_type.equal left_primitive right_primitive
      && left_depth = right_depth
  | ( Aggregate_class (left_symbol, left_depth),
      Aggregate_class (right_symbol, right_depth) ) ->
      Symbol.Id.equal (Symbol.id left_symbol) (Symbol.id right_symbol)
      && left_depth = right_depth
  | Primitive_class _, Aggregate_class _
  | Aggregate_class _, Primitive_class _ -> false

let fixed_member input =
  let parameter = input.parameter in
  {
    name =
      parameter |> Function_type_resolution.parameter_name
      |> Option.value ~default:"_anon_";
    class_key = class_key_of_parameter parameter;
    default = input.comparison_default;
  }

let synthetic_member binding =
  {
    name =
      binding |> Function_type_resolution.synthetic_binding_kind
      |> Function_type_resolution.synthetic_parameter_name;
    class_key =
      binding |> Function_type_resolution.synthetic_binding_type
      |> class_key_of_type;
    default = None;
  }

let members function_ input =
  let fixed = List.map fixed_member input.parameters in
  match Function_type_resolution.function_variadic_bindings function_ with
  | None -> fixed
  | Some bindings ->
      fixed
      @ [
          bindings |> Function_type_resolution.variadic_argc
          |> synthetic_member;
          bindings |> Function_type_resolution.variadic_argv
          |> synthetic_member;
        ]

let same_default left right =
  let c_string_equal left right =
    let rec compare index =
      let left_finished =
        index = String.length left || Char.equal left.[index] '\000'
      in
      let right_finished =
        index = String.length right || Char.equal right.[index] '\000'
      in
      if left_finished || right_finished then left_finished && right_finished
      else
        Char.equal left.[index] right.[index]
        && compare (index + 1)
    in
    compare 0
  in
  match (left, right) with
  | None, None -> true
  | Some (Bits left), Some (Bits right) -> Int64.equal left right
  | Some (String_bytes left), Some (String_bytes right) ->
      c_string_equal left right
  | None, Some _ | Some _, None
  | Some (Bits _), Some (String_bytes _)
  | Some (String_bytes _), Some (Bits _) -> false

let same_member left right =
  String.equal left.name right.name
  && same_class_key left.class_key right.class_key
  && same_default left.default right.default

let rec member_list_compare count current replaced =
  match (current, replaced) with
  | [], [] -> true
  | _ :: _, _ :: _ when count = 0 -> true
  | current :: current_rest, replaced :: replaced_rest ->
      same_member current replaced
      && member_list_compare (count - 1) current_rest replaced_rest
  | [], _ :: _ | _ :: _, [] -> false

let same_type_reference left right =
  same_class_key
    (left |> Type_reference.resolved_type |> class_key_of_type)
    (right |> Type_reference.resolved_type |> class_key_of_type)

module Int_map = Map.Make (Int)

let symbol_number symbol = symbol |> Symbol.id |> Symbol.Id.to_int

let validate_owner table declaration =
  let function_ = declaration_function declaration in
  let symbol = Function_type_resolution.function_symbol function_ in
  let scope = Function_type_resolution.function_scope function_ in
  let identity =
    Function_resolution.resolved_declaration_identity_symbol declaration
  in
  if
    not
      (Symbol_table.owns_symbol table symbol
      && Symbol_table.owns_symbol table identity)
  then
    Error
      (invalid_input ~origin:(Symbol.origin symbol)
         "function header analysis belongs to a different symbol table")
  else if not (Symbol_table.owns_scope table scope) then
    Error
      (invalid_input ~origin:(Symbol.origin symbol)
         "function header scope belongs to a different symbol table")
  else Ok ()

let validate_inputs table functions inputs =
  let expected = Function_resolution.declarations functions in
  let rec pair by_symbol expected inputs =
    match (expected, inputs) with
    | [], [] -> Ok by_symbol
    | declaration :: expected_rest, (input : function_input) :: input_rest ->
        if declaration != input.declaration then
          Error
            (invalid_input
               "function header inputs do not match function identity \
                resolution")
        else (
          match validate_owner table declaration with
          | Error _ as error -> error
          | Ok () ->
              let symbol = declaration |> function_symbol in
              let number = symbol_number symbol in
              if Int_map.mem number by_symbol then
                Error
                  (invalid_input ~origin:(Symbol.origin symbol)
                     "function header input repeats a source declaration")
              else
                pair (Int_map.add number input by_symbol) expected_rest
                  input_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error
          (invalid_input
             "function header input count does not match function identity \
              resolution")
  in
  pair Int_map.empty expected inputs

let input_for_site by_symbol site =
  let symbol =
    site |> Function_resolution.declaration_site_function
    |> Function_type_resolution.function_symbol
  in
  match Int_map.find_opt (symbol_number symbol) by_symbol with
  | Some input -> Ok input
  | None ->
      Error
        (invalid_input ~origin:(Symbol.origin symbol)
           "replaced function header has no evaluated input")

let make_warning kind declaration replaced_header =
  let symbol = function_symbol declaration in
  {
    kind;
    code =
      (match kind with
      | Return_type_mismatch -> "HCSEMA0037"
      | Argument_list_mismatch -> "HCSEMA0038");
    declaration;
    replaced_header;
    origin = Symbol.origin symbol;
  }

let compare_join by_symbol (input : function_input) replaced_header =
  let declaration = input.declaration in
  let site = Function_resolution.resolved_declaration_site declaration in
  let option_enabled =
    Compiler_option.is_enabled
      ~mask:(Function_resolution.declaration_site_compiler_option_mask site)
      Compiler_option.Warn_header_mismatch
  in
  if not option_enabled then
    Ok
      {
        declaration;
        replaced_header;
        option_enabled;
        return_types_match = None;
        arguments_match = None;
        warnings = [];
      }
  else
    match input_for_site by_symbol replaced_header with
    | Error _ as error -> error
    | Ok replaced_input ->
        let current_function = declaration_function declaration in
        let replaced_function =
          Function_resolution.declaration_site_function replaced_header
        in
        let return_types_match =
          same_type_reference
            (Function_type_resolution.function_return_type current_function)
            (Function_type_resolution.function_return_type replaced_function)
        in
        let replaced_fixed_count =
          replaced_function |> Function_type_resolution.function_signature
          |> Function_type_resolution.signature_parameters |> List.length
        in
        let arguments_match =
          member_list_compare replaced_fixed_count
            (members current_function input)
            (members replaced_function replaced_input)
        in
        let warnings =
          []
          |> (fun warnings ->
               if return_types_match then warnings
               else
                 make_warning Return_type_mismatch declaration replaced_header
                 :: warnings)
          |> (fun warnings ->
               if arguments_match then warnings
               else
                 make_warning Argument_list_mismatch declaration
                   replaced_header
                 :: warnings)
          |> List.rev
        in
        Ok
          {
            declaration;
            replaced_header;
            option_enabled;
            return_types_match = Some return_types_match;
            arguments_match = Some arguments_match;
            warnings;
          }

let analyze_validated functions by_symbol inputs =
  let rec loop comparisons_rev warnings_rev = function
    | [] ->
        Ok
          {
            source_functions = functions;
            comparisons = List.rev comparisons_rev;
            warnings = List.rev warnings_rev;
          }
    | (input : function_input) :: rest -> (
        match
          Function_resolution.resolved_declaration_replaced_header
            input.declaration
        with
        | None -> loop comparisons_rev warnings_rev rest
        | Some replaced_header -> (
            match compare_join by_symbol input replaced_header with
            | Error _ as error -> error
            | Ok comparison ->
                loop (comparison :: comparisons_rev)
                  (List.rev_append comparison.warnings warnings_rev)
                  rest))
  in
  loop [] [] inputs

let analyze ~table ~functions inputs =
  match validate_inputs table functions inputs with
  | Error _ as error -> error
  | Ok by_symbol -> analyze_validated functions by_symbol inputs
