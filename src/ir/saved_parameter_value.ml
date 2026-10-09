module Typed = Sema.Function_call_expression_result

type native_comparison = { owner : unit ref; offset : int; live : unit -> bool }

type data = {
  source : Typed.expression_result;
  type_ : Sema.Type.t;
  identity : unit ref;
  native_comparison : native_comparison option;
  string_default : string option;
}

type t =
  | Word of int64
  | Callback of Retained_function.t * Typed.expression_result
  | Undefined_callback of Typed.expression_result
  | Data of data

let word bits = Word bits

let word_bits = function
  | Word bits -> Some bits
  | Callback _ | Undefined_callback _ | Data _ -> None

let callback_source = function
  | Word _ | Undefined_callback _ | Data _ -> None
  | Callback (link, source) -> Some (link, source)

let undefined_callback_source = function
  | Undefined_callback source -> Some source
  | Word _ | Callback _ | Data _ -> None

let data ~source ~type_ =
  if
    Sema.Type.pointer_depth type_ = 1
    && Option.is_some
         (Option.bind
            (Result.to_option (Sema.Type.dereference type_))
            Integer_scalar_storage.of_type)
    && (not (Typed.result_is_numeric_callback source))
    && (not (Typed.result_is_callback_storage source))
    && Option.is_none (Typed.result_function_declaration source)
    && Option.fold ~none:false
         ~some:(Integer_scalar_storage.compatible_pointer type_)
         (Option.bind (Typed.result_type source) (fun original ->
              if Typed.result_is_array_address source then
                Result.to_option (Sema.Type.pointer_to original)
              else Some original))
  then
    Ok
      (Data
         {
           source;
           type_;
           identity = ref ();
           native_comparison = None;
           string_default = None;
         })
  else Error "saved data requires a one-level scalar data-pointer parameter"

let data_source = function
  | Data data -> Some data
  | _ -> None

let data_expression data = data.source
let data_type data = data.type_
let same_data left right = left.identity == right.identity

let with_native_data_comparison ~owner ~offset ~live = function
  | Data data
    when offset >= 0 && Option.is_none data.native_comparison && live () ->
      Ok (Data { data with native_comparison = Some { owner; offset; live } })
  | _ ->
      Error "native data comparison requires an original unbound live capture"

let compare_native_data left right =
  match (left.native_comparison, right.native_comparison) with
  | Some left, Some right when left.live () && right.live () ->
      Some (Ok (left.owner == right.owner && left.offset = right.offset))
  | Some _, Some _ ->
      Some (Error "native data comparison belongs to an expired arena")
  | _ -> None

let with_string_default ~bytes = function
  | Data data
    when Option.is_none data.string_default
         && String.length bytes > 0
         && bytes.[String.length bytes - 1] = '\000' ->
      Ok (Data { data with string_default = Some bytes })
  | _ ->
      Error
        "saved string comparison requires an original completed terminated copy"

let compare_string_defaults left right =
  let bytes = function
    | Data data -> data.string_default
    | _ -> None
  in
  match (bytes left, bytes right) with
  | None, None -> None
  | Some _, None | None, Some _ -> Some false
  | Some left, Some right ->
      let rec equal index =
        let left_end = index = String.length left || left.[index] = '\000' in
        let right_end = index = String.length right || right.[index] = '\000' in
        if left_end || right_end then left_end && right_end
        else left.[index] = right.[index] && equal (index + 1)
      in
      Some (equal 0)

let rec accepts_callback_expression source =
  let module Resolution = Sema.Function_call_resolution in
  Typed.result_array_rank source = 0
  &&
  match Typed.result_category source with
  | Typed.Callback_value -> true
  | Typed.Address_value ->
      Option.is_some (Typed.result_function_declaration source)
  | _ -> (
      match
        Resolution.argument_expression_kind (Typed.result_source source)
      with
      | Resolution.Parenthesized_expression _ ->
          Option.fold ~none:false ~some:accepts_callback_expression
            (Typed.result_operand source)
      | Resolution.Binary_expression binary
        when Resolution.binary_operator binary
             = Generated.Intermediate_codes.Ic_assign ->
          Option.fold ~none:false
            ~some:(fun (left, _) -> Typed.result_is_callback_storage left)
            (Typed.result_binary_operands source)
      | _ -> false)

let callback ~source ~link =
  if
    (not (accepts_callback_expression source))
    || not
         (match Typed.result_category source with
         | Typed.Address_value ->
             Option.fold ~none:false
               ~some:(fun declaration ->
                 Retained_function.metadata link
                 |> Sema.Outer_environment.function_declaration
                 |> fun original ->
                 original == declaration
                 || Sema.Function_resolution.is_joined_successor
                      ~earlier:declaration ~later:original)
               (Typed.result_function_declaration source)
         | _ -> true)
  then Error "saved callback requires its checked original expression value"
  else Ok (Callback (link, source))

let undefined_callback ~source =
  if accepts_callback_expression source then Ok (Undefined_callback source)
  else
    Error
      "saved undefined callback requires its checked original expression value"

let same left right =
  match (left, right) with
  | Word left, Word right -> Int64.equal left right
  | Callback (left, source), Callback (right, other) ->
      source == other && Retained_function.same left right
  | Undefined_callback source, Undefined_callback other -> source == other
  | Data left, Data right -> same_data left right
  | _ -> false
