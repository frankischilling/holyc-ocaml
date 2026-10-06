module Typed = Sema.Function_call_expression_result

type t =
  | Word of int64
  | Callback of Retained_function.t * Typed.expression_result

let word bits = Word bits

let word_bits = function
  | Word bits -> Some bits
  | Callback _ -> None

let callback_source = function
  | Word _ -> None
  | Callback (link, source) -> Some (link, source)

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
                 |> fun original -> original == declaration)
               (Typed.result_function_declaration source)
         | _ -> true)
  then Error "saved callback requires its checked original expression value"
  else Ok (Callback (link, source))

let same left right =
  match (left, right) with
  | Word left, Word right -> Int64.equal left right
  | Callback (left, source), Callback (right, other) ->
      source == other && Retained_function.same left right
  | _ -> false
