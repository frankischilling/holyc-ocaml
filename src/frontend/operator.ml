module Facts = Generated.Operator_tables

type t =
  | Increment
  | Decrement
  | Arrow
  | Double_colon
  | Shift_left
  | Shift_right
  | Equal
  | Not_equal
  | Less_equal
  | Greater_equal
  | Logical_and
  | Logical_or
  | Logical_xor
  | Shift_left_assign
  | Shift_right_assign
  | Multiply_assign
  | Divide_assign
  | Modulo_assign
  | Bit_and_assign
  | Bit_or_assign
  | Bit_xor_assign
  | Add_assign
  | Subtract_assign
  | Dot_dot
  | Ellipsis
  | Current_position

type provenance =
  | Dual_table of int
  | Shift_assignment
  | Dot_sequence
  | Dollar_sequence

type association = Unspecified | Left | Right
type precedence = { name : string; value : int; source_line : int }

type binary_operator = {
  spelling : string;
  token_name : string option;
  token_id : int;
  precedence_name : string;
  precedence_value : int;
  association : association;
  ic_name : string;
  ic_id : int;
  source_line : int;
}

let reference_commit = Facts.reference_commit
let precedence_source_path = Facts.compiler_source_path
let binary_source_path = Facts.cinit_source_path

let of_token_name = function
  | Some "TK_PLUS_PLUS" -> Increment
  | Some "TK_MINUS_MINUS" -> Decrement
  | Some "TK_DEREFERENCE" -> Arrow
  | Some "TK_DBL_COLON" -> Double_colon
  | Some "TK_SHL" -> Shift_left
  | Some "TK_SHR" -> Shift_right
  | Some "TK_EQU_EQU" -> Equal
  | Some "TK_NOT_EQU" -> Not_equal
  | Some "TK_LESS_EQU" -> Less_equal
  | Some "TK_GREATER_EQU" -> Greater_equal
  | Some "TK_AND_AND" -> Logical_and
  | Some "TK_OR_OR" -> Logical_or
  | Some "TK_XOR_XOR" -> Logical_xor
  | Some "TK_SHL_EQU" -> Shift_left_assign
  | Some "TK_SHR_EQU" -> Shift_right_assign
  | Some "TK_MUL_EQU" -> Multiply_assign
  | Some "TK_DIV_EQU" -> Divide_assign
  | Some "TK_MOD_EQU" -> Modulo_assign
  | Some "TK_AND_EQU" -> Bit_and_assign
  | Some "TK_OR_EQU" -> Bit_or_assign
  | Some "TK_XOR_EQU" -> Bit_xor_assign
  | Some "TK_ADD_EQU" -> Add_assign
  | Some "TK_SUB_EQU" -> Subtract_assign
  | Some "TK_DOT_DOT" -> Dot_dot
  | Some "TK_ELLIPSIS" -> Ellipsis
  | None -> Current_position
  | Some name ->
      invalid_arg (Printf.sprintf "unknown generated operator %s" name)

let entries =
  List.map
    (fun (entry : Facts.operator) -> (entry, of_token_name entry.token_name))
    Facts.operators

let all =
  List.map
    (fun ((entry : Facts.operator), operator) -> (entry.spelling, operator))
    entries

let has_prefix text ~offset spelling =
  let width = String.length spelling in
  offset + width <= String.length text
  && String.sub text offset width |> String.equal spelling

let find_prefix text ~offset =
  List.find_map
    (fun (spelling, operator) ->
      if has_prefix text ~offset spelling then
        Some (operator, String.length spelling)
      else None)
    all

let generated operator =
  List.find (fun (_, candidate) -> candidate = operator) entries |> fst

let spelling operator = (generated operator).spelling
let templeos_token_id operator = (generated operator).token_id
let source_line operator = (generated operator).source_line

let provenance operator =
  match (generated operator).origin with
  | Facts.Dual_table group -> Dual_table group
  | Facts.Shift_assignment -> Shift_assignment
  | Facts.Dot_sequence -> Dot_sequence
  | Facts.Current_position -> Dollar_sequence

let source_path operator =
  match (generated operator).origin with
  | Facts.Dual_table _ -> Facts.cinit_source_path
  | Facts.Shift_assignment | Facts.Dot_sequence | Facts.Current_position ->
      Facts.lex_source_path

let precedences =
  List.map
    (fun (entry : Facts.named_constant) ->
      {
        name = entry.name;
        value = entry.value;
        source_line = entry.source_line;
      })
    Facts.precedences

let association = function
  | Facts.Unspecified -> Unspecified
  | Facts.Left -> Left
  | Facts.Right -> Right

let binary_operators =
  List.map
    (fun (entry : Facts.binary_operator) ->
      {
        spelling = entry.spelling;
        token_name = entry.token_name;
        token_id = entry.token_id;
        precedence_name = entry.precedence_name;
        precedence_value = entry.precedence_value;
        association = association entry.association;
        ic_name = entry.ic_name;
        ic_id = entry.ic_id;
        source_line = entry.source_line;
      })
    Facts.binary_operators

let find_binary spelling =
  List.find_opt
    (fun (operator : binary_operator) ->
      String.equal operator.spelling spelling)
    binary_operators
