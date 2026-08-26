module Sequence = Instruction_sequence
module Type = Sema.Type
module Primitive = Sema.Primitive_type

type literal =
  | Integer of int64
  | Character of int64
  | Float_bits of int64
  | String_bytes of string

type description = {
  instruction_id : Sequence.Instruction_id.t;
  value_id : Sequence.Value_id.t;
  literal : literal;
  span : Common.Span.t option;
}

type t = { literal : literal; sequence : Sequence.t; result_type : Type.t }
type expression_result = Lowered of t | Not_literal

let reference_commit = Opcode.reference_commit

let primitive primitive pointer_depth =
  match
    Type.make_primitive ~form:Type.Internal_storage ~primitive ~pointer_depth
  with
  | Ok type_ -> type_
  | Error message -> invalid_arg message

let i64 = primitive Primitive.I64 0
let u64 = primitive Primitive.U64 0
let f64 = primitive Primitive.F64 0
let u8_pointer = primitive Primitive.U8 1

let lower_one description ~opcode ~payload ~result_type =
  let instruction : Sequence.description =
    {
      instruction_id = description.instruction_id;
      opcode;
      operands = [];
      result = Some { value_id = description.value_id };
      target_type = Some result_type;
      payload = Some payload;
      flags = 0L;
      span = description.span;
    }
  in
  match Sequence.create [ instruction ] with
  | Ok sequence -> Ok { literal = description.literal; sequence; result_type }
  | Error errors -> Error errors

let lower_integer description value =
  let result_type = if value < 0L then u64 else i64 in
  lower_one description ~opcode:Opcode.Ic_imm_i64
    ~payload:(Sequence.Integer value) ~result_type

let lower_float description bits =
  lower_one description ~opcode:Opcode.Ic_imm_f64
    ~payload:(Sequence.Float_bits bits) ~result_type:f64

let lower_string description bytes =
  lower_one description ~opcode:Opcode.Ic_str_const
    ~payload:(Sequence.Bytes bytes) ~result_type:u8_pointer

let lower (description : description) =
  match description.literal with
  | Integer value -> lower_integer description value
  | Character value -> lower_integer description value
  | Float_bits bits -> lower_float description bits
  | String_bytes bytes -> lower_string description bytes

let literal_of_expression = function
  | Frontend.Ast.Integer_literal source -> (
      match source.literal_value with
      | Frontend.Ast.Integer_value value -> Some (Integer value, source)
      | Frontend.Ast.Float_value _ | Frontend.Ast.Bytes_value _ -> None)
  | Frontend.Ast.Character_literal source -> (
      match source.literal_value with
      | Frontend.Ast.Integer_value value -> Some (Character value, source)
      | Frontend.Ast.Float_value _ | Frontend.Ast.Bytes_value _ -> None)
  | Frontend.Ast.Float_literal source -> (
      match source.literal_value with
      | Frontend.Ast.Float_value value ->
          Some (Float_bits (Int64.bits_of_float value), source)
      | Frontend.Ast.Integer_value _ | Frontend.Ast.Bytes_value _ -> None)
  | Frontend.Ast.String_literal source -> (
      match source.literal_value with
      | Frontend.Ast.Bytes_value value -> Some (String_bytes value, source)
      | Frontend.Ast.Integer_value _ | Frontend.Ast.Float_value _ -> None)
  | Frontend.Ast.Identifier_expression _
  | Frontend.Ast.Current_position_expression _
  | Frontend.Ast.Sizeof_expression _
  | Frontend.Ast.Offset_expression _
  | Frontend.Ast.Defined_expression _
  | Frontend.Ast.Parenthesized_expression _
  | Frontend.Ast.Prefix_expression _
  | Frontend.Ast.Postfix_expression _
  | Frontend.Ast.Postfix_cast_expression _
  | Frontend.Ast.Binary_expression _
  | Frontend.Ast.Call_expression _
  | Frontend.Ast.Index_expression _
  | Frontend.Ast.Member_expression _ -> None

let lower_expression ~instruction_id ~value_id expression =
  match literal_of_expression expression with
  | None -> Ok Not_literal
  | Some (literal, source) ->
      lower
        {
          instruction_id;
          value_id;
          literal;
          span = Some source.literal_location.span;
        }
      |> Result.map (fun lowered -> Lowered lowered)

let sequence lowered = lowered.sequence
let result_type lowered = lowered.result_type

let kind_name = function
  | Integer _ -> "integer"
  | Character _ -> "character"
  | Float_bits _ -> "f64"
  | String_bytes _ -> "string"

let human lowered =
  Printf.sprintf "holyc-ir-literal-v1 reference=%s\nkind=%s result-type=%s\n%s"
    reference_commit
    (kind_name lowered.literal)
    (Sequence.type_name lowered.result_type)
    (Sequence.human_body lowered.sequence)
