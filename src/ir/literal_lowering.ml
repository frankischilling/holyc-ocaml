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

type identity = {
  instruction_id : Sequence.Instruction_id.t;
  value_id : Sequence.Value_id.t;
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

let describe_one (description : description) ~opcode ~payload ~result_type =
  ( {
      Sequence.instruction_id = description.instruction_id;
      opcode;
      operands = [];
      result = Some { value_id = description.value_id };
      target_type = Some result_type;
      payload = Some payload;
      flags = 0L;
      span = description.span;
    },
    result_type )

let lower_integer description value =
  let result_type = if value < 0L then u64 else i64 in
  describe_one description ~opcode:Opcode.Ic_imm_i64
    ~payload:(Sequence.Integer value) ~result_type

let lower_float description bits =
  describe_one description ~opcode:Opcode.Ic_imm_f64
    ~payload:(Sequence.Float_bits bits) ~result_type:f64

let lower_string description bytes =
  describe_one description ~opcode:Opcode.Ic_str_const
    ~payload:(Sequence.Bytes bytes) ~result_type:u8_pointer

let describe_literal (description : description) =
  match description.literal with
  | Integer value -> lower_integer description value
  | Character value -> lower_integer description value
  | Float_bits bits -> lower_float description bits
  | String_bytes bytes -> lower_string description bytes

let lower description =
  let instruction, result_type = describe_literal description in
  match Sequence.create [ instruction ] with
  | Ok sequence -> Ok { literal = description.literal; sequence; result_type }
  | Error errors -> Error errors

let unwrap_expression expression =
  let current = ref expression in
  let unary_minus_spans = ref [] in
  let searching = ref true in
  while !searching do
    match !current with
    | Frontend.Ast.Parenthesized_expression grouped ->
        current := grouped.grouped_expression
    | Frontend.Ast.Prefix_expression prefix
      when prefix.prefix_operator_kind = Frontend.Ast.Unary_plus ->
        current := prefix.prefix_operand
    | Frontend.Ast.Prefix_expression prefix
      when prefix.prefix_operator_kind = Frontend.Ast.Unary_minus ->
        unary_minus_spans :=
          prefix.prefix_operator.operator_location.span :: !unary_minus_spans;
        current := prefix.prefix_operand
    | _ -> searching := false
  done;
  (!current, !unary_minus_spans)

let literal_of_expression expression =
  let expression, unary_minus_spans = unwrap_expression expression in
  match expression with
  | Frontend.Ast.Integer_literal source -> (
      match source.literal_value with
      | Frontend.Ast.Integer_value value ->
          Some (Integer value, source, unary_minus_spans)
      | Frontend.Ast.Float_value _ | Frontend.Ast.Bytes_value _ -> None)
  | Frontend.Ast.Character_literal source -> (
      match source.literal_value with
      | Frontend.Ast.Integer_value value ->
          Some (Character value, source, unary_minus_spans)
      | Frontend.Ast.Float_value _ | Frontend.Ast.Bytes_value _ -> None)
  | Frontend.Ast.Float_literal source -> (
      match source.literal_value with
      | Frontend.Ast.Float_value value ->
          Some
            (Float_bits (Int64.bits_of_float value), source, unary_minus_spans)
      | Frontend.Ast.Integer_value _ | Frontend.Ast.Bytes_value _ -> None)
  | Frontend.Ast.String_literal source -> (
      match source.literal_value with
      | Frontend.Ast.Bytes_value value ->
          Some (String_bytes value, source, unary_minus_spans)
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

let identity_count_error expression ~expected ~actual =
  {
    Sequence.code = "HCIRL0001";
    message =
      Printf.sprintf "expected %d unary instruction identit%s, got %d" expected
        (if expected = 1 then "y" else "ies")
        actual;
    instruction_id = None;
    span = Some (Frontend.Ast.expression_location expression).span;
  }

let lower_expression ~instruction_id ~value_id ?(unary_identities = [])
    expression =
  match literal_of_expression expression with
  | None -> Ok Not_literal
  | Some (literal, source, unary_minus_spans) -> (
      let expected = List.length unary_minus_spans in
      let actual = List.length unary_identities in
      if actual <> expected then
        Error [ identity_count_error expression ~expected ~actual ]
      else
        let literal_instruction, result_type =
          describe_literal
            {
              instruction_id;
              value_id;
              literal;
              span = Some source.literal_location.span;
            }
        in
        let rec add_unary reversed current_value spans identities =
          match (spans, identities) with
          | span :: remaining_spans, identity :: remaining_identities ->
              let instruction : Sequence.description =
                {
                  instruction_id = identity.instruction_id;
                  opcode = Opcode.Ic_unary_minus;
                  operands = [ current_value ];
                  result = Some { value_id = identity.value_id };
                  target_type = Some result_type;
                  payload = None;
                  flags = 0L;
                  span = Some span;
                }
              in
              add_unary (instruction :: reversed) identity.value_id
                remaining_spans remaining_identities
          | [], [] -> List.rev reversed
          | _ -> assert false
        in
        let descriptions =
          add_unary [ literal_instruction ] value_id unary_minus_spans
            unary_identities
        in
        match Sequence.create descriptions with
        | Ok sequence -> Ok (Lowered { literal; sequence; result_type })
        | Error errors -> Error errors)

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
