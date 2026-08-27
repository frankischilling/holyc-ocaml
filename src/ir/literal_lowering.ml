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

type unary_operation = {
  opcode : Opcode.t;
  span : Common.Span.t;
  cancels_dereference : bool;
}

type typed_unary_operation = {
  typed_opcode : Opcode.t;
  typed_span : Common.Span.t;
  typed_result_type : Type.t;
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

let direct_literal_of_typed_source source =
  match Sema.Function_call_resolution.argument_expression_kind source with
  | Sema.Function_call_resolution.Integer_literal value ->
      Some (Integer value, source)
  | Sema.Function_call_resolution.Character_literal value ->
      Some (Character value, source)
  | Sema.Function_call_resolution.Float_literal bits ->
      Some (Float_bits bits, source)
  | Sema.Function_call_resolution.String_literal bytes ->
      Some (String_bytes bytes, source)
  | Sema.Function_call_resolution.Parenthesized_expression _
  | Sema.Function_call_resolution.Prefix_expression _
  | Sema.Function_call_resolution.Postfix_expression _
  | Sema.Function_call_resolution.Postfix_cast_expression _
  | Sema.Function_call_resolution.Binary_expression _
  | Sema.Function_call_resolution.Index_expression _
  | Sema.Function_call_resolution.Member_access_expression _
  | Sema.Function_call_resolution.Bound_identifier_expression _
  | Sema.Function_call_resolution.Top_level_bound_identifier_expression _
  | Sema.Function_call_resolution.Defined_expression _
  | Sema.Function_call_resolution.Unresolved_expression _ -> None

let typed_result_error ?span message =
  { Sequence.code = "HCIRL0003"; message; instruction_id = None; span }

let identity_count_error ~span ~expected ~actual =
  {
    Sequence.code = "HCIRL0001";
    message =
      Printf.sprintf "expected %d unary instruction identit%s, got %d" expected
        (if expected = 1 then "y" else "ies")
        actual;
    instruction_id = None;
    span;
  }

let typed_operand result expected_source description =
  match Sema.Function_call_expression_result.result_operand result with
  | None ->
      Error
        (typed_result_error
           (Printf.sprintf
              "typed semantic %s does not retain its checked operand result"
              description))
  | Some operand
    when Sema.Function_call_expression_result.result_source operand
         != expected_source ->
      Error
        (typed_result_error
           (Printf.sprintf
              "typed semantic %s operand metadata does not match its source \
               expression"
              description))
  | Some operand -> Ok operand

let typed_unary result prefix ~opcode ~description =
  match Sema.Function_call_expression_result.result_type result with
  | None ->
      Error
        (typed_result_error
           (Printf.sprintf
              "typed semantic %s does not have a checked result type"
              description))
  | Some typed_result_type -> (
      match Sema.Function_call_resolution.prefix_operator_origin prefix with
      | Sema.Symbol.Source_location location ->
          Ok
            {
              typed_opcode = opcode;
              typed_span = location.span;
              typed_result_type;
            }
      | Sema.Symbol.Pinned_source _ | Sema.Symbol.Synthesized _ ->
          Error
            (typed_result_error
               (Printf.sprintf
                  "typed %s operator does not have a source location"
                  description)))

let unwrap_typed_prefix current unary_operations prefix ~opcode ~description =
  match
    ( typed_unary current prefix ~opcode ~description,
      typed_operand current
        (Sema.Function_call_resolution.prefix_operand prefix)
        (description ^ " expression") )
  with
  | Ok operation, Ok operand -> Ok (operand, operation :: unary_operations)
  | Error operation_error, _ -> Error operation_error
  | _, Error operand_error -> Error operand_error

let typed_address_error ~span message =
  {
    Sequence.code = "HCIRL0002";
    message = "cannot lower typed prefix address-of: " ^ message;
    instruction_id = None;
    span = Some span;
  }

let unwrap_typed_address current unary_operations prefix =
  match
    typed_operand current
      (Sema.Function_call_resolution.prefix_operand prefix)
      "address-of expression"
  with
  | Error operand_error -> Error operand_error
  | Ok operand -> (
      match Sema.Function_call_resolution.prefix_operator_origin prefix with
      | Sema.Symbol.Pinned_source _ | Sema.Symbol.Synthesized _ ->
          Error
            (typed_result_error
               "typed address-of operator does not have a source location")
      | Sema.Symbol.Source_location location -> (
          match Sema.Function_call_expression_result.result_type current with
          | Some typed_result_type ->
              Ok
                ( operand,
                  {
                    typed_opcode = Opcode.Ic_addr;
                    typed_span = location.span;
                    typed_result_type;
                  }
                  :: unary_operations )
          | None -> (
              match
                Sema.Function_call_expression_result.result_type operand
              with
              | Some operand_type -> (
                  match Type.pointer_to operand_type with
                  | Error message ->
                      Error (typed_address_error ~span:location.span message)
                  | Ok _ ->
                      Error
                        (typed_result_error ~span:location.span
                           "typed semantic address-of does not have its \
                            checked result type"))
              | None ->
                  Error
                    (typed_result_error ~span:location.span
                       "typed semantic address-of operand does not have a \
                        checked type"))))

let cancel_typed_dereferences operations =
  let reversed = ref [] in
  List.iter
    (fun operation ->
      if Opcode.equal operation.typed_opcode Opcode.Ic_addr then
        match !reversed with
        | previous :: remaining
          when Opcode.equal previous.typed_opcode Opcode.Ic_deref ->
            reversed := operation :: remaining
        | _ -> reversed := operation :: !reversed
      else reversed := operation :: !reversed)
    operations;
  List.rev !reversed

let literal_of_typed_source result =
  let current = ref result in
  let unary_operations = ref [] in
  let error = ref None in
  let unwrapping = ref true in
  while !unwrapping do
    let source = Sema.Function_call_expression_result.result_source !current in
    match Sema.Function_call_resolution.argument_expression_kind source with
    | Sema.Function_call_resolution.Parenthesized_expression inner -> (
        match typed_operand !current inner "parenthesized expression" with
        | Ok operand -> current := operand
        | Error operand_error ->
            error := Some operand_error;
            unwrapping := false)
    | Sema.Function_call_resolution.Prefix_expression prefix
      when Sema.Function_call_resolution.prefix_operator prefix
           = Sema.Function_call_resolution.Unary_plus -> (
        match
          typed_operand !current
            (Sema.Function_call_resolution.prefix_operand prefix)
            "unary-plus expression"
        with
        | Ok operand -> current := operand
        | Error operand_error ->
            error := Some operand_error;
            unwrapping := false)
    | Sema.Function_call_resolution.Prefix_expression prefix
      when Sema.Function_call_resolution.prefix_operator prefix
           = Sema.Function_call_resolution.Unary_minus -> (
        match
          unwrap_typed_prefix !current !unary_operations prefix
            ~opcode:Opcode.Ic_unary_minus ~description:"unary-minus"
        with
        | Ok (operand, operations) ->
            current := operand;
            unary_operations := operations
        | Error prefix_error ->
            error := Some prefix_error;
            unwrapping := false)
    | Sema.Function_call_resolution.Prefix_expression prefix
      when Sema.Function_call_resolution.prefix_operator prefix
           = Sema.Function_call_resolution.Logical_not -> (
        match
          unwrap_typed_prefix !current !unary_operations prefix
            ~opcode:Opcode.Ic_not ~description:"logical-not"
        with
        | Ok (operand, operations) ->
            current := operand;
            unary_operations := operations
        | Error prefix_error ->
            error := Some prefix_error;
            unwrapping := false)
    | Sema.Function_call_resolution.Prefix_expression prefix
      when Sema.Function_call_resolution.prefix_operator prefix
           = Sema.Function_call_resolution.Bitwise_not -> (
        match
          unwrap_typed_prefix !current !unary_operations prefix
            ~opcode:Opcode.Ic_com ~description:"bitwise-complement"
        with
        | Ok (operand, operations) ->
            current := operand;
            unary_operations := operations
        | Error prefix_error ->
            error := Some prefix_error;
            unwrapping := false)
    | Sema.Function_call_resolution.Prefix_expression prefix
      when Sema.Function_call_resolution.prefix_operator prefix
           = Sema.Function_call_resolution.Dereference -> (
        match
          unwrap_typed_prefix !current !unary_operations prefix
            ~opcode:Opcode.Ic_deref ~description:"dereference"
        with
        | Ok (operand, operations) ->
            current := operand;
            unary_operations := operations
        | Error prefix_error ->
            error := Some prefix_error;
            unwrapping := false)
    | Sema.Function_call_resolution.Prefix_expression prefix
      when Sema.Function_call_resolution.prefix_operator prefix
           = Sema.Function_call_resolution.Address_of -> (
        match unwrap_typed_address !current !unary_operations prefix with
        | Ok (operand, operations) ->
            current := operand;
            unary_operations := operations
        | Error prefix_error ->
            error := Some prefix_error;
            unwrapping := false)
    | Sema.Function_call_resolution.Integer_literal _
    | Sema.Function_call_resolution.Float_literal _
    | Sema.Function_call_resolution.Character_literal _
    | Sema.Function_call_resolution.String_literal _
    | Sema.Function_call_resolution.Prefix_expression _
    | Sema.Function_call_resolution.Postfix_expression _
    | Sema.Function_call_resolution.Postfix_cast_expression _
    | Sema.Function_call_resolution.Binary_expression _
    | Sema.Function_call_resolution.Index_expression _
    | Sema.Function_call_resolution.Member_access_expression _
    | Sema.Function_call_resolution.Bound_identifier_expression _
    | Sema.Function_call_resolution.Top_level_bound_identifier_expression _
    | Sema.Function_call_resolution.Defined_expression _
    | Sema.Function_call_resolution.Unresolved_expression _ ->
        unwrapping := false
  done;
  match !error with
  | Some error -> Error error
  | None ->
      let source =
        Sema.Function_call_expression_result.result_source !current
      in
      Ok
        ( Option.map
            (fun (literal, literal_source) ->
              (literal, literal_source, !current))
            (direct_literal_of_typed_source source),
          cancel_typed_dereferences !unary_operations )

let typed_source_span source message =
  match Sema.Function_call_resolution.argument_expression_origin source with
  | Sema.Symbol.Source_location location -> Ok location.span
  | Sema.Symbol.Pinned_source _ | Sema.Symbol.Synthesized _ ->
      Error (typed_result_error message)

let typed_result_span result source literal_source =
  let result_origin =
    Sema.Function_call_expression_result.result_origin result
  in
  let source_origin =
    Sema.Function_call_resolution.argument_expression_origin source
  in
  if result_origin <> source_origin then
    Error
      (typed_result_error
         "typed semantic literal source metadata does not match its source \
          expression")
  else
    match result_origin with
    | Sema.Symbol.Source_location _ ->
        typed_source_span literal_source
          "typed semantic literal leaf does not have a source location"
    | Sema.Symbol.Pinned_source _ | Sema.Symbol.Synthesized _ ->
        Error
          (typed_result_error
             "typed semantic literal does not have a source location")

let describe_typed_literal (description : description) ~result_type =
  match description.literal with
  | Integer value | Character value ->
      describe_one description ~opcode:Opcode.Ic_imm_i64
        ~payload:(Sequence.Integer value) ~result_type
  | Float_bits bits ->
      describe_one description ~opcode:Opcode.Ic_imm_f64
        ~payload:(Sequence.Float_bits bits) ~result_type
  | String_bytes bytes ->
      describe_one description ~opcode:Opcode.Ic_str_const
        ~payload:(Sequence.Bytes bytes) ~result_type

let typed_identity_span result =
  match Sema.Function_call_expression_result.result_origin result with
  | Sema.Symbol.Source_location location -> Some location.span
  | Sema.Symbol.Pinned_source _ | Sema.Symbol.Synthesized _ -> None

let rec append_typed_unaries reversed current_value current_type operations
    identities =
  match (operations, identities) with
  | operation :: remaining_operations, identity :: remaining_identities ->
      let result_type = operation.typed_result_type in
      let instruction : Sequence.description =
        {
          instruction_id = identity.instruction_id;
          opcode = operation.typed_opcode;
          operands = [ current_value ];
          result = Some { value_id = identity.value_id };
          target_type = Some result_type;
          payload = None;
          flags = 0L;
          span = Some operation.typed_span;
        }
      in
      append_typed_unaries (instruction :: reversed) identity.value_id
        result_type remaining_operations remaining_identities
  | [], [] -> (List.rev reversed, current_type)
  | _ -> assert false

let lower_typed_result ~instruction_id ~value_id ?(unary_identities = []) result
    =
  let source = Sema.Function_call_expression_result.result_source result in
  match literal_of_typed_source result with
  | Error error -> Error [ error ]
  | Ok (None, _) -> Ok Not_literal
  | Ok (Some (literal, literal_source, literal_result), unary_operations) -> (
      let expected = List.length unary_operations in
      let actual = List.length unary_identities in
      if actual <> expected then
        Error
          [
            identity_count_error
              ~span:(typed_identity_span result)
              ~expected ~actual;
          ]
      else
        match typed_result_span result source literal_source with
        | Error error -> Error [ error ]
        | Ok span -> (
            match
              Sema.Function_call_expression_result.result_type literal_result
            with
            | None ->
                Error
                  [
                    typed_result_error ~span
                      "typed semantic literal does not have a checked result \
                       type";
                  ]
            | Some result_type -> (
                let description =
                  { instruction_id; value_id; literal; span = Some span }
                in
                let instruction, literal_result_type =
                  describe_typed_literal description ~result_type
                in
                let descriptions, result_type =
                  append_typed_unaries [ instruction ] value_id
                    literal_result_type unary_operations unary_identities
                in
                if
                  Some result_type
                  <> Sema.Function_call_expression_result.result_type result
                then
                  Error
                    [
                      typed_result_error ~span
                        "typed semantic unary result types do not reach the \
                         outer checked type";
                    ]
                else
                  match Sequence.create descriptions with
                  | Ok sequence ->
                      Ok (Lowered { literal; sequence; result_type })
                  | Error errors -> Error errors)))

let unwrap_expression expression =
  let current = ref expression in
  let unary_operations = ref [] in
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
        unary_operations :=
          {
            opcode = Opcode.Ic_unary_minus;
            span = prefix.prefix_operator.operator_location.span;
            cancels_dereference = false;
          }
          :: !unary_operations;
        current := prefix.prefix_operand
    | Frontend.Ast.Prefix_expression prefix
      when prefix.prefix_operator_kind = Frontend.Ast.Logical_not ->
        unary_operations :=
          {
            opcode = Opcode.Ic_not;
            span = prefix.prefix_operator.operator_location.span;
            cancels_dereference = false;
          }
          :: !unary_operations;
        current := prefix.prefix_operand
    | Frontend.Ast.Prefix_expression prefix
      when prefix.prefix_operator_kind = Frontend.Ast.Bitwise_not ->
        unary_operations :=
          {
            opcode = Opcode.Ic_com;
            span = prefix.prefix_operator.operator_location.span;
            cancels_dereference = false;
          }
          :: !unary_operations;
        current := prefix.prefix_operand
    | Frontend.Ast.Prefix_expression prefix
      when prefix.prefix_operator_kind = Frontend.Ast.Dereference ->
        unary_operations :=
          {
            opcode = Opcode.Ic_deref;
            span = prefix.prefix_operator.operator_location.span;
            cancels_dereference = false;
          }
          :: !unary_operations;
        current := prefix.prefix_operand
    | Frontend.Ast.Prefix_expression prefix
      when prefix.prefix_operator_kind = Frontend.Ast.Address_of ->
        unary_operations :=
          {
            opcode = Opcode.Ic_addr;
            span = prefix.prefix_operator.operator_location.span;
            cancels_dereference = false;
          }
          :: !unary_operations;
        current := prefix.prefix_operand
    | _ -> searching := false
  done;
  let reversed = ref [] in
  List.iter
    (fun operation ->
      if Opcode.equal operation.opcode Opcode.Ic_addr then
        match !reversed with
        | previous :: remaining
          when Opcode.equal previous.opcode Opcode.Ic_deref ->
            reversed :=
              { operation with cancels_dereference = true } :: remaining
        | _ -> reversed := operation :: !reversed
      else reversed := operation :: !reversed)
    !unary_operations;
  (!current, List.rev !reversed)

let literal_of_expression expression =
  let expression, unary_operations = unwrap_expression expression in
  match expression with
  | Frontend.Ast.Integer_literal source -> (
      match source.literal_value with
      | Frontend.Ast.Integer_value value ->
          Some (Integer value, source, unary_operations)
      | Frontend.Ast.Float_value _ | Frontend.Ast.Bytes_value _ -> None)
  | Frontend.Ast.Character_literal source -> (
      match source.literal_value with
      | Frontend.Ast.Integer_value value ->
          Some (Character value, source, unary_operations)
      | Frontend.Ast.Float_value _ | Frontend.Ast.Bytes_value _ -> None)
  | Frontend.Ast.Float_literal source -> (
      match source.literal_value with
      | Frontend.Ast.Float_value value ->
          Some (Float_bits (Int64.bits_of_float value), source, unary_operations)
      | Frontend.Ast.Integer_value _ | Frontend.Ast.Bytes_value _ -> None)
  | Frontend.Ast.String_literal source -> (
      match source.literal_value with
      | Frontend.Ast.Bytes_value value ->
          Some (String_bytes value, source, unary_operations)
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

let unary_result_type operation current_type =
  if Opcode.equal operation.opcode Opcode.Ic_com then Ok i64
  else if
    Opcode.equal operation.opcode Opcode.Ic_deref
    && Type.pointer_depth current_type > 0
  then
    match Type.dereference current_type with
    | Ok type_ -> Ok type_
    | Error message -> invalid_arg message
  else if Opcode.equal operation.opcode Opcode.Ic_addr then
    let operand_type =
      if operation.cancels_dereference && Type.pointer_depth current_type > 0
      then
        match Type.dereference current_type with
        | Ok type_ -> type_
        | Error message -> invalid_arg message
      else current_type
    in
    Type.pointer_to operand_type
  else Ok current_type

let unary_type_error operation identity message =
  {
    Sequence.code = "HCIRL0002";
    message = "cannot lower prefix address-of: " ^ message;
    instruction_id =
      Some (Sequence.Instruction_id.to_int identity.instruction_id);
    span = Some operation.span;
  }

let lower_expression ~instruction_id ~value_id ?(unary_identities = [])
    expression =
  match literal_of_expression expression with
  | None -> Ok Not_literal
  | Some (literal, source, unary_operations) -> (
      let expected = List.length unary_operations in
      let actual = List.length unary_identities in
      if actual <> expected then
        Error
          [
            identity_count_error
              ~span:(Some (Frontend.Ast.expression_location expression).span)
              ~expected ~actual;
          ]
      else
        let literal_instruction, literal_result_type =
          describe_literal
            {
              instruction_id;
              value_id;
              literal;
              span = Some source.literal_location.span;
            }
        in
        let rec add_unary reversed current_value current_type operations
            identities =
          match (operations, identities) with
          | operation :: remaining_operations, identity :: remaining_identities
            -> (
              match unary_result_type operation current_type with
              | Error message ->
                  Error [ unary_type_error operation identity message ]
              | Ok result_type ->
                  let instruction : Sequence.description =
                    {
                      instruction_id = identity.instruction_id;
                      opcode = operation.opcode;
                      operands = [ current_value ];
                      result = Some { value_id = identity.value_id };
                      target_type = Some result_type;
                      payload = None;
                      flags = 0L;
                      span = Some operation.span;
                    }
                  in
                  add_unary (instruction :: reversed) identity.value_id
                    result_type remaining_operations remaining_identities)
          | [], [] -> Ok (List.rev reversed, current_type)
          | _ -> assert false
        in
        match
          add_unary [ literal_instruction ] value_id literal_result_type
            unary_operations unary_identities
        with
        | Error _ as error -> error
        | Ok (descriptions, result_type) -> (
            match Sequence.create descriptions with
            | Ok sequence -> Ok (Lowered { literal; sequence; result_type })
            | Error errors -> Error errors))

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
