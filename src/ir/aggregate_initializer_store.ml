module Sequence = Instruction_sequence
module Type = Sema.Type
module Leaf = Sema.Initializer_type

type t = {
  descriptions_ : Sequence.description list;
  result_ : Sequence.Value_id.t;
  type_ : Type.t;
  next_instruction_ : Sequence.Instruction_id.t;
  next_value_ : Sequence.Value_id.t;
}

let descriptions t = t.descriptions_
let result_value t = t.result_
let result_type t = t.type_
let next_instruction_id t = t.next_instruction_
let next_value_id t = t.next_value_

let instruction_count scalar =
  let count = Leaf.scalar_byte_size scalar in
  if count = 0 then 3 else 2 + (7 * count)

let ( let* ) = Result.bind

let lower ~scalar ~byte_offset ~root_type ~root ~word ~span ~instruction_id
    ~value_id =
  let singleton result = Result.map_error (fun error -> [ error ]) result in
  let instruction = ref (Sequence.Instruction_id.to_int instruction_id)
  and value = ref (Sequence.Value_id.to_int value_id) in
  let count = Leaf.scalar_byte_size scalar in
  let* () =
    match Type.base root_type with
    | Type.Aggregate _
      when Type.pointer_depth root_type = 0
           && byte_offset >= 0
           && byte_offset <= Int.max_int - count
           && count >= 0 && count <= 8
           && Result.is_ok
                (Integer_initializer_layout.normalized_word scalar 0L) -> Ok ()
    | _ ->
        Error
          [
            {
              Sequence.code = "HCIRL0004";
              message =
                "aggregate initializer store requires its original integer \
                 leaf and nominal class root";
              instruction_id = None;
              span;
            };
          ]
  in
  let byte_type =
    Type.make_primitive ~form:Type.Public_spelling
      ~primitive:Sema.Primitive_type.U8 ~pointer_depth:0
    |> Result.get_ok
  in
  let byte_pointer = Type.pointer_to byte_type |> Result.get_ok in
  let class_pointer = Type.pointer_to root_type |> Result.get_ok in
  let word_type =
    Type.make_primitive ~form:Type.Internal_storage
      ~primitive:Sema.Primitive_type.I64 ~pointer_depth:0
    |> Result.get_ok
  in
  let descriptions = ref [] in
  let emit opcode operands target_type payload =
    let* instruction_id =
      Sequence.Instruction_id.of_int !instruction |> singleton
    in
    let* value_id = Sequence.Value_id.of_int !value |> singleton in
    if !instruction = Int.max_int || !value = Int.max_int then
      Error
        [
          {
            Sequence.code = "HCIRL0005";
            message = "aggregate initializer identity space is exhausted";
            instruction_id = None;
            span;
          };
        ]
    else (
      incr instruction;
      incr value;
      descriptions :=
        {
          Sequence.instruction_id;
          opcode;
          operands;
          result = Some { value_id };
          target_type = Some target_type;
          payload;
          flags = 0L;
          span;
        }
        :: !descriptions;
      Ok value_id)
  in
  let* owned_root = emit Opcode.Ic_addr [ root ] class_pointer None in
  let* byte_root =
    emit Opcode.Ic_holyc_typecast [ owned_root ] byte_pointer
      (Some (Sequence.Integer 1L))
  in
  let* word =
    emit Opcode.Ic_holyc_typecast [ word ] word_type
      (Some (Sequence.Integer 1L))
  in
  let rec write index previous =
    if index = count then Ok previous
    else
      let* stride =
        emit Opcode.Ic_imm_i64 [] byte_pointer (Some (Sequence.Integer 1L))
      in
      let* coordinate =
        emit Opcode.Ic_imm_i64 [] word_type
          (Some (Sequence.Integer (Int64.of_int (byte_offset + index))))
      in
      let* scaled =
        emit Opcode.Ic_mul [ stride; coordinate ] byte_pointer None
      in
      let* destination =
        emit Opcode.Ic_add [ byte_root; scaled ] byte_pointer None
      in
      let* bits =
        if index = 0 then Ok word
        else
          emit Opcode.Ic_shr_const [ word ] word_type
            (Some (Sequence.Integer (Int64.of_int (index * 8))))
      in
      let* byte =
        emit Opcode.Ic_holyc_typecast [ bits ] word_type
          (Some (Sequence.Integer 1L))
      in
      let* stored =
        emit Opcode.Ic_assign [ destination; byte ] byte_type None
      in
      write (index + 1) stored
  in
  let* result_ = write 0 word in
  let* next_instruction_ =
    Sequence.Instruction_id.of_int !instruction |> singleton
  in
  let* next_value_ = Sequence.Value_id.of_int !value |> singleton in
  Ok
    {
      descriptions_ = List.rev !descriptions;
      result_;
      type_ = (if count = 0 then word_type else byte_type);
      next_instruction_;
      next_value_;
    }

let copy_instruction_count bytes = 2 + (6 * String.length bytes)

let lower_copy ~bytes ~byte_offset ~root_type ~root ~span ~instruction_id
    ~value_id =
  let singleton result = Result.map_error (fun error -> [ error ]) result in
  let count = String.length bytes in
  let instruction = ref (Sequence.Instruction_id.to_int instruction_id)
  and value = ref (Sequence.Value_id.to_int value_id) in
  let* () =
    if
      count > 0
      && count <= (Int.max_int - 2) / 6
      && byte_offset >= 0
      && byte_offset <= Int.max_int - count
      && Type.pointer_depth root_type = 0
    then Ok ()
    else
      Error
        [
          {
            Sequence.code = "HCIRL0004";
            message =
              "initializer copy requires its original nonempty source byte \
               window";
            instruction_id = None;
            span;
          };
        ]
  in
  let byte_type =
    Type.make_primitive ~form:Type.Public_spelling
      ~primitive:Sema.Primitive_type.U8 ~pointer_depth:0
    |> Result.get_ok
  in
  let byte_pointer = Type.pointer_to byte_type |> Result.get_ok in
  let root_pointer = Type.pointer_to root_type |> Result.get_ok in
  let word_type =
    Type.make_primitive ~form:Type.Internal_storage
      ~primitive:Sema.Primitive_type.I64 ~pointer_depth:0
    |> Result.get_ok
  in
  let descriptions = ref [] in
  let emit opcode operands target_type payload =
    let* instruction_id =
      Sequence.Instruction_id.of_int !instruction |> singleton
    in
    let* value_id = Sequence.Value_id.of_int !value |> singleton in
    if !instruction = Int.max_int || !value = Int.max_int then
      Error
        [
          {
            Sequence.code = "HCIRL0005";
            message = "initializer copy identity space is exhausted";
            instruction_id = None;
            span;
          };
        ]
    else (
      incr instruction;
      incr value;
      descriptions :=
        {
          Sequence.instruction_id;
          opcode;
          operands;
          result = Some { value_id };
          target_type = Some target_type;
          payload;
          flags = 0L;
          span;
        }
        :: !descriptions;
      Ok value_id)
  in
  let* owned = emit Opcode.Ic_addr [ root ] root_pointer None in
  let* byte_root =
    emit Opcode.Ic_holyc_typecast [ owned ] byte_pointer
      (Some (Sequence.Integer 1L))
  in
  let rec write index previous =
    if index = count then Ok previous
    else
      let* stride =
        emit Opcode.Ic_imm_i64 [] byte_pointer (Some (Sequence.Integer 1L))
      in
      let* coordinate =
        emit Opcode.Ic_imm_i64 [] word_type
          (Some (Sequence.Integer (Int64.of_int (byte_offset + index))))
      in
      let* scaled =
        emit Opcode.Ic_mul [ stride; coordinate ] byte_pointer None
      in
      let* destination =
        emit Opcode.Ic_add [ byte_root; scaled ] byte_pointer None
      in
      let* word =
        emit Opcode.Ic_imm_i64 [] word_type
          (Some (Sequence.Integer (Int64.of_int (Char.code bytes.[index]))))
      in
      let* stored =
        emit Opcode.Ic_assign [ destination; word ] byte_type None
      in
      write (index + 1) stored
  in
  let* result_ = write 0 byte_root in
  let* next_instruction_ =
    Sequence.Instruction_id.of_int !instruction |> singleton
  in
  let* next_value_ = Sequence.Value_id.of_int !value |> singleton in
  Ok
    {
      descriptions_ = List.rev !descriptions;
      result_;
      type_ = byte_type;
      next_instruction_;
      next_value_;
    }
