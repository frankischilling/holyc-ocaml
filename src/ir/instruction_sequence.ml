type error = {
  code : string;
  message : string;
  instruction_id : int option;
  span : Common.Span.t option;
}

module Make_id (Name : sig
  val label : string
end) =
struct
  type t = int

  let of_int value =
    if value < 0 then
      Error
        {
          code = "HCIR0001";
          message = Printf.sprintf "%s must be nonnegative" Name.label;
          instruction_id = None;
          span = None;
        }
    else Ok value

  let to_int value = value
  let compare = Int.compare
  let equal = Int.equal
end

module Instruction_id = Make_id (struct
  let label = "instruction ID"
end)

module Value_id = Make_id (struct
  let label = "value ID"
end)

module Block_id = Make_id (struct
  let label = "block ID"
end)

type payload =
  | Integer of int64
  | Float_bits of int64
  | Bytes of string
  | Symbol of Sema.Symbol.t
  | Block of Block_id.t
  | Block_targets of Block_id.t list

type value_definition = { value_id : Value_id.t }

type description = {
  instruction_id : Instruction_id.t;
  opcode : Opcode.t;
  operands : Value_id.t list;
  result : value_definition option;
  target_type : Sema.Type.t option;
  payload : payload option;
  flags : int64;
  span : Common.Span.t option;
}

type instruction = description
type t = instruction list

let reference_commit = Opcode.reference_commit
let known_flag_mask = 0x1ffe1ffffL

module Int_map = Map.Make (Int)
module Int_set = Set.Make (Int)

let instruction_number description =
  Instruction_id.to_int description.instruction_id

let error ?span description code message =
  {
    code;
    message;
    instruction_id = Some (instruction_number description);
    span;
  }

let expected_argument_count = function
  | Opcode.Zero -> Some 0
  | Opcode.One -> Some 1
  | Opcode.Two -> Some 2
  | Opcode.Variable -> None

let validate_shape description =
  let info = Opcode.info description.opcode in
  let errors = ref [] in
  let add code message =
    errors := error ?span:description.span description code message :: !errors
  in
  (match expected_argument_count info.argument_count with
  | Some expected when List.length description.operands <> expected ->
      add "HCIR0004"
        (Printf.sprintf "%s expects %d operand%s, but instruction has %d"
           info.source_name expected
           (if expected = 1 then "" else "s")
           (List.length description.operands))
  | Some _ | None -> ());
  let actual_results =
    Option.fold ~none:0 ~some:(fun _ -> 1) description.result
  in
  if actual_results <> info.result_count then
    add "HCIR0005"
      (Printf.sprintf "%s expects %d result%s, but instruction has %d"
         info.source_name info.result_count
         (if info.result_count = 1 then "" else "s")
         actual_results);
  if actual_results > 0 && Option.is_none description.target_type then
    add "HCIR0010"
      (Printf.sprintf "%s produces a value but has no target type"
         info.source_name);
  (match description.span with
  | Some span when span.start < 0 || span.stop < span.start ->
      add "HCIR0002"
        (Printf.sprintf "invalid source span %d..%d" span.start span.stop)
  | Some _ | None -> ());
  let unknown_flags =
    Int64.logand description.flags (Int64.lognot known_flag_mask)
  in
  if unknown_flags <> 0L then
    add "HCIR0003"
      (Printf.sprintf "instruction uses unsupported flag bits 0x%Lx"
         unknown_flags);
  List.rev !errors

let result_positions descriptions =
  List.fold_left
    (fun positions (position, description) ->
      match description.result with
      | None -> positions
      | Some result ->
          let value = Value_id.to_int result.value_id in
          if Int_map.mem value positions then positions
          else Int_map.add value position positions)
    Int_map.empty
    (List.mapi
       (fun position description -> (position, description))
       descriptions)

let create descriptions =
  let positions = result_positions descriptions in
  let instruction_ids = ref Int_set.empty in
  let value_ids = ref Int_set.empty in
  let errors = ref [] in
  let add item = errors := item :: !errors in
  List.iteri
    (fun position description ->
      List.iter add (validate_shape description);
      let instruction_id = instruction_number description in
      if Int_set.mem instruction_id !instruction_ids then
        add
          (error ?span:description.span description "HCIR0006"
             (Printf.sprintf "instruction ID %d is defined more than once"
                instruction_id))
      else instruction_ids := Int_set.add instruction_id !instruction_ids;
      (match description.result with
      | None -> ()
      | Some result ->
          let value_id = Value_id.to_int result.value_id in
          if Int_set.mem value_id !value_ids then
            add
              (error ?span:description.span description "HCIR0007"
                 (Printf.sprintf "value %%%d is defined more than once" value_id))
          else value_ids := Int_set.add value_id !value_ids);
      List.iter
        (fun operand ->
          let value_id = Value_id.to_int operand in
          if Int_set.mem value_id !value_ids then ()
          else
            match Int_map.find_opt value_id positions with
            | Some result_position when result_position >= position ->
                add
                  (error ?span:description.span description "HCIR0008"
                     (Printf.sprintf
                        "value %%%d is used before its definition in source \
                         order"
                        value_id))
            | Some _ ->
                add
                  (error ?span:description.span description "HCIR0009"
                     (Printf.sprintf "value %%%d has no usable definition"
                        value_id))
            | None ->
                add
                  (error ?span:description.span description "HCIR0009"
                     (Printf.sprintf
                        "value %%%d is not defined in this sequence" value_id)))
        description.operands)
    descriptions;
  match List.rev !errors with
  | [] -> Ok descriptions
  | errors -> Error errors

let instructions sequence = sequence
let description instruction = instruction
let length = List.length

let primitive_form_name = function
  | Sema.Type.Public_spelling -> "public"
  | Sema.Type.Internal_storage -> "internal"

let type_name value_type =
  let base =
    match Sema.Type.base value_type with
    | Sema.Type.Primitive (form, primitive) ->
        Printf.sprintf "%s:%s" (primitive_form_name form)
          (Sema.Primitive_type.to_string primitive)
    | Sema.Type.Aggregate symbol ->
        Printf.sprintf "aggregate:%s#%d" (Sema.Symbol.name symbol)
          (Sema.Symbol.Id.to_int (Sema.Symbol.id symbol))
  in
  base ^ String.make (Sema.Type.pointer_depth value_type) '*'

let add_escaped_bytes buffer bytes =
  Buffer.add_char buffer '"';
  String.iter
    (fun character ->
      match Char.code character with
      | 0x09 -> Buffer.add_string buffer "\\t"
      | 0x0a -> Buffer.add_string buffer "\\n"
      | 0x0d -> Buffer.add_string buffer "\\r"
      | 0x22 -> Buffer.add_string buffer "\\\""
      | 0x5c -> Buffer.add_string buffer "\\\\"
      | code when code >= 0x20 && code <= 0x7e ->
          Buffer.add_char buffer character
      | code -> Printf.bprintf buffer "\\x%02x" code)
    bytes;
  Buffer.add_char buffer '"'

let add_payload buffer = function
  | Integer value -> Printf.bprintf buffer " i64:%Ld" value
  | Float_bits bits -> Printf.bprintf buffer " f64:0x%016Lx" bits
  | Bytes bytes ->
      Buffer.add_string buffer " bytes:";
      add_escaped_bytes buffer bytes
  | Symbol symbol ->
      Printf.bprintf buffer " symbol:@s%d:%s:"
        (Sema.Symbol.Id.to_int (Sema.Symbol.id symbol))
        (Sema.Symbol.kind_name (Sema.Symbol.kind symbol));
      add_escaped_bytes buffer (Sema.Symbol.name symbol)
  | Block block -> Printf.bprintf buffer " block:^b%d" (Block_id.to_int block)
  | Block_targets blocks ->
      Buffer.add_string buffer " blocks:[";
      List.iteri
        (fun index block ->
          if index > 0 then Buffer.add_char buffer ',';
          Printf.bprintf buffer "^b%d" (Block_id.to_int block))
        blocks;
      Buffer.add_char buffer ']'

let add_instruction buffer description =
  Printf.bprintf buffer "!i%d " (instruction_number description);
  (match description.result with
  | None -> ()
  | Some result ->
      let target_type = Option.get description.target_type in
      Printf.bprintf buffer "%%v%d:%s = "
        (Value_id.to_int result.value_id)
        (type_name target_type));
  Buffer.add_string buffer (Opcode.to_source_name description.opcode);
  List.iter
    (fun operand -> Printf.bprintf buffer " %%v%d" (Value_id.to_int operand))
    description.operands;
  (match (description.result, description.target_type) with
  | None, Some target_type ->
      Printf.bprintf buffer " type=%s" (type_name target_type)
  | Some _, _ | None, None -> ());
  Option.iter (add_payload buffer) description.payload;
  Printf.bprintf buffer " flags=0x%09Lx" description.flags;
  (match description.span with
  | None -> ()
  | Some span ->
      Printf.bprintf buffer " @source=%d:%d..%d"
        (Common.Source_id.to_int span.source)
        span.start span.stop);
  Buffer.add_char buffer '\n'

let human sequence =
  let buffer = Buffer.create 256 in
  Printf.bprintf buffer "holyc-ir-v1 reference=%s\n" reference_commit;
  List.iter (add_instruction buffer) sequence;
  Buffer.contents buffer

let human_body sequence =
  let buffer = Buffer.create 256 in
  List.iter (add_instruction buffer) sequence;
  Buffer.contents buffer
