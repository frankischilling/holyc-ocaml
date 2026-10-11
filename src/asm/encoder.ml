type error = { operand_index : int option; message : string }
type plan = { encoding : Encoding.plan }

type emission = {
  bytes : string;
  size : int;
  selection : Instruction.selection;
}

let max_code_bytes = min (16 * 1024 * 1024) Sys.max_string_length

exception Invalid of error

let invalid ?operand_index message = raise (Invalid { operand_index; message })

let check_limit max_bytes =
  if max_bytes <= 0 || max_bytes > max_code_bytes then
    invalid (Printf.sprintf "max_bytes must be between 1 and %d" max_code_bytes)

let flag form mask = form.Generated.Opcode_keywords.flags land mask <> 0

let operand_register operand =
  Option.fold ~none:(-1) ~some:Register.number (Operand.register1 operand)

let floating operand =
  Option.fold ~none:false
    ~some:(fun register -> Register.kind register = Register.Float_stack)
    (Operand.register1 operand)

let opcode_patch (selection : Instruction.selection) =
  let form = selection.form in
  if not (flag form 0x004) then None
  else
    let register operand_index operand =
      let number = operand_register operand in
      if number < 0 then
        invalid ~operand_index "selected opcode patch has no register operand";
      number land 7
    in
    match selection.operands with
    | [ operand ] ->
        if form.slash_value = 8 then
          Some (Encoding.Or_low3 (register 0 operand))
        else if floating operand then
          Some (Encoding.Add_low3 (register 0 operand))
        else None
    | [ first; second ] ->
        if form.slash_value = 8 then
          let number =
            if form.argument1 >= 32 && form.argument1 <= 35 then
              register 1 second
            else register 0 first
          in
          Some (Encoding.Or_low3 number)
        else if floating first || floating second then
          let number operand =
            if floating operand then operand_register operand else 0
          in
          Some (Encoding.Or_low3 (number first lor number second))
        else None
    | _ -> None

let check_known operand_index operand =
  match Operand.immediate operand with
  | Some { value = None; _ } ->
      invalid ~operand_index
        "assembler operand requires expression or symbol resolution before \
         byte emission"
  | Some { absolute_known = false; _ } ->
      invalid ~operand_index
        "assembler operand requires its address category before byte emission"
  | None | Some _ -> ()

let address_registers operand =
  List.filter_map
    (function
      | Operand.Register register -> (
          match Register.kind register with
          | Register.R16 | Register.R32 | Register.R64 -> Some register
          | _ -> None)
      | _ -> None)
    (Operand.tokens operand)

let synthetic_no_index operand =
  match (address_registers operand, Operand.register2 operand) with
  | [ original ], Some base ->
      Register.number original land 7 = 4
      && Register.number original = Register.number base
      && Register.kind original = Register.kind base
      && Operand.scale operand = 1L
  | _ -> false

let check_data_mode (selection : Instruction.selection) =
  (* R64, RM64 and the fixed RAX class describe integer data. M64 also describes
     x87 values and machine-state blocks whose width is defined by the opcode. *)
  if selection.mode <> Operand.Use64 then
    List.iteri
      (fun operand_index argument ->
        if argument = 15 || argument = 19 || argument = 35 then
          invalid ~operand_index
            "selected 64-bit integer operand requires USE64 for byte emission")
      [ selection.form.argument1; selection.form.argument2 ]

let check_registers (selection : Instruction.selection) operand_index operand =
  let check register =
    let number = Register.number register in
    let spelling = Register.spelling register in
    match Register.kind register with
    | Register.Mm | Register.Xmm ->
        invalid ~operand_index
          (Printf.sprintf
             "register %s has no encodable operand class in the pinned \
              instruction table"
             spelling)
    | Register.R8 | Register.R16 | Register.R32 | Register.R64 ->
        if selection.mode <> Operand.Use64 && number >= 8 then
          invalid ~operand_index
            (Printf.sprintf "register %s requires USE64 for byte emission"
               spelling);
        if Register.kind register = Register.R8 then
          if number >= 4 && number <= 7 && Option.is_some selection.rex then
            invalid ~operand_index
              (Printf.sprintf
                 "high-byte register %s cannot be encoded with a REX prefix"
                 spelling)
          else if number >= 20 && Option.is_none selection.rex then
            invalid ~operand_index
              (Printf.sprintf "byte register %s requires an emitted REX prefix"
                 spelling)
    | Register.Segment | Register.Float_stack -> ()
  in
  Option.iter check (Operand.register1 operand);
  Option.iter check (Operand.register2 operand);
  if Operand.indirect operand then (
    if not (List.mem (Operand.scale operand) [ 1L; 2L; 4L; 8L ]) then
      invalid ~operand_index
        "memory scale must be 1, 2, 4 or 8 for byte emission";
    if
      Option.is_none (Operand.register1 operand)
      && not (Operand.immediate_present operand)
    then
      invalid ~operand_index
        "memory address requires a base register, index register or numeric \
         address";
    if
      Option.is_some selection.sib
      && operand_register operand = 4
      && not (synthetic_no_index operand)
    then
      invalid ~operand_index
        "stack pointer cannot be an address index because its SIB code denotes \
         no index")

let prefixes (selection : Instruction.selection) =
  let segment =
    match selection.segment_prefix with
    | None -> []
    | Some index -> (
        match List.nth_opt [ 0x26; 0x2e; 0x36; 0x3e; 0x64; 0x65 ] index with
        | Some byte -> [ byte ]
        | None ->
            invalid
              "selected segment prefix is outside the pinned register table")
  in
  segment
  @ (if selection.operand_prefix then [ 0x66 ] else [])
  @ (if selection.address_prefix then [ 0x67 ] else [])
  @ Option.to_list selection.rex

let checked_end ~position bytes =
  let distance = Int64.of_int bytes in
  if position > Int64.sub Int64.max_int distance then
    invalid "instruction position plus encoded size overflows int64";
  Int64.add position distance

let relative_value ?operand_index ~target ~base () =
  if
    (base > 0L && target < Int64.add Int64.min_int base)
    || (base < 0L && target > Int64.add Int64.max_int base)
  then
    invalid ?operand_index
      "relative target minus instruction end overflows int64";
  Int64.sub target base

let numeric_value selection (field : Instruction.numeric_field) =
  match field.operand_index with
  | None -> 0L
  | Some operand_index -> (
      match List.nth_opt selection.Instruction.operands operand_index with
      | None ->
          invalid ~operand_index "selected numeric field has no source operand"
      | Some operand -> (
          match Operand.immediate operand with
          | None -> 0L
          | Some { value = Some value; _ } -> value
          | Some _ ->
              invalid ~operand_index
                "selected numeric field requires expression resolution before \
                 byte emission"))

let numeric_field selection ~base ~displacement
    (field : Instruction.numeric_field) =
  let value = numeric_value selection field in
  let value =
    if field.relative then
      relative_value ?operand_index:field.operand_index ~target:value ~base ()
    else value
  in
  let argument =
    match field.operand_index with
    | Some 0 -> selection.Instruction.form.argument1
    | Some 1 -> selection.Instruction.form.argument2
    | _ -> 0
  in
  let range =
    if field.relative then Encoding.Signed
    else if field.address then Encoding.Bit_pattern
    else if displacement || (argument >= 4 && argument <= 7) then
      Encoding.Signed
    else if argument >= 8 && argument <= 11 then Encoding.Unsigned
    else Encoding.Bit_pattern
  in
  match Encoding.field ~bytes:field.bytes ~range value with
  | Ok encoded -> encoded
  | Error detail ->
      let label =
        if field.relative then "relative target"
        else if displacement then "displacement"
        else "immediate"
      in
      invalid ?operand_index:field.operand_index
        (Printf.sprintf "%s out of range for selected form at OpCodes.DD:%d: %s"
           label selection.form.source_line detail)

let plan ?(max_bytes = max_code_bytes) ~position selection =
  try
    check_limit max_bytes;
    if selection.Instruction.encoded_bytes > max_bytes then
      invalid "selected instruction exceeds max_bytes";
    let instruction_end = checked_end ~position selection.encoded_bytes in
    List.iteri check_known selection.operands;
    check_data_mode selection;
    List.iteri (check_registers selection) selection.operands;
    let trailing_zero = flag selection.form 0x400 in
    (* PrsAsmInst supplies AsmStoreNum the end of displacement and immediate
       fields. ENTER's trailing zero is written afterward. *)
    let base =
      if trailing_zero then Int64.pred instruction_end else instruction_end
    in
    let displacement, immediate = Instruction.numeric_fields selection in
    let displacement =
      Option.map (numeric_field selection ~base ~displacement:true) displacement
    in
    let immediate =
      Option.map (numeric_field selection ~base ~displacement:false) immediate
    in
    match
      Encoding.plan ~mode:selection.mode ~form:selection.form
        ~prefixes:(prefixes selection) ?opcode_patch:(opcode_patch selection)
        ?modrm:selection.modrm ?sib:selection.sib ?displacement ?immediate
        ~trailing_zero ()
    with
    | Error message -> invalid message
    | Ok encoding ->
        if Encoding.size encoding <> selection.encoded_bytes then
          invalid
            "checked byte plan disagrees with selected source instruction size";
        Ok { encoding }
  with Invalid error -> Error error

let size plan = Encoding.size plan.encoding
let emit plan = Encoding.encode plan.encoding

let encode ?max_bytes ~position selection =
  match plan ?max_bytes ~position selection with
  | Error error -> Error error
  | Ok prepared -> Ok { bytes = emit prepared; size = size prepared; selection }

let encode_all ~max_bytes plans =
  try
    check_limit max_bytes;
    match
      Encoding.encode_all ~max_bytes
        (List.map (fun plan -> plan.encoding) plans)
    with
    | Ok bytes -> Ok bytes
    | Error message -> Error { operand_index = None; message }
  with Invalid error -> Error error
