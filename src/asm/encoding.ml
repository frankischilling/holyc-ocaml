module Facts = Generated.Opcode_keywords

type range = Signed | Unsigned | Bit_pattern
type field = { bytes : int; value : int64; range : range }

let fits bytes range value =
  if bytes = 8 then true
  else
    let bits = bytes * 8 in
    let signed_limit = Int64.shift_left 1L (bits - 1) in
    let signed_minimum = Int64.neg signed_limit in
    let signed_maximum = Int64.pred signed_limit in
    let unsigned_maximum = Int64.pred (Int64.shift_left 1L bits) in
    match range with
    | Signed -> value >= signed_minimum && value <= signed_maximum
    | Unsigned -> value >= 0L && value <= unsigned_maximum
    | Bit_pattern -> value >= signed_minimum && value <= unsigned_maximum

let field ~bytes ~range value =
  if bytes <> 1 && bytes <> 2 && bytes <> 4 && bytes <> 8 then
    Error "numeric field width must be one, two, four or eight bytes"
  else if not (fits bytes range value) then
    Error
      (Printf.sprintf "numeric field does not fit its %d-byte %s range" bytes
         (match range with
         | Signed -> "signed"
         | Unsigned -> "unsigned"
         | Bit_pattern -> "bit-pattern"))
  else Ok { bytes; value; range }

type opcode_patch = Or_low3 of int | Add_low3 of int

type plan = {
  prefixes : int list;
  opcodes : int list;
  modrm : int option;
  sib : int option;
  displacement : field option;
  immediate : field option;
  trailing_zero : bool;
  length : int;
}

let max_prefix_bytes = 4
let ( let* ) = Result.bind
let byte value = value >= 0 && value <= 255

let owned_forms =
  let table = Hashtbl.create 1024 in
  List.iter
    (fun (opcode : Facts.opcode) ->
      List.iter
        (fun (form : Facts.instruction) ->
          Hashtbl.add table form.source_line form)
        opcode.instructions)
    Facts.opcodes;
  table

let owned_form (form : Facts.instruction) =
  match Hashtbl.find_opt owned_forms form.source_line with
  | Some owned -> owned == form
  | None -> false

let flag (form : Facts.instruction) mask = form.flags land mask <> 0
let rm argument = argument >= 16 && argument <= 23
let moffs argument = argument >= 28 && argument <= 31
let immediate_kind argument = argument >= 4 && argument <= 11
let relative_kind argument = argument >= 1 && argument <= 3

let immediate_width argument =
  match argument with
  | 4 | 8 -> 1
  | 5 | 9 -> 2
  | 6 | 10 -> 4
  | _ -> 8

let expected_immediate (form : Facts.instruction) =
  let initial =
    match form.opcode_modifier with
    | 1 | 5 -> 1
    | 2 | 6 -> 2
    | 3 | 7 -> 4
    | _ -> 0
  in
  if form.argument2 = 0 then initial
  else if immediate_kind form.argument2 then immediate_width form.argument2
  else if
    immediate_kind form.argument1
    && (not (rm form.argument1 || rm form.argument2))
    && not (moffs form.argument1 || moffs form.argument2)
  then immediate_width form.argument1
  else initial

let validate_prefixes mode prefixes =
  let rec visit count groups = function
    | [] -> Ok count
    | prefix :: rest ->
        if count = max_prefix_bytes then
          Error "encoding has more than four separate prefix bytes"
        else if not (byte prefix) then Error "prefix must fit one byte"
        else
          let group =
            match prefix with
            | 0xf0 | 0xf2 | 0xf3 -> 1
            | 0x26 | 0x2e | 0x36 | 0x3e | 0x64 | 0x65 -> 2
            | 0x66 -> 4
            | 0x67 -> 8
            | value when value >= 0x40 && value <= 0x4f -> 16
            | _ -> 0
          in
          if group = 0 then Error "unrecognized instruction prefix"
          else if group land groups <> 0 then
            Error "instruction prefix group occurs more than once"
          else if group = 16 && mode <> Operand.Use64 then
            Error "REX prefix requires USE64"
          else if group = 16 && rest <> [] then
            Error "REX must be the final separate instruction prefix"
          else visit (count + 1) (groups lor group) rest
  in
  visit 0 0 prefixes

let opcode_bytes (form : Facts.instruction) patch =
  let rec patch_last = function
    | [] -> Error "generated instruction form has no opcode bytes"
    | [ last ] -> (
        match patch with
        | None -> Ok [ last ]
        | Some (Or_low3 register | Add_low3 register) -> (
            if register < 0 || register > 7 then
              Error "opcode register patch must fit three bits"
            else if not (flag form 0x004) then
              Error
                "opcode register patch requires a generated plus-opcode form"
            else if form.slash_value <> 8 && form.slash_value <> 9 then
              Error "generated form has no opcode register field"
            else if last land 7 <> 0 then
              Error "generated opcode register field is not clear"
            else
              match patch with
              | Some (Add_low3 _) when form.slash_value <> 9 ->
                  Error "additive opcode patch requires a floating-stack form"
              | Some (Add_low3 _) -> Ok [ last + register ]
              | Some (Or_low3 _) -> Ok [ last lor register ]
              | None -> assert false))
    | first :: rest ->
        let* rest = patch_last rest in
        Ok (first :: rest)
  in
  if not (List.for_all byte form.opcode_bytes) then
    Error "generated opcode value does not fit one byte"
  else patch_last form.opcode_bytes

let optional_byte label = function
  | Some value when not (byte value) -> Error (label ^ " must fit one byte")
  | _ -> Ok ()

let validate_width_prefixes mode (form : Facts.instruction) prefixes modrm =
  let operand_prefix =
    (mode = Operand.Use16 && flag form 0x002)
    || (mode <> Operand.Use16 && flag form 0x001)
  in
  if List.mem 0x66 prefixes <> operand_prefix then
    Error "operand-size prefix does not match the generated form and mode"
  else if mode <> Operand.Use64 then Ok ()
  else
    let rex =
      List.find_opt (fun prefix -> prefix >= 0x40 && prefix <= 0x4f) prefixes
      |> Option.value ~default:0
    in
    let qword argument = argument = 15 || argument = 19 || argument = 23 in
    let immediate64 =
      form.argument2 <> 0
      && (form.argument2 = 7 || form.argument2 = 11
         || (form.argument1 = 7 || form.argument1 = 11)
            && not (rm form.argument2 || moffs form.argument2))
    in
    let same_registers =
      Option.fold ~none:false
        ~some:(fun modrm ->
          modrm land 0xc0 = 0xc0 && (modrm lsr 3) land 7 = modrm land 7)
        modrm
    in
    (* Asm.HC:743-746 clears W for the unsigned imm32 forms. Lines 781-785
       suppress an otherwise unneeded REX only when no extension bit is set. *)
    let suppress_w =
      (flag form 0x080 && form.argument2 = 10)
      || rex land 7 = 0
         && (flag form 0x080 || (flag form 0x100 && same_registers))
    in
    let requires_w =
      (flag form 0x040 || qword form.argument1 || qword form.argument2
     || immediate64)
      && not suppress_w
    in
    if requires_w && rex land 8 = 0 then
      Error "generated qword form requires REX.W in USE64"
    else Ok ()

let field_bytes = function
  | None -> 0
  | Some field -> field.bytes

let validate_field label expected field =
  if field_bytes field <> expected then
    Error (Printf.sprintf "%s field requires exactly %d bytes" label expected)
  else
    match field with
    | Some field when not (fits field.bytes field.range field.value) ->
        Error (label ^ " field is outside its checked numeric range")
    | _ -> Ok ()

let displacement_width mode modrm sib (form : Facts.instruction) =
  match modrm with
  | None ->
      if moffs form.argument1 || moffs form.argument2 then
        if mode = Operand.Use16 then 2 else 4
      else 0
  | Some modrm -> (
      match modrm land 0xc0 with
      | 0x40 -> 1
      | 0x80 -> if mode = Operand.Use16 then 2 else 4
      | 0xc0 -> 0
      | _ -> (
          if modrm land 7 = 5 then 4
          else
            match sib with
            | Some sib when sib land 7 = 5 -> 4
            | _ -> 0))

let plan ~mode ~form ?(prefixes = []) ?opcode_patch ?modrm ?sib ?displacement
    ?immediate ?(trailing_zero = false) () =
  if not (owned_form form) then
    Error "instruction form is not owned by the pinned opcode table"
  else if form.opcode_bytes = [] then
    Error "generated instruction form has no opcode bytes"
  else if mode = Operand.Use64 && flag form 0x020 then
    Error "generated instruction form is unavailable in USE64"
  else
    let* prefix_bytes = validate_prefixes mode prefixes in
    if flag form 0x008 && (List.mem 0x66 prefixes || List.mem 0x67 prefixes)
    then Error "generated instruction form forbids a size override"
    else
      let* opcodes = opcode_bytes form opcode_patch in
      let* () = optional_byte "ModR/M" modrm in
      let* () = optional_byte "SIB" sib in
      let* () = validate_width_prefixes mode form prefixes modrm in
      let expects_modrm =
        form.slash_value < 8 || rm form.argument1 || rm form.argument2
      in
      if Option.is_some modrm <> expects_modrm then
        Error "ModR/M presence does not match the generated instruction form"
      else if
        Option.fold ~none:false
          ~some:(fun modrm ->
            form.slash_value < 8 && (modrm lsr 3) land 7 <> form.slash_value)
          modrm
      then Error "ModR/M extension does not match the generated slash value"
      else if
        Option.is_some sib
        && not
             (Option.fold ~none:false
                ~some:(fun modrm -> modrm land 0xc0 <> 0xc0 && modrm land 7 = 4)
                modrm)
      then Error "SIB requires a memory ModR/M with the SIB selector"
      else if
        Option.fold ~none:false
          ~some:(fun modrm -> modrm land 0xc0 <> 0xc0 && modrm land 7 = 4)
          modrm
        && Option.is_none sib
      then Error "memory ModR/M with the SIB selector requires a SIB byte"
      else if trailing_zero <> flag form 0x400 then
        Error "trailing zero does not match the generated instruction form"
      else
        let* () =
          validate_field "displacement"
            (displacement_width mode modrm sib form)
            displacement
        in
        let* () =
          validate_field "immediate" (expected_immediate form) immediate
        in
        let relative_immediate =
          relative_kind form.argument1 || relative_kind form.argument2
        in
        let relative_displacement =
          mode = Operand.Use64
          && Option.fold ~none:false
               ~some:(fun modrm -> modrm land 0xc7 = 5)
               modrm
        in
        let relative_fits relative = function
          | Some field when relative -> fits field.bytes Signed field.value
          | _ -> true
        in
        if not (relative_fits relative_immediate immediate) then
          Error "relative immediate must fit its signed field width"
        else if not (relative_fits relative_displacement displacement) then
          Error "RIP-relative displacement must fit its signed field width"
        else
          let length =
            prefix_bytes + List.length opcodes
            + (if Option.is_some modrm then 1 else 0)
            + (if Option.is_some sib then 1 else 0)
            + field_bytes displacement + field_bytes immediate
            + if trailing_zero then 1 else 0
          in
          Ok
            {
              prefixes;
              opcodes;
              modrm;
              sib;
              displacement;
              immediate;
              trailing_zero;
              length;
            }

let size plan = plan.length

let write buffer position plan =
  let put value =
    Bytes.set buffer !position (Char.chr value);
    incr position
  in
  let numeric field =
    for index = 0 to field.bytes - 1 do
      put
        (Int64.to_int
           (Int64.logand
              (Int64.shift_right_logical field.value (index * 8))
              0xffL))
    done
  in
  List.iter put plan.prefixes;
  List.iter put plan.opcodes;
  Option.iter put plan.modrm;
  Option.iter put plan.sib;
  Option.iter numeric plan.displacement;
  Option.iter numeric plan.immediate;
  if plan.trailing_zero then put 0

let emit length plans =
  let buffer = Bytes.create length in
  let position = ref 0 in
  List.iter (write buffer position) plans;
  Bytes.unsafe_to_string buffer

let encode plan = emit plan.length [ plan ]

let encode_all ~max_bytes plans =
  if max_bytes <= 0 || max_bytes > Sys.max_string_length then
    Error "max_bytes must be positive and fit the host string limit"
  else
    let rec measure total = function
      | [] -> Ok total
      | plan :: rest ->
          if plan.length > max_bytes - total then
            Error "encoded instructions exceed max_bytes"
          else measure (total + plan.length) rest
    in
    let* length = measure 0 plans in
    Ok (emit length plans)
