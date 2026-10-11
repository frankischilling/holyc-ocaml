module Operand = Holyc_lib__Asm.Operand
module Instruction = Holyc_lib__Asm.Instruction
module Encoder = Holyc_lib__Asm.Encoder
module Opcode = Holyc_lib__Asm.Opcode
module Register = Holyc_lib__Asm.Register
module Database = Holyc_lib__Generated.Opcode_keywords

let register spelling =
  match Register.find spelling with
  | Some value -> Operand.Register value
  | None -> Alcotest.failf "missing original register %s" spelling

let integer value =
  if value < 0L && value <> Int64.min_int then
    [ Operand.Operator "-"; Operand.Integer (Int64.neg value) ]
  else [ Operand.Integer value ]

let memory ?(width = 8) ?segment inner =
  [ Operand.Size width ]
  @ (match segment with
    | None -> []
    | Some spelling -> [ register spelling; Operand.Punctuation ':' ])
  @ [ Operand.Punctuation '[' ]
  @ inner
  @ [ Operand.Punctuation ']' ]

let base ?width ?segment spelling = memory ?width ?segment [ register spelling ]

let displaced ?width ?segment spelling displacement =
  memory ?width ?segment
    ([ register spelling; Operand.Operator "+" ] @ integer displacement)

let mode = function
  | 16 -> Operand.Use16
  | 32 -> Operand.Use32
  | 64 -> Operand.Use64
  | value -> Alcotest.failf "unsupported fixture mode %d" value

let hex bytes =
  let result = Buffer.create (2 * String.length bytes) in
  String.iter
    (fun byte ->
      Buffer.add_string result (Printf.sprintf "%02x" (Char.code byte)))
    bytes;
  Buffer.contents result

let contains text fragment =
  let rec at offset =
    offset + String.length fragment <= String.length text
    && (String.sub text offset (String.length fragment) = fragment
       || at (offset + 1))
  in
  at 0

let resolved spelling =
  match Opcode.resolve spelling with
  | Some opcode -> opcode
  | None -> Alcotest.failf "missing original opcode %s" spelling

let selected_form spelling source_line =
  Opcode.forms (Opcode.resolved_opcode (resolved spelling))
  |> List.find_opt (fun (form : Database.instruction) ->
      form.source_line = source_line)
  |> function
  | Some form -> form
  | None ->
      Alcotest.failf "missing %s form at OpCodes.DD:%d" spelling source_line

let selection_result ?(mode = Operand.Use64) ?source_line spelling arguments =
  let opcode = resolved spelling in
  let explicit = Option.map (selected_form spelling) source_line in
  let relative_arguments =
    match explicit with
    | Some form ->
        List.map
          (fun argument -> argument >= 1 && argument <= 3)
          [ form.argument1; form.argument2 ]
    | None ->
        Opcode.first_form_relative_arguments (Opcode.resolved_opcode opcode)
  in
  let operands =
    List.mapi
      (fun index tokens ->
        let relative =
          Option.value ~default:false (List.nth_opt relative_arguments index)
        in
        match Operand.parse ~mode ~relative tokens with
        | Ok operand -> operand
        | Error error ->
            Alcotest.failf "%s operand %d token %d: %s" spelling index
              error.token_index error.message)
      arguments
  in
  match explicit with
  | None -> Instruction.select ~mode ~opcode ~operands
  | Some form ->
      Instruction.select_form ~mode ~opcode ~operands
        ~entry_index:form.entry_index

let selected ?mode ?source_line spelling arguments =
  match selection_result ?mode ?source_line spelling arguments with
  | Ok selection -> selection
  | Error error -> Alcotest.failf "%s selection: %s" spelling error.message

let encode ?max_bytes ?(position = 0L) selection =
  match Encoder.encode ?max_bytes ~position selection with
  | Ok emission -> emission
  | Error error -> Alcotest.failf "emission: %s" error.message

let check_bytes ?mode ?source_line ?position label spelling arguments expected =
  let selection = selected ?mode ?source_line spelling arguments in
  let emission =
    encode ?position ~max_bytes:(String.length expected / 2) selection
  in
  Alcotest.(check string) label expected (hex emission.bytes);
  Alcotest.(check int)
    (label ^ " size")
    (String.length expected / 2)
    emission.size

let require_error label = function
  | Error _ -> ()
  | Ok _ -> Alcotest.failf "%s unexpectedly succeeded" label

let emission_error ?mode ?source_line ?(position = 0L) label fragment spelling
    arguments =
  let selection = selected ?mode ?source_line spelling arguments in
  match Encoder.encode ~position selection with
  | Error error ->
      Alcotest.(check bool)
        (label ^ ": " ^ error.message)
        true
        (contains error.message fragment)
  | Ok emission -> Alcotest.failf "%s emitted %s" label (hex emission.bytes)

type fixture = {
  source_line : int;
  mnemonic : string;
  entry_index : int;
  mode : Operand.mode;
  argument1 : string;
  argument2 : string;
  operand1 : string;
  operand2 : string;
  expected : string;
}

let fixture_path () =
  let local = "fixtures/assembly-encoder-forms.tsv" in
  if Sys.file_exists local then local else Filename.concat "test" local

let fixtures () =
  let channel = open_in (fixture_path ()) in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () ->
      let rec read rows =
        match input_line channel with
        | line -> (
            let line = String.trim line in
            if line = "" || String.starts_with ~prefix:"#" line then read rows
            else
              match String.split_on_char '\t' line with
              | [
               source_line;
               mnemonic;
               entry_index;
               bits;
               argument1;
               argument2;
               operand1;
               operand2;
               expected;
              ] ->
                  read
                    ({
                       source_line = int_of_string source_line;
                       mnemonic;
                       entry_index = int_of_string entry_index;
                       mode = mode (int_of_string bits);
                       argument1;
                       argument2;
                       operand1;
                       operand2;
                       expected;
                     }
                    :: rows)
              | _ -> Alcotest.failf "malformed fixed encoder fixture: %s" line)
        | exception End_of_file -> List.rev rows
      in
      read [])

let fixture_operand text =
  match String.split_on_char ':' text with
  | [ "register"; spelling ] -> [ register spelling ]
  | [ "imm"; value ] -> integer (Int64.of_string value)
  | [ "memory"; width; spelling ] -> base ~width:(int_of_string width) spelling
  | [ "address"; width; value ] ->
      memory ~width:(int_of_string width) (integer (Int64.of_string value))
  | _ -> Alcotest.failf "unknown fixture operand %s" text

let argument_number = function
  | "-" -> 0
  | "REL8" -> 1
  | "REL16" -> 2
  | "REL32" -> 3
  | "IMM8" -> 4
  | "IMM16" -> 5
  | "IMM32" -> 6
  | "IMM64" -> 7
  | "UIMM8" -> 8
  | "UIMM16" -> 9
  | "UIMM32" -> 10
  | "UIMM64" -> 11
  | "R8" -> 12
  | "R16" -> 13
  | "R32" -> 14
  | "R64" -> 15
  | "RM8" -> 16
  | "RM16" -> 17
  | "RM32" -> 18
  | "RM64" -> 19
  | "M8" -> 20
  | "M16" -> 21
  | "M32" -> 22
  | "M64" -> 23
  | "MOFFS8" -> 28
  | "MOFFS16" -> 29
  | "MOFFS32" -> 30
  | "MOFFS64" -> 31
  | "AL" -> 32
  | "AX" -> 33
  | "EAX" -> 34
  | "RAX" -> 35
  | "CL" -> 36
  | "DX" -> 37
  | "SREG" -> 39
  | "SS" -> 40
  | "DS" -> 41
  | "ES" -> 42
  | "FS" -> 43
  | "GS" -> 44
  | "CS" -> 45
  | "ST0" -> 46
  | "STI" -> 47
  | unknown -> Alcotest.failf "unknown original argument class %s" unknown

let inventory () =
  let cases = fixtures () in
  let key mnemonic entry_index source_line =
    Printf.sprintf "%s:%d:%d" mnemonic entry_index source_line
  in
  let expected_keys =
    List.map
      (fun case -> key case.mnemonic case.entry_index case.source_line)
      cases
    |> List.sort String.compare
  in
  let actual_keys =
    Opcode.all
    |> List.concat_map (fun opcode ->
        List.map
          (fun (form : Database.instruction) ->
            key (Opcode.spelling opcode) form.entry_index form.source_line)
          (Opcode.forms opcode))
    |> List.sort String.compare
  in
  Alcotest.(check int) "fixed original record count" 924 (List.length cases);
  Alcotest.(check int)
    "unique fixed identities" 924
    (List.length (List.sort_uniq String.compare expected_keys));
  Alcotest.(check (list string))
    "every generated form has a fixed specimen" expected_keys actual_keys;
  let rejected = ref [] and failures = ref [] in
  List.iter
    (fun case ->
      let label =
        Printf.sprintf "%s at OpCodes.DD:%d" case.mnemonic case.source_line
      in
      try
        let form = selected_form case.mnemonic case.source_line in
        Alcotest.(check int)
          (label ^ " index") case.entry_index form.entry_index;
        Alcotest.(check int)
          (label ^ " argument 1")
          (argument_number case.argument1)
          form.argument1;
        Alcotest.(check int)
          (label ^ " argument 2")
          (argument_number case.argument2)
          form.argument2;
        let operands =
          [ case.operand1; case.operand2 ]
          |> List.filter (fun operand -> operand <> "-")
          |> List.map fixture_operand
        in
        if case.expected = "!empty-opcode" then (
          rejected := case.source_line :: !rejected;
          require_error label
            (selection_result ~mode:case.mode ~source_line:case.source_line
               case.mnemonic operands))
        else (
          check_bytes ~mode:case.mode ~source_line:case.source_line label
            case.mnemonic operands case.expected;
          let selection =
            selected ~mode:case.mode ~source_line:case.source_line case.mnemonic
              operands
          in
          require_error
            (label ^ " one below output cap")
            (Encoder.plan
               ~max_bytes:((String.length case.expected / 2) - 1)
               ~position:0L selection))
      with error ->
        failures := (label ^ ": " ^ Printexc.to_string error) :: !failures)
    cases;
  Alcotest.(check (list int))
    "only the malformed original empty record" [ 920 ] !rejected;
  Alcotest.(check (list string))
    "all original specimen bytes, sizes, and caps" [] (List.rev !failures)

let output_limits () =
  let first = selected "MOV" [ [ register "RAX" ]; integer 42L ] in
  let second = selected "RET" [] in
  let plan_at position selection =
    match Encoder.plan ~max_bytes:6 ~position selection with
    | Ok plan -> plan
    | Error error -> Alcotest.fail error.message
  in
  let a = plan_at 0L first and b = plan_at 5L second in
  Alcotest.(check int) "source immediate move length" 5 (Encoder.size a);
  Alcotest.(check int) "return length" 1 (Encoder.size b);
  Alcotest.(check string)
    "planned move bytes" "b82a000000"
    (hex (Encoder.emit a));
  (match Encoder.encode_all ~max_bytes:6 [ a; b ] with
  | Ok bytes ->
      Alcotest.(check string) "aggregate exact cap" "b82a000000c3" (hex bytes)
  | Error error -> Alcotest.fail error.message);
  require_error "aggregate one below" (Encoder.encode_all ~max_bytes:5 [ a; b ]);
  require_error "one instruction one below"
    (Encoder.plan ~max_bytes:4 ~position:0L first);
  List.iter
    (fun max_bytes ->
      require_error "invalid output cap"
        (Encoder.plan ~max_bytes ~position:0L second);
      require_error "invalid aggregate cap"
        (Encoder.encode_all ~max_bytes [ b ]))
    [ -1; 0; Encoder.max_code_bytes + 1 ];
  match Encoder.encode_all ~max_bytes:1 [] with
  | Ok bytes -> Alcotest.(check string) "empty validated stream" "" bytes
  | Error error -> Alcotest.fail error.message

let immediate_boundaries () =
  let signed mode source_line spelling minimum maximum expected_min expected_max
      =
    check_bytes ~mode ~source_line "signed minimum" "MOV"
      [ [ register spelling ]; integer minimum ]
      expected_min;
    check_bytes ~mode ~source_line "signed maximum" "MOV"
      [ [ register spelling ]; integer maximum ]
      expected_max;
    if minimum <> Int64.min_int then
      require_error "signed value below selected class"
        (selection_result ~mode ~source_line "MOV"
           [ [ register spelling ]; integer (Int64.pred minimum) ]);
    if maximum <> Int64.max_int then
      require_error "signed value above selected class"
        (selection_result ~mode ~source_line "MOV"
           [ [ register spelling ]; integer (Int64.succ maximum) ])
  in
  signed Operand.Use32 269 "DL" (-128L) 127L "b280" "b27f";
  signed Operand.Use32 271 "DX" (-32768L) 32767L "66ba0080" "66baff7f";
  signed Operand.Use32 273 "EDX" (-2147483648L) 2147483647L "ba00000080"
    "baffffff7f";
  signed Operand.Use64 276 "RDX" Int64.min_int Int64.max_int
    "48ba0000000000000080" "48baffffffffffffff7f";
  let unsigned source_line spelling maximum expected =
    check_bytes ~mode:Operand.Use32 ~source_line "unsigned maximum" "MOV"
      [ [ register spelling ]; integer maximum ]
      expected;
    require_error "unsigned value above selected class"
      (selection_result ~mode:Operand.Use32 ~source_line "MOV"
         [ [ register spelling ]; integer (Int64.succ maximum) ]);
    require_error "negative value outside unsigned class"
      (selection_result ~mode:Operand.Use32 ~source_line "MOV"
         [ [ register spelling ]; integer (-1L) ])
  in
  unsigned 268 "DL" 255L "b2ff";
  unsigned 270 "DX" 65535L "66baffff";
  unsigned 272 "EDX" 4294967295L "baffffffff";
  check_bytes ~source_line:275 "unsigned qword high bit" "MOV"
    [ [ register "RDX" ]; integer Int64.min_int ]
    "48ba0000000000000080";
  check_bytes ~source_line:275 "unsigned all-one qword" "MOV"
    [ [ register "RDX" ]; [ Operand.Integer (-1L) ] ]
    "48baffffffffffffffff";
  check_bytes ~source_line:283 "original qword UIMM32 REX suppression" "MOV"
    [ base "RBX"; integer 4294967295L ]
    "c703ffffffff"

let relative_boundaries () =
  List.iter
    (fun (target, expected) ->
      check_bytes ~source_line:582 "relative byte edge" "JMP"
        [ integer target ]
        expected)
    [ (129L, "eb7f"); (-126L, "eb80") ];
  List.iter
    (fun target ->
      emission_error ~source_line:582 "relative byte outside range" "range"
        "JMP"
        [ integer target ])
    [ 130L; -127L ];
  check_bytes ~mode:Operand.Use16 ~source_line:583 "relative word maximum" "JMP"
    [ integer 32770L ]
    "e9ff7f";
  check_bytes ~mode:Operand.Use16 ~source_line:583 "relative word minimum" "JMP"
    [ integer (-32765L) ]
    "e90080";
  emission_error ~mode:Operand.Use16 ~source_line:583
    "relative word outside range" "range" "JMP"
    [ integer 32771L ];
  check_bytes ~mode:Operand.Use32 ~source_line:584 "relative dword maximum"
    "JMP"
    [ integer 2147483652L ]
    "e9ffffff7f";
  check_bytes ~mode:Operand.Use32 ~source_line:584 "relative dword minimum"
    "JMP"
    [ integer (-2147483643L) ]
    "e900000080";
  emission_error ~mode:Operand.Use32 ~source_line:584
    "relative dword outside range" "range" "JMP"
    [ integer 2147483653L ];
  check_bytes ~source_line:582 ~position:(-10L) "signed low-level position"
    "JMP"
    [ integer 0L ]
    "eb08";
  emission_error ~position:Int64.max_int "instruction end overflow" "overflows"
    "NOP" [];
  emission_error ~source_line:582 "relative subtraction overflow" "overflows"
    "JMP"
    [ integer Int64.min_int ];
  check_bytes ~source_line:261 ~position:4096L "RIP-relative source target"
    "MOV"
    [ [ register "RAX" ]; memory (integer 305419896L) ]
    "488b0571463412";
  check_bytes ~source_line:261 "RIP-relative maximum" "MOV"
    [ [ register "RAX" ]; memory (integer 2147483654L) ]
    "488b05ffffff7f";
  emission_error ~source_line:261 "RIP-relative outside range" "range" "MOV"
    [ [ register "RAX" ]; memory (integer 2147483655L) ]

let displacement_boundaries () =
  List.iter
    (fun (value, expected) ->
      check_bytes ~source_line:261 "base displacement edge" "MOV"
        [ [ register "RAX" ]; displaced "RBX" value ]
        expected)
    [
      (127L, "488b437f");
      (128L, "488b8380000000");
      (-128L, "488b4380");
      (-129L, "488b837fffffff");
      (2147483647L, "488b83ffffff7f");
      (-2147483648L, "488b8300000080");
    ];
  List.iter
    (fun value ->
      emission_error ~source_line:261 "signed base displacement overflow"
        "range" "MOV"
        [ [ register "RAX" ]; displaced "RBX" value ])
    [ 2147483648L; -2147483649L ];
  check_bytes ~mode:Operand.Use32 ~source_line:260
    "absolute address bit pattern" "MOV"
    [ [ register "EDX" ]; memory ~width:4 (integer 4294967295L) ]
    "8b15ffffffff";
  emission_error ~mode:Operand.Use32 ~source_line:260
    "absolute address exceeds dword" "range" "MOV"
    [ [ register "EDX" ]; memory ~width:4 (integer 4294967296L) ];
  check_bytes ~mode:Operand.Use16 ~source_line:253 "moffs word maximum" "MOV"
    [ [ register "AX" ]; memory ~width:2 (integer 65535L) ]
    "a1ffff";
  check_bytes ~mode:Operand.Use16 ~source_line:253 "moffs signed bit pattern"
    "MOV"
    [ [ register "AX" ]; memory ~width:2 (integer (-32768L)) ]
    "a10080";
  emission_error ~mode:Operand.Use16 ~source_line:253
    "moffs address exceeds word" "range" "MOV"
    [ [ register "AX" ]; memory ~width:2 (integer 65536L) ];
  emission_error ~mode:Operand.Use16 ~source_line:253
    "negative moffs outside word pattern" "range" "MOV"
    [ [ register "AX" ]; memory ~width:2 (integer (-32769L)) ];
  check_bytes ~mode:Operand.Use16 ~source_line:259
    "original USE16 address-prefix displacement width" "MOV"
    [ [ register "AX" ]; displaced ~width:2 "EBX" 128L ]
    "678b838000";
  emission_error ~mode:Operand.Use16 ~source_line:259
    "USE16 displacement retains signed word bound" "range" "MOV"
    [ [ register "AX" ]; displaced ~width:2 "EBX" 32768L ]

let address_and_register_forms () =
  List.iter
    (fun (spelling, expected) ->
      check_bytes "base and synthetic no-index address" "MOV"
        [ [ register "RAX" ]; base spelling ]
        expected)
    [
      ("RBP", "488b4500");
      ("RSP", "488b0424");
      ("R12", "498b0424");
      ("R13", "498b4500");
    ];
  check_bytes "extended index and base" "MOV"
    [
      [ register "RAX" ];
      memory
        [
          register "RBX";
          Operand.Operator "+";
          register "R12";
          Operand.Operator "*";
          Operand.Integer 4L;
          Operand.Operator "+";
          Operand.Integer 16L;
        ];
    ]
    "4a8b44a310";
  check_bytes "FS then REX with all extension fields" "MOV"
    [
      [ register "R9" ];
      memory ~segment:"FS"
        [
          register "R13";
          Operand.Operator "+";
          register "R12";
          Operand.Operator "*";
          Operand.Integer 8L;
          Operand.Operator "+";
          Operand.Integer 16L;
        ];
    ]
    "644f8b4ce510";
  check_bytes ~mode:Operand.Use16 ~source_line:260
    "segment then operand then address prefixes" "MOV"
    [ [ register "EDX" ]; base ~width:4 ~segment:"FS" "EBX" ]
    "6466678b13";
  check_bytes "stack pointer as genuine base" "MOV"
    [
      [ register "RAX" ];
      memory [ register "RAX"; Operand.Operator "+"; register "RSP" ];
    ]
    "488b0404";
  check_bytes "R12 is a genuine scaled index" "MOV"
    [
      [ register "RAX" ];
      memory
        [
          register "RAX";
          Operand.Operator "+";
          register "R12";
          Operand.Operator "*";
          Operand.Integer 4L;
        ];
    ]
    "4a8b04a0";
  check_bytes "high byte without REX" "MOV"
    [ [ register "AH" ]; [ register "AL" ] ]
    "8ae0";
  check_bytes "low byte requiring REX" "MOV"
    [ [ register "RSPu8" ]; [ register "AL" ] ]
    "408ae0"

let rejected_shapes () =
  List.iter
    (fun scale ->
      emission_error "source scale outside encodable powers" "scale" "MOV"
        [
          [ register "RAX" ];
          memory [ register "RBX"; Operand.Operator "*"; Operand.Integer scale ];
        ])
    [ 0L; 3L; 16L ];
  emission_error "empty original memory shape" "address requires" "MOV"
    [ [ register "RAX" ]; memory [] ];
  emission_error "ignored stack-pointer index" "index" "MOV"
    [
      [ register "RAX" ];
      memory
        [
          register "RBX";
          Operand.Operator "+";
          register "RSP";
          Operand.Operator "*";
          Operand.Integer 2L;
        ];
    ];
  List.iter
    (fun spelling ->
      emission_error "unimplemented vector operand class" "operand class" "MOV"
        [ [ register "RAX" ]; [ register spelling ] ])
    [ "MM0"; "XMM0" ];
  emission_error "high byte conflicts with register extension" "REX" "MOV"
    [ [ register "AH" ]; [ register "R8u8" ] ];
  emission_error "high byte conflicts with address extension" "REX" "MOV"
    [ [ register "AH" ]; base ~width:1 "R8" ];
  emission_error ~mode:Operand.Use32 "extended register outside USE64" "USE64"
    "MOV"
    [ [ register "R8u32" ]; integer 1L ];
  List.iter
    (fun mode ->
      emission_error ~mode "qword data register outside USE64" "USE64" "MOV"
        [ [ register "RAX" ]; [ register "RBX" ] ])
    [ Operand.Use16; Operand.Use32 ];
  List.iter
    (fun (mode, prefix) ->
      emission_error ~mode "qword integer memory outside USE64" "USE64" "INC"
        [ base "EAX" ];
      emission_error ~mode "qword immediate store outside USE64" "USE64" "MOV"
        [ base "EAX"; integer 1L ];
      check_bytes ~mode "x87 qword load retains mode" "FLD"
        [ base "EAX" ]
        (prefix ^ "dd00");
      check_bytes ~mode "x87 integer qword load retains mode" "FILD"
        [ base "EAX" ]
        (prefix ^ "df28"))
    [ (Operand.Use16, "67"); (Operand.Use32, "") ];
  check_bytes "qword data registers in USE64" "MOV"
    [ [ register "RAX" ]; [ register "RBX" ] ]
    "488bc3";
  check_bytes ~mode:Operand.Use32
    "address spelling retains original mode semantics" "MOV"
    [ [ register "EAX" ]; base ~width:4 "RBX" ]
    "8b03";
  emission_error ~mode:Operand.Use16 "extended address outside USE64" "USE64"
    "MOV"
    [ [ register "AX" ]; base ~width:2 "R8u32" ];
  emission_error ~mode:Operand.Use32 "low byte outside USE64" "USE64" "MOV"
    [ [ register "RSPu8" ]; [ register "AL" ] ];
  emission_error "unresolved immediate" "resolution" "MOV"
    [ [ register "RAX" ]; [ Operand.Symbol "@@missing" ] ];
  emission_error "unresolved memory target" "resolution" "MOV"
    [ [ register "RAX" ]; memory [ Operand.Symbol "@@missing" ] ];
  emission_error "unresolved address category" "resolution" "MOV"
    [
      [ register "RAX" ];
      [ Operand.Address_symbol { name = "export"; absolute = None } ];
    ]

let x87_aliases_and_original_choices () =
  List.iter
    (fun (spelling, operands, expected) ->
      check_bytes "original x87 opcode patch" spelling operands expected)
    [
      ("FLD", [ [ register "ST7" ] ], "d9c7");
      ("FSTP", [ [ register "ST7" ] ], "dddf");
      ("FXCH", [ [ register "ST7" ] ], "d9cf");
      ("FADDP", [ [ register "ST7" ]; [ register "ST0" ] ], "dec7");
      ("FADD", [ [ register "ST0" ]; [ register "ST7" ] ], "d8c7");
      ("FADD", [ [ register "ST7" ]; [ register "ST0" ] ], "dcc7");
    ];
  check_bytes "ENTER appends the source zero" "ENTER" [ integer 42L ] "c82a0000";
  check_bytes "source REX XOR omission" "XOR"
    [ [ register "RAX" ]; [ register "RAX" ] ]
    "33c0";
  check_bytes "different XOR registers retain REX.W" "XOR"
    [ [ register "RAX" ]; [ register "RCX" ] ]
    "4833c1";
  check_bytes ~mode:Operand.Use32 "source REP_LODSB uses F2" "REP_LODSB" []
    "f2ac";
  check_bytes ~mode:Operand.Use32 "original repeat prefix order" "REP_MOVSW" []
    "66f3a5";
  let alias = selected "JZ" [ integer 42L ] in
  Alcotest.(check bool)
    "alias identity remains" true
    (Opcode.resolved_is_alias alias.opcode);
  Alcotest.(check string) "alias bytes" "7428" (hex (encode alias).bytes);
  let opcode = resolved "RET" in
  List.iter
    (fun entry_index ->
      require_error "explicit form outside original opcode"
        (Instruction.select_form ~mode:Operand.Use64 ~opcode ~operands:[]
           ~entry_index))
    [ -1; 1; max_int ]

let tests =
  [
    Alcotest.test_case "all 924 original instruction records" `Quick inventory;
    Alcotest.test_case "output limits and aggregate preflight" `Quick
      output_limits;
    Alcotest.test_case "signed and unsigned immediate boundaries" `Quick
      immediate_boundaries;
    Alcotest.test_case "relative field bounds and address overflow" `Quick
      relative_boundaries;
    Alcotest.test_case "base displacement and absolute address bounds" `Quick
      displacement_boundaries;
    Alcotest.test_case "prefixes SIB and byte-register roles" `Quick
      address_and_register_forms;
    Alcotest.test_case "explicit unresolved and malformed-shape refusals" `Quick
      rejected_shapes;
    Alcotest.test_case "x87 aliases and original source choices" `Quick
      x87_aliases_and_original_choices;
  ]
