module Facts = Generated.Opcode_keywords

type symbol = { name : string; address : int64 }

type operand = {
  argument_type : int;
  size : int;
  text : string;
  value : int64 option;
  target : int64 option;
}

type instruction = {
  offset : int;
  address : int64;
  bytes : string;
  opcode : string;
  form : Facts.instruction;
  prefixes : int list;
  operands : operand list;
  jump_target : int64 option;
}

type error = { offset : int; message : string }

type t = {
  mode_ : Operand.mode;
  position_ : int64;
  byte_count_ : int;
  symbols : symbol list;
  instructions_ : instruction list;
}

let max_input_bytes = min (16 * 1024 * 1024) Sys.max_string_length
let instructions t = t.instructions_
let mode t = t.mode_
let position t = t.position_
let byte_count t = t.byte_count_
let flag (form : Facts.instruction) mask = form.flags land mask <> 0
let between low high n = n >= low && n <= high
let rm n = between 16 23 n
let memory n = between 20 23 n
let immediate n = between 4 11 n
let signed n = between 1 7 n

let ins64 n =
  Int64.logand 0x0880888880L (Int64.shift_left 1L n) <> 0L || n = 46 || n = 47

exception Invalid of error

let invalid offset message = raise (Invalid { offset; message })

let add offset left right =
  if
    (right > 0L && left > Int64.sub Int64.max_int right)
    || (right < 0L && left < Int64.sub Int64.min_int right)
  then invalid offset "decoded address arithmetic overflows int64";
  Int64.add left right

(* Keep equal records equal: their order depends on the original hash traversal
   and QSortI64 swaps, and affects InstEntryFind's binary search. *)
let compare_entries (_, (a : Facts.instruction)) (_, (b : Facts.instruction)) =
  let opcode form =
    if flag form 0x200 then
      List.filteri
        (fun i _ -> i < List.length form.opcode_bytes - 1)
        form.opcode_bytes
    else form.opcode_bytes
  in
  let rec first = function
    | [] -> 0
    | x :: xs -> if x = 0 then first xs else x
  in
  first
    [
      compare (opcode a) (opcode b);
      (if flag a 0x200 && flag b 0x200 then
         compare (List.rev a.opcode_bytes) (List.rev b.opcode_bytes)
       else 0);
      compare (flag a 0x200) (flag b 0x200);
      compare a.slash_value b.slash_value;
      compare (flag a 0x002) (flag b 0x002);
      compare
        (ins64 a.argument1 || ins64 a.argument2)
        (ins64 b.argument1 || ins64 b.argument2);
      compare (flag a 0x040) (flag b 0x040);
      compare
        (a.argument2 = 7 || a.argument2 = 11)
        (b.argument2 = 7 || b.argument2 = 11);
    ]

let tables =
  let buckets = Array.make 1024 [] in
  List.iter
    (fun (opcode : Facts.opcode) ->
      let bucket =
        Int64.to_int
          (Int64.logand
             (Common.Native_hash_record.hash_string opcode.spelling)
             1023L)
      in
      buckets.(bucket) <- opcode :: buckets.(bucket))
    Facts.opcodes;
  let entries =
    Array.to_list buckets
    |> List.concat_map
         (List.concat_map (fun (opcode : Facts.opcode) ->
              List.map (fun form -> (opcode.spelling, form)) opcode.instructions))
  in
  let sorted entries =
    let array = Array.of_list entries in
    let rec sort start count =
      if count > 1 then (
        let left = ref start and right = ref (start + count - 1) in
        let pivot = array.(start + (count / 2)) in
        let rec partition () =
          while compare_entries array.(!left) pivot < 0 do
            incr left
          done;
          while compare_entries array.(!right) pivot > 0 do
            decr right
          done;
          if !left <= !right then (
            let saved = array.(!left) in
            array.(!left) <- array.(!right);
            array.(!right) <- saved;
            incr left;
            decr right);
          if !left <= !right then partition ()
        in
        partition ();
        let low = !right + 1 - start and high = start + count - !left in
        if low > 1 && low < count then sort start low;
        if high > 1 && high < count then sort !left high)
    in
    sort 0 (Array.length array);
    array
  in
  ( sorted entries,
    sorted (List.filter (fun (_, form) -> not (flag form 0x020)) entries) )

let find_entry table read opsize =
  let compare (_, (form : Facts.instruction)) =
    let opcodes = Array.of_list form.opcode_bytes in
    let count = Array.length opcodes in
    let prefix_count = count - if flag form 0x200 then 1 else 0 in
    let rec prefix n =
      if n < prefix_count then
        let byte = read n in
        let byte =
          if n = count - 1 && flag form 0x004 then byte land 0xf8 else byte
        in
        let difference = byte - opcodes.(n) in
        if difference <> 0 then difference else prefix (n + 1)
      else
        let width () =
          if opsize = 16 && flag form 0x002 then -1
          else if opsize <> 16 && flag form 0x001 then 1
          else if opsize = 64 || form.argument1 = 23 || form.argument2 = 23 then
            if
              ins64 form.argument1 || ins64 form.argument2 || flag form 0x040
              || form.uasm_slash_value >= 8 && form.argument2 = 0
                 && immediate form.argument1
            then 0
            else 1
          else if
            ins64 form.argument1 || ins64 form.argument2 || flag form 0x040
          then -1
          else 0
        in
        match form.uasm_slash_value with
        | slash when slash < 8 ->
            let byte = read n in
            let difference = ((byte lsr 3) land 7) - form.slash_value in
            if
              (memory form.argument1 || memory form.argument2)
              && byte land 0xc0 = 0xc0
            then 1
            else if difference <> 0 then difference
            else width ()
        | 9 -> (read n lsr 3) - (opcodes.(count - 1) lsr 3)
        | 10 -> read n - opcodes.(count - 1)
        | _ -> width ()
    in
    prefix 0
  in
  let rec search low high =
    let middle = (low + high) / 2 in
    let difference = compare table.(middle) in
    if difference > 0 then
      if middle = low then table.(high) else search middle high
    else if difference < 0 then
      if middle - low <= 1 then table.(low) else search low middle
    else table.(middle)
  in
  search 0 (Array.length table - 1)

let register size rex number =
  let family =
    match size with
    | 8 -> Facts.R8
    | 16 -> Facts.R16
    | 32 -> Facts.R32
    | _ -> Facts.R64
  in
  let number =
    if size = 8 && rex && between 4 7 number then number + 16 else number
  in
  match
    List.find_opt
      (fun (r : Facts.register) ->
        r.register_kind = family && r.register_number = number)
      Facts.registers
  with
  | Some r -> r.spelling
  | None -> Printf.sprintf "<register:%d:%d>" size number

let fixed_register = function
  | 32 -> "AL"
  | 33 -> "AX"
  | 34 -> "EAX"
  | 35 -> "RAX"
  | 36 -> "CL"
  | 37 -> "DX"
  | 40 -> "SS"
  | 41 -> "DS"
  | 42 -> "ES"
  | 43 -> "FS"
  | 44 -> "GS"
  | 45 -> "CS"
  | 46 -> "ST0"
  | _ -> ""

(* Initial ST_INT_NAMES, Kernel/KDefine.HC:118-124. *)
let interrupt_names =
  [
    "Divide Error";
    "SingleStep";
    "NMI";
    "Breakpoint";
    "Overflow";
    "BOUND Range Exceeded";
    "Invalid Opcode";
    "No Math Coprocessor";
    "Double Fault";
    "Coprocessor Segment Fault";
    "Invalid TASK";
    "Segment Not Present";
    "Stk Segment Fault";
    "General Protection";
    "Page Fault";
    " ";
    "Math Fault";
    "Alignment Check";
    "Machine Check";
    "SIMD Exception";
  ]
  @ List.init 28 (fun _ -> " ")
  @ [ "MP Crash"; "Wake"; "Dbg" ]

let symbol_text symbols address =
  match
    List.find_opt (fun (symbol : symbol) -> symbol.address = address) symbols
  with
  | Some symbol -> symbol.name
  | None -> Printf.sprintf "%LX" address

let decode ?(max_bytes = max_input_bytes) ?(max_instructions = 1_000_000)
    ?(symbols = []) ~mode ~position bytes =
  try
    if max_bytes <= 0 || max_bytes > max_input_bytes then
      invalid 0 "max_bytes must be positive and at most 16 MiB";
    if max_instructions <= 0 || max_instructions > max_input_bytes then
      invalid 0 "max_instructions must be positive and at most 16777216";
    let length = String.length bytes in
    if length > max_bytes then invalid 0 "input exceeds max_bytes";
    ignore (add 0 position (Int64.of_int length));
    let names = Hashtbl.create 16 in
    List.iter
      (fun (symbol : symbol) ->
        if
          symbol.name = ""
          || String.exists
               (fun c -> Char.code c < 33 || Char.code c = 127)
               symbol.name
        then
          invalid 0
            "symbol names must be nonempty and contain no whitespace or \
             control bytes";
        match Hashtbl.find_opt names symbol.name with
        | Some previous when previous <> symbol.address ->
            invalid 0 "one symbol name has conflicting addresses"
        | _ -> Hashtbl.replace names symbol.name symbol.address)
      symbols;
    let get index =
      if index >= length then None else Some (Char.code bytes.[index])
    in
    let one start =
      let cursor = ref start in
      let ensure count =
        if !cursor - start + count > 15 then
          invalid start "instruction exceeds the 15-byte boundary";
        if count > length - !cursor then invalid start "truncated instruction"
      in
      let read count =
        ensure count;
        let result = ref 0L in
        for i = 0 to count - 1 do
          result :=
            Int64.logor !result
              (Int64.shift_left
                 (Int64.of_int (Char.code bytes.[!cursor + i]))
                 (i * 8))
        done;
        cursor := !cursor + count;
        !result
      in
      let byte () = Int64.to_int (read 1) in
      let extend count value =
        if count = 8 then value
        else
          Int64.shift_right
            (Int64.shift_left value (64 - (count * 8)))
            (64 - (count * 8))
      in
      let opsize = ref (if mode = Operand.Use16 then 16 else 32) in
      let opadd = ref (Operand.mode_bits mode) in
      let rex = ref None and prefixes = ref [] and segments = ref [] in
      let rec prefix () =
        ensure 1;
        let value = Char.code bytes.[!cursor] in
        let segment =
          List.assoc_opt value
            [
              (0x2e, "CS:");
              (0x36, "SS:");
              (0x3e, "DS:");
              (0x26, "ES:");
              (0x64, "FS:");
              (0x65, "GS:");
            ]
        in
        let consumed =
          match segment with
          | Some name ->
              if List.length !segments < 8 then segments := name :: !segments;
              true
          | None when value = 0x66 ->
              opsize := if !opsize = 16 then 32 else 16;
              true
          | None when value = 0x67 ->
              opadd := if !opadd = 16 then 32 else 16;
              true
          | None when mode = Operand.Use64 && between 0x40 0x4f value ->
              rex := Some value;
              if value >= 0x48 then opsize := 64;
              true
          | None -> false
        in
        if consumed then (
          prefixes := byte () :: !prefixes;
          prefix ())
      in
      prefix ();
      let opcode_start = !cursor in
      let table = if mode = Operand.Use64 then snd tables else fst tables in
      let lookup index = Option.value ~default:0 (get (opcode_start + index)) in
      let selected = find_entry table lookup !opsize in
      let selected =
        if mode = Operand.Use64 && !opsize = 32 then
          let ((_, wide) as candidate) = find_entry table lookup 64 in
          let _, original = selected in
          if
            (wide != original && flag wide 0x080)
            || (flag wide 0x100 && (lookup 1 lsr 3) land 7 = lookup 1 land 7)
          then candidate
          else selected
        else selected
      in
      let opcode, form = selected in
      if form.opcode_bytes = [] then
        invalid start "no matching pinned UAsm instruction form";
      List.iteri
        (fun i expected ->
          match get (opcode_start + i) with
          | None -> invalid start "truncated opcode"
          | Some actual ->
              let last = i = List.length form.opcode_bytes - 1 in
              let matches =
                if last && flag form 0x004 then
                  actual land 0xf8 = expected land 0xf8
                else actual = expected
              in
              if not matches then
                invalid start "no matching pinned UAsm instruction form")
        form.opcode_bytes;
      (if form.uasm_slash_value < 8 then
         match get (opcode_start + List.length form.opcode_bytes) with
         | None -> invalid start "truncated ModR/M"
         | Some byte ->
             if
               (byte lsr 3) land 7 <> form.slash_value
               || (memory form.argument1 || memory form.argument2)
                  && byte land 0xc0 = 0xc0
             then invalid start "no matching pinned UAsm instruction form");
      ignore (read (List.length form.opcode_bytes));
      let arguments = [| form.argument1; form.argument2 |] in
      let sizes = [| form.size1; form.size2 |] in
      let strings =
        Array.init 2 (fun i ->
            if sizes.(i) = 0 then ""
            else
              Printf.sprintf "%s%d "
                (if signed arguments.(i) then "I" else "U")
                sizes.(i))
      in
      let values = Array.make 2 None and targets = Array.make 2 None in
      let append i text = strings.(i) <- strings.(i) ^ text in
      let rex_value = Option.value ~default:0 !rex in
      let rex_b = (rex_value land 1) lsl 3
      and rex_x = (rex_value land 2) lsl 2
      and rex_r = (rex_value land 4) lsl 1 in
      (if flag form 0x004 && form.slash_value = 8 then
         let index = if between 12 15 form.argument1 then 0 else 1 in
         let last = List.hd (List.rev form.opcode_bytes) in
         let number = Char.code bytes.[!cursor - 1] - last + rex_b in
         append index (register sizes.(index) (Option.is_some !rex) number));
      if rm form.argument1 || rm form.argument2 then (
        let mem = if rm form.argument2 then 1 else 0 in
        let other = 1 - mem in
        append mem (String.concat "" (List.rev !segments));
        let modrm = byte () in
        let mod_ = modrm lsr 6
        and r1 = (modrm land 7) + rex_b
        and r2 = ((modrm lsr 3) land 7) + rex_r in
        let sib = if mod_ < 3 && r1 land 7 = 4 then Some (byte ()) else None in
        if mod_ = 1 then (
          let value = read 1 in
          values.(mem) <- Some (extend 1 value);
          append mem (Printf.sprintf "%02LX" value))
        else if mod_ = 2 then (
          let value = read 4 in
          values.(mem) <- Some (extend 4 value);
          append mem (Printf.sprintf "%08LX" value));
        if form.slash_value >= 8 then
          if arguments.(other) = 39 then
            match List.nth_opt [ "ES"; "CS"; "SS"; "DS"; "FS"; "GS" ] r2 with
            | Some name -> append other name
            | None -> invalid start "invalid segment register field"
          else if
            List.mem sizes.(other) [ 8; 16; 32; 64 ]
            && not (immediate arguments.(other))
          then append other (register sizes.(other) (Option.is_some !rex) r2);
        if r1 land 7 = 5 && mod_ = 0 then (
          let displacement = extend 4 (read 4) in
          let immediate_size =
            if immediate arguments.(other) then sizes.(other) / 8 else 0
          in
          let target =
            if mode = Operand.Use64 then
              add start
                (add start position (Int64.of_int (!cursor + immediate_size)))
                displacement
            else displacement
          in
          targets.(mem) <- Some target;
          values.(mem) <- Some displacement;
          append mem (Printf.sprintf "[%s]" (symbol_text symbols target)))
        else if mod_ < 3 then
          match sib with
          | Some sib ->
              let base = (sib land 7) + rex_b
              and index = ((sib lsr 3) land 7) + rex_x
              and scale = 1 lsl sib lsr 6 in
              let width = if mode = Operand.Use64 then 64 else 32 in
              if base = 5 && mod_ = 0 then (
                let displacement = read 4 in
                values.(mem) <- Some displacement;
                append mem
                  (Printf.sprintf "%08LX[%s*%d]" displacement
                     (register width false index)
                     scale))
              else if index = 4 then
                append mem (Printf.sprintf "[%s]" (register width false base))
              else
                append mem
                  (Printf.sprintf "[%s+%s*%d]"
                     (register width false base)
                     (register width false index)
                     scale)
          | None ->
              append mem (Printf.sprintf "[%s]" (register !opadd false r1))
        else append mem (register sizes.(mem) (Option.is_some !rex) r1));
      let jump = ref None in
      for i = 0 to 1 do
        let argument = arguments.(i) in
        if immediate argument then (
          let count = sizes.(i) / 8 in
          let value = read count in
          values.(i) <- Some value;
          append i (Printf.sprintf "%0*LX" (count * 2) value);
          if i = 0 && count = 1 && List.hd form.opcode_bytes = 0xcd then
            Option.iter
              (fun name -> append i (" " ^ name))
              (List.nth_opt interrupt_names (Int64.to_int value)))
        else if between 1 3 argument then (
          let count = 1 lsl (argument - 1) in
          let displacement = extend count (read count) in
          let target =
            add start (add start position (Int64.of_int !cursor)) displacement
          in
          values.(i) <- Some displacement;
          targets.(i) <- Some target;
          jump := Some target;
          append i (symbol_text symbols target))
        else if between 28 31 argument then (
          append i (String.concat "" (List.rev !segments));
          let count =
            if sizes.(i) = 8 then 1 else if !opadd = 16 then 2 else 4
          in
          let value = read count in
          values.(i) <- Some value;
          targets.(i) <- Some value;
          append i (Printf.sprintf "[%s]" (symbol_text symbols value)))
        else if argument = 47 then (
          let number =
            Char.code bytes.[!cursor - 1] - List.hd (List.rev form.opcode_bytes)
          in
          if not (between 0 7 number) then
            invalid start "invalid x87 stack register";
          append i (Printf.sprintf "ST%d" number))
        else append i (fixed_register argument)
      done;
      if flag form 0x400 && byte () <> 0 then
        invalid start "pinned ENTER form requires a zero nesting byte";
      let operands =
        Array.to_list
          (Array.mapi
             (fun i argument_type ->
               if argument_type = 0 then None
               else
                 Some
                   {
                     argument_type;
                     size = sizes.(i);
                     text = strings.(i);
                     value = values.(i);
                     target = targets.(i);
                   })
             arguments)
        |> List.filter_map Fun.id
      in
      ( {
          offset = start;
          address = add start position (Int64.of_int start);
          bytes = String.sub bytes start (!cursor - start);
          opcode;
          form;
          prefixes = List.rev !prefixes;
          operands;
          jump_target = !jump;
        },
        !cursor )
    in
    let rec stream offset count collected =
      if offset = length then List.rev collected
      else if count = max_instructions then
        invalid offset "input exceeds max_instructions"
      else
        let instruction, next = one offset in
        stream next (count + 1) (instruction :: collected)
    in
    let instructions_ = stream 0 0 [] in
    Ok
      {
        mode_ = mode;
        position_ = position;
        byte_count_ = length;
        symbols;
        instructions_;
      }
  with Invalid error -> Error error

let text instruction =
  instruction.opcode
  ^
  match instruction.operands with
  | [] -> ""
  | operands ->
      "\t" ^ String.concat "," (List.map (fun operand -> operand.text) operands)

let hex bytes =
  String.to_seq bytes
  |> Seq.map (fun c -> Printf.sprintf "%02X" (Char.code c))
  |> List.of_seq |> String.concat ""

let listing t =
  let buffer = Buffer.create 256 in
  List.iter
    (fun instruction ->
      List.iter
        (fun (symbol : symbol) ->
          if symbol.address = instruction.address then
            Buffer.add_string buffer (symbol.name ^ ":\n"))
        t.symbols;
      Buffer.add_string buffer
        (Printf.sprintf "%016LX  %-30s  %s\n" instruction.address
           (hex instruction.bytes) (text instruction)))
    t.instructions_;
  Buffer.contents buffer

let map t =
  let buffer = Buffer.create 128 in
  Buffer.add_string buffer "holyc-disassembly-map-v1\n";
  List.iter
    (fun (symbol : symbol) ->
      Buffer.add_string buffer
        (Printf.sprintf "symbol %016LX %s\n" symbol.address symbol.name))
    t.symbols;
  List.iter
    (fun instruction ->
      Option.iter
        (fun target ->
          Buffer.add_string buffer
            (Printf.sprintf "branch %016LX %016LX %s\n" instruction.address
               target
               (symbol_text t.symbols target)))
        instruction.jump_target)
    t.instructions_;
  Buffer.contents buffer

let to_yojson t =
  let integer n = `String (Int64.to_string n)
  and optional f = function
    | None -> `Null
    | Some x -> f x
  in
  `Assoc
    [
      ("schema", `String "holyc-disassembly-v1");
      ("reference_commit", `String Facts.reference_commit);
      ("bits", `Int (Operand.mode_bits t.mode_));
      ("position", integer t.position_);
      ("byte_count", `Int t.byte_count_);
      ( "symbols",
        `List
          (List.map
             (fun (symbol : symbol) ->
               `Assoc
                 [
                   ("name", `String symbol.name);
                   ("address", integer symbol.address);
                 ])
             t.symbols) );
      ( "instructions",
        `List
          (List.map
             (fun (instruction : instruction) ->
               `Assoc
                 [
                   ("offset", `Int instruction.offset);
                   ("address", integer instruction.address);
                   ("bytes_hex", `String (hex instruction.bytes));
                   ("size", `Int (String.length instruction.bytes));
                   ("opcode", `String instruction.opcode);
                   ("text", `String (text instruction));
                   ("entry_index", `Int instruction.form.entry_index);
                   ("source_line", `Int instruction.form.source_line);
                   ( "prefixes",
                     `List (List.map (fun n -> `Int n) instruction.prefixes) );
                   ("jump_target", optional integer instruction.jump_target);
                   ( "operands",
                     `List
                       (List.map
                          (fun operand ->
                            `Assoc
                              [
                                ("argument_type", `Int operand.argument_type);
                                ("size", `Int operand.size);
                                ("text", `String operand.text);
                                ("value", optional integer operand.value);
                                ("target", optional integer operand.target);
                              ])
                          instruction.operands) );
                 ])
             t.instructions_) );
    ]
