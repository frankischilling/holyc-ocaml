module Ids = Map.Make (Int)
module Names = Map.Make (String)

type mode = Aot | Jit | Hosted
type section_kind = Code | Data | Private_data

type section = {
  id : int;
  name : string;
  kind : section_kind;
  alignment_bits : int;
  bytes : string;
}

type binding = Local | Import | Export
type address = { section : int; offset : int }
type definition = Defined of address | Absolute of int64 | Undefined
type source = { item : int option; span : Common.Span.t option }

type symbol = {
  id : int;
  name : string;
  scope : string option;
  source_identity : int option;
  binding : binding;
  definition : definition;
  source : source;
}

type range = Signed | Unsigned | Bit_pattern | Modulo
type target = Symbol of int | Value of int64

type intent =
  | Set of {
      target : target;
      addend : int64;
      relative_to : address option;
      range : range;
    }
  | Add_bias
  | Subtract_bias

type phase = Compile | Load

type relocation = {
  field : address;
  width : int;
  intent : intent;
  phase : phase;
  source : source;
}

type mapping = { address : address; size : int; source : source }

type error = {
  relocation_index : int option;
  source : source option;
  message : string;
}

type t = {
  mode_ : mode;
  position_ : int64;
  image_alignment_bits_ : int;
  sections_ : section list;
  symbols_ : symbol list;
  relocations_ : relocation list;
  mappings_ : mapping list;
  section_ids : (section * int) Ids.t;
  symbol_ids : symbol Ids.t;
  size_ : int;
  work_ : int;
}

type linked = {
  base_ : int64;
  object_ : t;
  bytes_ : string;
  pending_ : relocation list;
  symbols_ : (symbol * int64 option) list;
}

let no_source = { item = None; span = None }

exception Rejected of error

let reject ?relocation_index ?source message =
  raise (Rejected { relocation_index; source; message })

let copy s = Bytes.to_string (Bytes.of_string s)

let copy_section (s : section) =
  { s with name = copy s.name; bytes = copy s.bytes }

let copy_symbol (s : symbol) =
  { s with name = copy s.name; scope = Option.map copy s.scope }

let sum ?relocation_index ?source message left right =
  if
    (right > 0L && left > Int64.sub Int64.max_int right)
    || (right < 0L && left < Int64.sub Int64.min_int right)
  then reject ?relocation_index ?source message;
  Int64.add left right

let difference ?relocation_index ?source left right =
  if
    (right > 0L && left < Int64.add Int64.min_int right)
    || (right < 0L && left > Int64.add Int64.max_int right)
  then
    reject ?relocation_index ?source
      "relative relocation arithmetic exceeds signed 64-bit range";
  Int64.sub left right

let valid_work n =
  if n < 1 || n > 1_010_000_000 then
    reject "object work limit must be between 1 and 1010000000"

let source_check source =
  match source.item with
  | Some n when n < 0 -> reject ~source "object source item must be nonnegative"
  | _ -> ()

let create ?(max_bytes = 16 * 1024 * 1024) ?(max_work = 100_000_000)
    ?(position = 0L) ?image_alignment_bits ~mode ~sections ~symbols ~relocations
    ?(mappings = []) () =
  try
    valid_work max_work;
    if max_bytes < 1 || max_bytes > 100_000_000 then
      reject "object byte limit must be between 1 and 100000000";
    if position < 0L then reject "object raw position must be nonnegative";
    let work = ref 0 in
    let charge n =
      if n < 0 || n > max_work - !work then
        reject "object work exceeds its configured limit";
      work := !work + n
    in
    let section_ids = ref Ids.empty and size = ref 0 in
    List.iter
      (fun (s : section) ->
        charge 1;
        charge (String.length s.name);
        charge (String.length s.bytes);
        if s.id < 0 || Ids.mem s.id !section_ids then
          reject "object section IDs must be nonnegative and unique";
        if s.name = "" then reject "object section name must be nonempty";
        if s.alignment_bits < 0 || s.alignment_bits > 62 then
          reject "object section alignment bits must be between 0 and 62";
        let alignment = Int64.shift_left 1L s.alignment_bits in
        let mask = Int64.sub alignment 1L in
        let aligned =
          Int64.logand
            (sum "section alignment exceeds signed 64-bit range"
               (Int64.of_int !size) mask)
            (Int64.lognot mask)
        in
        if aligned > Int64.of_int max_bytes then
          reject "object section alignment exceeds its byte limit";
        let offset = Int64.to_int aligned in
        if String.length s.bytes > max_bytes - offset then
          reject "object sections exceed their byte limit";
        charge (offset - !size);
        section_ids := Ids.add s.id (s, offset) !section_ids;
        size := offset + String.length s.bytes)
      sections;
    ignore
      (sum "object raw image end exceeds signed 64-bit range" position
         (Int64.of_int !size));
    let address ?source address count =
      if address.offset < 0 || count < 0 then
        reject ?source "object range must be nonnegative";
      match Ids.find_opt address.section !section_ids with
      | None -> reject ?source "object address names an unknown section"
      | Some (s, _) ->
          let n = String.length s.bytes in
          if count > n || address.offset > n - count then
            reject ?source "object range extends beyond its section"
    in
    let section_alignment =
      List.fold_left (fun n (s : section) -> max n s.alignment_bits) 0 sections
    in
    let image_alignment_bits =
      Option.value image_alignment_bits ~default:section_alignment
    in
    if image_alignment_bits < section_alignment || image_alignment_bits > 63
    then
      reject
        "object image alignment metadata must cover its sections and fit bits \
         0 through 63";
    let symbol_ids = ref Ids.empty and names = Hashtbl.create 16 in
    List.iter
      (fun (s : symbol) ->
        charge 1;
        charge (String.length s.name);
        Option.iter (fun n -> charge (String.length n)) s.scope;
        source_check s.source;
        Option.iter
          (fun id ->
            if id < 0 then
              reject ~source:s.source
                "object source symbol identity must be nonnegative")
          s.source_identity;
        if s.id < 0 || Ids.mem s.id !symbol_ids then
          reject ~source:s.source
            "object symbol IDs must be nonnegative and unique";
        if s.name = "" then
          reject ~source:s.source "object symbol name must be nonempty";
        if Hashtbl.mem names (s.scope, s.name) then
          reject ~source:s.source
            "object symbol names must be unique within their scope";
        Hashtbl.add names (s.scope, s.name) ();
        (match (s.binding, s.definition) with
        | Import, Undefined when s.scope = None -> ()
        | Import, _ ->
            reject ~source:s.source
              "object imports must be undefined root symbols"
        | (Local | Export), Defined a -> address ~source:s.source a 0
        | (Local | Export), Absolute _ -> ()
        | (Local | Export), Undefined ->
            reject ~source:s.source
              "object definitions must have an address or an absolute value");
        if s.binding = Export && s.scope <> None then
          reject ~source:s.source "object exports must be root symbols";
        symbol_ids := Ids.add s.id s !symbol_ids)
      symbols;
    List.iteri
      (fun relocation_index (r : relocation) ->
        charge 1;
        source_check r.source;
        let fail m = reject ~relocation_index ~source:r.source m in
        if not (List.mem r.width [ 1; 2; 4; 8 ]) then
          fail "object relocation width must be one, two, four or eight bytes";
        (try address ~source:r.source r.field r.width
         with Rejected e ->
           raise (Rejected { e with relocation_index = Some relocation_index }));
        match r.intent with
        | Add_bias | Subtract_bias ->
            if r.phase = Compile then
              fail "object bias relocations belong to the load phase"
        | Set { target; relative_to; _ } ->
            (match target with
            | Symbol id when not (Ids.mem id !symbol_ids) ->
                fail "object relocation names an unknown symbol ID"
            | _ -> ());
            Option.iter (fun a -> address ~source:r.source a 0) relative_to)
      relocations;
    List.iter
      (fun (m : mapping) ->
        charge 1;
        source_check m.source;
        address ~source:m.source m.address m.size)
      mappings;
    (* Copy only after the complete structural and resource preflight. *)
    let sections = List.map copy_section sections
    and symbols = List.map copy_symbol symbols in
    let section_ids =
      List.fold_left
        (fun ids (s : section) ->
          let _, offset = Ids.find s.id !section_ids in
          Ids.add s.id (s, offset) ids)
        Ids.empty sections
    in
    let symbol_ids =
      List.fold_left
        (fun ids (s : symbol) -> Ids.add s.id s ids)
        Ids.empty symbols
    in
    Ok
      {
        mode_ = mode;
        position_ = position;
        image_alignment_bits_ = image_alignment_bits;
        sections_ = sections;
        symbols_ = symbols;
        relocations_ = relocations;
        mappings_ = mappings;
        section_ids;
        symbol_ids;
        size_ = !size;
        work_ = !work;
      }
  with Rejected e -> Error e

let mode t = t.mode_
let position t = t.position_
let image_alignment_bits t = t.image_alignment_bits_
let size t = t.size_
let work t = t.work_
let sections t = List.map copy_section t.sections_
let symbols (t : t) = List.map copy_symbol t.symbols_
let relocations t = t.relocations_
let mappings t = t.mappings_
let section_offset t id = Option.map snd (Ids.find_opt id t.section_ids)

let address_offset t a =
  match Ids.find_opt a.section t.section_ids with
  | Some (s, offset) when a.offset >= 0 && a.offset <= String.length s.bytes ->
      Ok (offset + a.offset)
  | _ ->
      Error
        {
          relocation_index = None;
          source = None;
          message = "object address does not belong to a section";
        }

let read_word get offset width =
  let n = ref 0L in
  for i = 0 to width - 1 do
    n :=
      Int64.logor !n
        (Int64.shift_left (Int64.of_int (Char.code (get (offset + i)))) (8 * i))
  done;
  !n

let word_byte value index =
  Char.chr
    (Int64.to_int
       (Int64.logand (Int64.shift_right_logical value (8 * index)) 255L))

let link ?(max_work = 100_000_000) ?(imports = []) ?(defer_imports = false)
    ?(compile_only = false) ~base t =
  try
    valid_work max_work;
    if base < 0L then reject "object load bias must be nonnegative";
    if compile_only && base <> 0L then
      reject "compile-only object resolution requires zero load bias";
    let loaded =
      sum "object loaded position exceeds signed 64-bit range" t.position_ base
    in
    ignore
      (sum "object loaded image end exceeds signed 64-bit range" loaded
         (Int64.of_int t.size_));
    let work = ref 0 in
    let charge ?relocation_index ?source n =
      if n > max_work - !work then
        reject ?relocation_index ?source
          "object link work exceeds its configured limit";
      work := !work + n
    in
    charge t.work_;
    let names = ref Names.empty in
    List.iter
      (fun (name, value) ->
        charge 1;
        charge (String.length name);
        if name = "" || Names.mem name !names then
          reject "object import values must have nonempty unique names";
        names := Names.add name value !names)
      imports;
    let offset a =
      match address_offset t a with
      | Ok n -> n
      | Error e -> raise (Rejected e)
    in
    let address ~loaded_coordinates a =
      sum "object symbol address exceeds signed 64-bit range"
        (if loaded_coordinates then loaded else t.position_)
        (Int64.of_int (offset a))
    in
    let symbol_value ~loaded_coordinates (s : symbol) =
      match s.definition with
      | Defined a -> Some (address ~loaded_coordinates a)
      | Absolute v -> Some v
      | Undefined -> Names.find_opt s.name !names
    in
    let values =
      List.map
        (fun s -> (s, symbol_value ~loaded_coordinates:true s))
        t.symbols_
    in
    let by_offset =
      List.fold_left
        (fun ids (s : section) ->
          if s.bytes = "" then ids
          else Ids.add (snd (Ids.find s.id t.section_ids)) s ids)
        Ids.empty t.sections_
    in
    let original offset =
      match Ids.find_last_opt (fun start -> start <= offset) by_offset with
      | Some (start, s) when offset - start < String.length s.bytes ->
          s.bytes.[offset - start]
      | _ -> '\000'
    in
    (* Sparse private overlay plans overlap in source order without touching the
       input object or allocating the final image before all checks finish. *)
    let overlay = ref Ids.empty and pending = ref [] and writes = ref [] in
    let get i = Option.value (Ids.find_opt i !overlay) ~default:(original i) in
    let write index (r : relocation) value =
      charge ~relocation_index:index ~source:r.source r.width;
      let start = offset r.field in
      for i = 0 to r.width - 1 do
        overlay := Ids.add (start + i) (word_byte value i) !overlay
      done;
      writes := (start, r.width, value) :: !writes
    in
    let records = List.mapi (fun i r -> (i, r)) t.relocations_ in
    let is_bias (r : relocation) =
      match r.intent with
      | Add_bias | Subtract_bias -> true
      | Set _ -> false
    in
    let order =
      List.filter (fun (_, r) -> r.phase = Compile) records
      @ List.filter (fun (_, r) -> r.phase = Load && is_bias r) records
      @ List.filter (fun (_, r) -> r.phase = Load && not (is_bias r)) records
    in
    List.iter
      (fun (index, (r : relocation)) ->
        charge ~relocation_index:index ~source:r.source 1;
        if compile_only && r.phase = Load then pending := (index, r) :: !pending
        else
          match r.intent with
          | Add_bias ->
              write index r
                (Int64.add (read_word get (offset r.field) r.width) base)
          | Subtract_bias ->
              write index r
                (Int64.sub (read_word get (offset r.field) r.width) base)
          | Set { target; addend; relative_to; range } -> (
              let value =
                match target with
                | Value n -> Some n
                | Symbol id ->
                    symbol_value ~loaded_coordinates:(r.phase = Load)
                      (Ids.find id t.symbol_ids)
              in
              match value with
              | None when defer_imports && r.phase = Load ->
                  pending := (index, r) :: !pending
              | None ->
                  reject ~relocation_index:index ~source:r.source
                    "object relocation has an unresolved import"
              | Some value ->
                  let value =
                    if range = Modulo then Int64.add value addend
                    else
                      sum ~relocation_index:index ~source:r.source
                        "relocation addend exceeds signed 64-bit range" value
                        addend
                  in
                  let value =
                    match relative_to with
                    | None -> value
                    | Some a ->
                        let anchor =
                          address ~loaded_coordinates:(r.phase = Load) a
                        in
                        if range = Modulo then Int64.sub value anchor
                        else
                          difference ~relocation_index:index ~source:r.source
                            value anchor
                  in
                  (match range with
                  | Modulo -> ()
                  | _ -> (
                      let range =
                        match range with
                        | Signed -> Encoding.Signed
                        | Unsigned -> Encoding.Unsigned
                        | Bit_pattern -> Encoding.Bit_pattern
                        | Modulo -> assert false
                      in
                      match Encoding.field ~bytes:r.width ~range value with
                      | Ok _ -> ()
                      | Error message ->
                          reject ~relocation_index:index ~source:r.source
                            message));
                  write index r value))
      order;
    let output = Bytes.make t.size_ '\000' in
    List.iter
      (fun (s : section) ->
        Bytes.blit_string s.bytes 0 output
          (snd (Ids.find s.id t.section_ids))
          (String.length s.bytes))
      t.sections_;
    List.iter
      (fun (start, width, value) ->
        for i = 0 to width - 1 do
          Bytes.set output (start + i) (word_byte value i)
        done)
      (List.rev !writes);
    let pending =
      List.sort (fun (a, _) (b, _) -> Int.compare a b) !pending |> List.map snd
    in
    Ok
      {
        base_ = base;
        object_ = t;
        bytes_ = Bytes.unsafe_to_string output;
        pending_ = pending;
        symbols_ = values;
      }
  with Rejected e -> Error e

let base t = t.base_
let bytes t = copy t.bytes_
let pending t = t.pending_
let resolved_symbols t = List.map (fun (s, v) -> (copy_symbol s, v)) t.symbols_
let object_ t = t.object_

let hex bytes =
  let alphabet = "0123456789abcdef" in
  String.init
    (2 * String.length bytes)
    (fun i ->
      let b = Char.code bytes.[i / 2] in
      alphabet.[if i mod 2 = 0 then b lsr 4 else b land 15])

let report_name name =
  let out = Buffer.create (String.length name) in
  String.iter
    (fun byte ->
      if Char.code byte < 127 then Buffer.add_char out byte
      else Buffer.add_string out (Printf.sprintf "\\x%02x" (Char.code byte)))
    name;
  Buffer.contents out

let integer v = `String (Printf.sprintf "0x%016Lx" v)

let json_source s =
  `Assoc
    [
      ("item", Option.fold ~none:`Null ~some:(fun n -> `Int n) s.item);
      ( "span",
        Option.fold ~none:`Null
          ~some:(fun (s : Common.Span.t) ->
            `Assoc
              [
                ("source_id", `Int (Common.Source_id.to_int s.source));
                ("start", `Int s.start);
                ("stop", `Int s.stop);
              ])
          s.span );
    ]

let json_address a =
  `Assoc [ ("section", `Int a.section); ("offset", `Int a.offset) ]

let json_symbol (s : symbol) =
  `Assoc
    [
      ("id", `Int s.id);
      ("name", `String (report_name s.name));
      ("name_hex", `String (hex s.name));
      ( "scope",
        Option.fold ~none:`Null ~some:(fun n -> `String (report_name n)) s.scope
      );
      ( "source_identity",
        Option.fold ~none:`Null ~some:(fun n -> `Int n) s.source_identity );
      ( "binding",
        `String
          (match s.binding with
          | Local -> "local"
          | Import -> "import"
          | Export -> "export") );
      ( "definition",
        match s.definition with
        | Defined a -> json_address a
        | Absolute n -> `Assoc [ ("absolute", integer n) ]
        | Undefined -> `Null );
      ("source", json_source s.source);
    ]

let json_relocation (r : relocation) =
  `Assoc
    [
      ("field", json_address r.field);
      ("width", `Int r.width);
      ( "phase",
        `String
          (match r.phase with
          | Compile -> "compile"
          | Load -> "load") );
      ("source", json_source r.source);
      ( "intent",
        match r.intent with
        | Add_bias -> `String "add_bias"
        | Subtract_bias -> `String "subtract_bias"
        | Set { target; addend; relative_to; range } ->
            `Assoc
              [
                ( "target",
                  match target with
                  | Symbol id -> `Assoc [ ("symbol", `Int id) ]
                  | Value n -> `Assoc [ ("value", integer n) ] );
                ("addend", integer addend);
                ( "relative_to",
                  Option.fold ~none:`Null ~some:json_address relative_to );
                ( "range",
                  `String
                    (match range with
                    | Signed -> "signed"
                    | Unsigned -> "unsigned"
                    | Bit_pattern -> "bit_pattern"
                    | Modulo -> "modulo") );
              ] );
    ]

let to_yojson (t : t) =
  `Assoc
    [
      ("schema", `String "holyc-native-sections-v1");
      ( "mode",
        `String
          (match t.mode_ with
          | Aot -> "aot"
          | Jit -> "jit"
          | Hosted -> "hosted") );
      ("position", integer t.position_);
      ("image_alignment_bits", `Int t.image_alignment_bits_);
      ("size", `Int t.size_);
      ("work", `Int t.work_);
      ( "sections",
        `List
          (List.map
             (fun (s : section) ->
               `Assoc
                 [
                   ("id", `Int s.id);
                   ("name", `String (report_name s.name));
                   ("name_hex", `String (hex s.name));
                   ( "kind",
                     `String
                       (match s.kind with
                       | Code -> "code"
                       | Data -> "data"
                       | Private_data -> "private_data") );
                   ("alignment_bits", `Int s.alignment_bits);
                   ("offset", `Int (snd (Ids.find s.id t.section_ids)));
                   ("size", `Int (String.length s.bytes));
                   ("bytes", `String (hex s.bytes));
                 ])
             t.sections_) );
      ("symbols", `List (List.map json_symbol t.symbols_));
      ("relocations", `List (List.map json_relocation t.relocations_));
      ( "mappings",
        `List
          (List.map
             (fun (m : mapping) ->
               `Assoc
                 [
                   ("address", json_address m.address);
                   ("size", `Int m.size);
                   ("source", json_source m.source);
                 ])
             t.mappings_) );
    ]

let linked_to_yojson t =
  `Assoc
    [
      ("schema", `String "holyc-linked-native-sections-v1");
      ("object", to_yojson t.object_);
      ("base", integer t.base_);
      ("position", integer (Int64.add t.object_.position_ t.base_));
      ("bytes", `String (hex t.bytes_));
      ("pending", `List (List.map json_relocation t.pending_));
      ( "symbols",
        `List
          (List.map
             (fun (s, v) ->
               `Assoc
                 [
                   ("symbol", json_symbol s);
                   ("value", Option.fold ~none:`Null ~some:integer v);
                 ])
             t.symbols_) );
    ]
