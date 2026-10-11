type error = { artifact : string; offset : int option; message : string }

type limits = {
  file_bytes : int;
  work : int;
  image_bytes : int;
  heap_bytes : int64;
  modules : int;
}

let default_limits =
  {
    file_bytes = 16 * 1024 * 1024;
    work = 1_000_000;
    image_bytes = 64 * 1024 * 1024;
    heap_bytes = 67_108_864L;
    modules = 4096;
  }

type record_kind =
  | Import of { width : int; relative : bool; target : int }
  | Export of { relative : bool; value : int64 }
  | Absolute of int list
  | Heap of { code : bool; zeroed : bool; size : int64; targets : int list }
  | Main of int

type record = {
  offset : int;
  wire_type : int;
  name : string;
  kind : record_kind;
}

type artifact = {
  name : string;
  raw : string;
  data : string;
  alignment_bits : int;
  origin : int64 option;
  patch_offset : int;
  table_end : int;
  records : record list;
  cost : int;
}

exception Invalid of error

let fail ?offset artifact message =
  raise
    (Invalid
       {
         artifact = Bytes.to_string (Bytes.of_string artifact);
         offset;
         message = Bytes.to_string (Bytes.of_string message);
       })

let checked f = try Ok (f ()) with Invalid e -> Error e
let copy s = Bytes.to_string (Bytes.of_string s)

let check_limits name l =
  if
    l.file_bytes < 1
    || l.file_bytes > default_limits.file_bytes
    || l.work < 1 || l.work > 100_000_000 || l.image_bytes < 1
    || l.image_bytes > default_limits.image_bytes
    || l.heap_bytes < 0L
    || l.heap_bytes > default_limits.heap_bytes
    || l.modules < 1
    || l.modules > default_limits.modules
  then fail name "limits are outside the supported bounds"

let charge name limit counter amount offset =
  if amount < 0 || amount > limit - !counter then
    fail ?offset name "work limit exceeded";
  counter := !counter + amount

let word raw offset width =
  let value = ref 0L in
  for i = 0 to width - 1 do
    value :=
      Int64.logor !value
        (Int64.shift_left (Int64.of_int (Char.code raw.[offset + i])) (8 * i))
  done;
  !value

let bounded_int name offset label value max_value =
  if value < 0L || value > Int64.of_int max_value then
    fail ~offset name (label ^ " is outside the artifact");
  Int64.to_int value

let field name body_size offset width record_offset =
  if offset < 0 || width > body_size || offset > body_size - width then
    fail ~offset:record_offset name "target field is outside the module body"

let read ?(limits = default_limits) ?(name = "<bin>") input =
  checked (fun () ->
      if String.length name > 65536 then
        fail "<artifact>" "artifact label exceeds 65536 bytes";
      check_limits name limits;
      let length = String.length input in
      if length > limits.file_bytes then
        fail ~offset:0 name "file byte limit exceeded";
      if length < 32 then fail ~offset:length name "truncated CBinFile header";
      let cost = ref 0 in
      charge name limits.work cost length (Some 0);
      charge name limits.work cost (String.length name) None;
      let raw = copy input in
      let require at count =
        if count < 0 || at < 0 || at > length - count then
          fail ~offset:(min at length) name "truncated record payload"
      in
      if String.sub raw 0 2 <> "\xeb\x1e" then
        fail ~offset:0 name "invalid header jump";
      let alignment_bits = Char.code raw.[2] in
      if alignment_bits > 62 then
        fail ~offset:2 name "alignment exponent exceeds 62";
      if raw.[3] <> '\000' then
        fail ~offset:3 name "reserved header byte is nonzero";
      if word raw 4 4 <> 0x42534f54L then
        fail ~offset:4 name "invalid TOSB signature";
      let org = word raw 8 8 in
      if org < 0L then fail ~offset:8 name "negative origin";
      let origin = if org = Int64.max_int then None else Some org in
      let patch_offset =
        bounded_int name 16 "patch table offset" (word raw 16 8) (length - 1)
      in
      if patch_offset < 32 then
        fail ~offset:16 name "patch table overlaps the header";
      if word raw 24 8 <> Int64.of_int length || length land 15 <> 0 then
        fail ~offset:24 name
          "file size must match a complete 16-byte-aligned artifact";
      let body_size = patch_offset - 32 in
      let cursor = ref patch_offset
      and result = ref []
      and import_group = ref false in
      let u32 () =
        require !cursor 4;
        let value = word raw !cursor 4 in
        cursor := !cursor + 4;
        value
      in
      let targets count width at =
        (* Bound multiplication against remaining bytes before allocating a list. *)
        let count =
          bounded_int name (at + 1) "patch count" count ((length - !cursor) / 4)
        in
        charge name limits.work cost count (Some at);
        let offsets = ref [] in
        for _ = 1 to count do
          let payload_offset = !cursor in
          let offset =
            bounded_int name payload_offset "patch target" (u32 ()) body_size
          in
          field name body_size offset width payload_offset;
          offsets := offset :: !offsets
        done;
        List.rev !offsets
      in
      let finished = ref false in
      while not !finished do
        require !cursor 1;
        let at = !cursor in
        let wire_type = Char.code raw.[!cursor] in
        incr cursor;
        if wire_type = 0 then finished := true
        else (
          charge name limits.work cost 1 (Some at);
          if
            not
              ((wire_type >= 2 && wire_type <= 11)
              || (wire_type >= 16 && wire_type <= 25))
          then fail ~offset:at name "unknown or reserved IET type";
          let value = u32 () in
          let name_start = !cursor in
          while !cursor < length && raw.[!cursor] <> '\000' do
            incr cursor
          done;
          if !cursor = length then
            fail ~offset:name_start name "unterminated record name";
          let symbol = String.sub raw name_start (!cursor - name_start) in
          charge name limits.work cost (String.length symbol) (Some name_start);
          incr cursor;
          let is_import = wire_type <= 11 in
          if !import_group && symbol = "" && not is_import then
            fail ~offset:at name
              "empty-name nonimport would be consumed by LoadOneImport";
          if is_import && symbol = "" && not !import_group then
            fail ~offset:at name "import continuation has no named group";
          import_group := is_import;
          let kind =
            if is_import then (
              let width =
                if wire_type < 4 then 0 else 1 lsl ((wire_type - 4) / 2)
              in
              let target =
                bounded_int name (at + 1) "import target" value body_size
              in
              field name body_size target width (at + 1);
              charge name limits.work cost 1 (Some at);
              Import { width; relative = wire_type land 1 = 0; target })
            else
              match wire_type with
              | 16 | 17 | 18 | 19 ->
                  if symbol = "" then
                    fail ~offset:name_start name "export name is empty";
                  let relative = wire_type land 1 = 0 in
                  if relative && value > Int64.of_int body_size then
                    fail ~offset:(at + 1) name
                      "relative export is outside the body";
                  Export { relative; value }
              | 20 ->
                  if symbol <> "" then
                    fail ~offset:name_start name "ABS record name must be empty";
                  Absolute (targets value 4 at)
              | 21 | 22 | 23 | 24 ->
                  let code = wire_type <= 22 in
                  let size_width = if code then 4 else 8 in
                  require !cursor size_width;
                  let size_at = !cursor in
                  let size = word raw size_at size_width in
                  let size =
                    if code then Int64.of_int32 (Int64.to_int32 size) else size
                  in
                  if size < 0L then
                    fail ~offset:size_at name "negative heap size";
                  cursor := !cursor + size_width;
                  Heap
                    {
                      code;
                      zeroed = wire_type = 22 || wire_type = 24;
                      size;
                      targets = targets value (if code then 4 else 8) at;
                    }
              | 25 ->
                  if symbol <> "" then
                    fail ~offset:name_start name
                      "MAIN record name must be empty";
                  let target =
                    bounded_int name (at + 1) "MAIN target" value body_size
                  in
                  field name body_size target 1 (at + 1);
                  Main target
              | _ -> assert false
          in
          result := { offset = at; wire_type; name = symbol; kind } :: !result)
      done;
      let table_end = !cursor in
      if length - table_end > 15 then
        fail ~offset:table_end name "excess bytes after IET_END";
      for i = table_end to length - 1 do
        if raw.[i] <> '\000' then
          fail ~offset:i name "nonzero bytes after IET_END"
      done;
      {
        name = copy name;
        raw;
        data = String.sub raw 32 body_size;
        alignment_bits;
        origin;
        patch_offset;
        table_end;
        records = List.rev !result;
        cost = !cost;
      })

let clone_record (r : record) = { r with name = copy r.name }
let records (a : artifact) = List.map clone_record a.records
let body (a : artifact) = copy a.data
let bytes (a : artifact) = copy a.raw
let work (a : artifact) = a.cost
let integer x = `String (Int64.to_string x)
let hex x = `String (Printf.sprintf "0x%016Lx" x)

let hex_bytes s =
  let b = Buffer.create (2 * String.length s) in
  String.iter
    (fun c -> Buffer.add_string b (Printf.sprintf "%02x" (Char.code c)))
    s;
  Buffer.contents b

let name_json s =
  (* Names on the wire are byte strings, not necessarily UTF-8. *)
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if Char.code c >= 128 then
        Buffer.add_string b (Printf.sprintf "\\x%02x" (Char.code c))
      else Buffer.add_char b c)
    s;
  `String (Buffer.contents b)

let ints xs = `List (List.map (fun x -> `Int x) xs)

let record_json (r : record) =
  let fields =
    match r.kind with
    | Import { width; relative; target } ->
        [
          ("category", `String "import");
          ("width", `Int width);
          ("relative", `Bool relative);
          ("target", `Int target);
        ]
    | Export { relative; value } ->
        [
          ("category", `String "export");
          ("relative", `Bool relative);
          ("value", integer value);
        ]
    | Absolute targets ->
        [ ("category", `String "absolute"); ("targets", ints targets) ]
    | Heap { code; zeroed; size; targets } ->
        [
          ("category", `String "heap");
          ("code", `Bool code);
          ("zeroed", `Bool zeroed);
          ("size", integer size);
          ("targets", ints targets);
        ]
    | Main target -> [ ("category", `String "main"); ("target", `Int target) ]
  in
  `Assoc
    ([
       ("offset", `Int r.offset);
       ("wire_type", `Int r.wire_type);
       ("name", name_json r.name);
       ("name_hex", `String (hex_bytes r.name));
     ]
    @ fields)

let to_yojson (a : artifact) =
  `Assoc
    [
      ("schema", `String "holyc-bin-validation-v1");
      ("artifact", `String (copy a.name));
      ("sha256", `String Digestif.SHA256.(to_hex (digest_string a.raw)));
      ("file_size", `Int (String.length a.raw));
      ("body_size", `Int (String.length a.data));
      ("alignment_bits", `Int a.alignment_bits);
      ( "origin",
        match a.origin with
        | None -> `Null
        | Some x -> integer x );
      ("patch_offset", `Int a.patch_offset);
      ("table_end", `Int a.table_end);
      ("work", `Int a.cost);
      ("records", `List (List.map record_json a.records));
    ]

type flags = { no_abss : bool; just_load : bool; silent : bool }

let default_flags = { no_abss = false; just_load = false; silent = false }

type image = { id : int; name : string; base : int64; body : string }

type relocation = {
  module_id : int;
  artifact : string;
  record_offset : int;
  field_offset : int;
  width : int;
  kind : string;
  symbol : string;
  operand : int64;
  before : int64;
  after : int64;
}

type pending = {
  module_id : int;
  artifact : string;
  record_offset : int;
  symbol : string;
  fields : (int * int * int) list;
}

type heap = {
  module_id : int;
  record_offset : int;
  name : string;
  code : bool;
  zeroed : bool;
  base : int64;
  size : int64;
}

type entry = { module_id : int; record_offset : int; address : int64 }

module Names = Map.Make (String)
module Ids = Map.Make (Int)

module Cells = Map.Make (struct
  type t = int * int

  let compare = compare
end)

type world = {
  image_table : image Ids.t;
  module_ranges : (int64 * int64) list;
  symbol_table : int64 Names.t;
  pending : pending list;
  patches : relocation list;
  allocations : heap list;
  mains : entry list;
  messages : string list;
  next_module : int64;
  next_code : int64;
  next_data : int64;
  heap_used : int64;
  image_used : int;
  last_work : int;
  work_used : int;
}

let empty ?(module_base = 0x10000000L) ?(code_base = 0x20000000L)
    ?(data_base = 0x30000000L) () =
  {
    image_table = Ids.empty;
    module_ranges = [];
    symbol_table = Names.empty;
    pending = [];
    patches = [];
    allocations = [];
    mains = [];
    messages = [];
    next_module = module_base;
    next_code = code_base;
    next_data = data_base;
    heap_used = 0L;
    image_used = 0;
    last_work = 0;
    work_used = 0;
  }

let add_address name offset value delta =
  if value < 0L || delta < 0L || value > Int64.sub Int64.max_int delta then
    fail ?offset name "virtual address arithmetic exceeds I64 range";
  Int64.add value delta

let aligned name offset value bits =
  let alignment = Int64.shift_left 1L bits in
  let mask = Int64.pred alignment in
  Int64.logand (add_address name offset value mask) (Int64.lognot mask)

let overlaps low high other_low other_high =
  low < other_high && other_low < high

let reserve (w : world) name offset low high =
  if low < 0L || high < low then
    fail ?offset name "invalid virtual allocation range";
  List.iter
    (fun (start, finish) ->
      if overlaps low high start finish then
        fail ?offset name "virtual allocation overlaps a module")
    w.module_ranges;
  List.iter
    (fun (h : heap) ->
      let finish = Int64.add h.base (max 16L h.size) in
      if overlaps low high h.base finish then
        fail ?offset name "virtual allocation overlaps a heap")
    w.allocations

let images w =
  Ids.bindings w.image_table
  |> List.map (fun (_, (i : image)) ->
      { i with name = copy i.name; body = copy i.body })

let relocations w =
  List.rev_map
    (fun (r : relocation) ->
      {
        r with
        artifact = copy r.artifact;
        symbol = copy r.symbol;
        kind = copy r.kind;
      })
    w.patches

let unresolved w =
  List.map
    (fun (p : pending) ->
      { p with artifact = copy p.artifact; symbol = copy p.symbol })
    w.pending

let heaps w =
  List.rev_map (fun (h : heap) -> { h with name = copy h.name }) w.allocations

let entries w = List.rev w.mains

let symbols w =
  Names.bindings w.symbol_table |> List.map (fun (n, v) -> (copy n, v))

let transaction_work w = w.last_work

type transaction = {
  mutable state : world;
  mutable cells : int Cells.t;
  cost : int ref;
  initial_work : int;
  limits : limits;
  name : string;
}

let start limits name state =
  check_limits name limits;
  if
    state.image_used > limits.image_bytes
    || state.heap_used > limits.heap_bytes
    || Ids.cardinal state.image_table > limits.modules
  then fail name "existing world exceeds limits";
  if state.work_used > limits.work then
    fail name "existing world exceeds work limit";
  let cost = ref state.work_used in
  charge name limits.work cost state.image_used None;
  {
    state;
    cells = Cells.empty;
    cost;
    initial_work = state.work_used;
    limits;
    name;
  }

let spend t n at = charge t.name t.limits.work t.cost n at

let patch t id (r : record) target width kind symbol operand relative additive =
  let image = Ids.find id t.state.image_table in
  charge image.name t.limits.work t.cost (1 + width) (Some r.offset);
  let before = ref 0L in
  for i = 0 to width - 1 do
    let byte =
      match Cells.find_opt (id, target + i) t.cells with
      | Some b -> b
      | None -> Char.code image.body.[target + i]
    in
    before := Int64.logor !before (Int64.shift_left (Int64.of_int byte) (8 * i))
  done;
  let value =
    if relative then
      Int64.sub operand (Int64.add image.base (Int64.of_int (target + width)))
    else if additive then Int64.add !before operand
    else operand
  in
  let after =
    if width = 8 then value
    else if width = 0 then 0L
    else Int64.logand value (Int64.pred (Int64.shift_left 1L (8 * width)))
  in
  for i = 0 to width - 1 do
    t.cells <-
      Cells.add
        (id, target + i)
        (Int64.to_int
           (Int64.logand (Int64.shift_right_logical after (8 * i)) 255L))
        t.cells
  done;
  t.state <-
    {
      t.state with
      patches =
        {
          module_id = id;
          artifact = image.name;
          record_offset = r.offset;
          field_offset = 32 + target;
          width;
          kind;
          symbol;
          operand;
          before = !before;
          after;
        }
        :: t.state.patches;
    }

let replay t (p : pending) value =
  List.iter
    (fun (at, target, wire_type) ->
      let width = if wire_type < 4 then 0 else 1 lsl ((wire_type - 4) / 2) in
      let r = { offset = at; wire_type; name = p.symbol; kind = Main 0 } in
      patch t p.module_id r target width "import" p.symbol value
        (wire_type land 1 = 0)
        false)
    p.fields

let resolve t name value =
  let remaining = ref [] in
  List.iter
    (fun (p : pending) ->
      charge p.artifact t.limits.work t.cost 1 (Some p.record_offset);
      if p.symbol = name then replay t p value else remaining := p :: !remaining)
    t.state.pending;
  t.state <- { t.state with pending = List.rev !remaining }

let commit t =
  (* Planning uses an immutable byte overlay. Only this final phase creates and
     mutates private copies, after every possible validation failure. *)
  let changed = Hashtbl.create 16 in
  Cells.iter
    (fun (id, offset) byte ->
      let buffer =
        match Hashtbl.find_opt changed id with
        | Some b -> b
        | None ->
            let b = Bytes.of_string (Ids.find id t.state.image_table).body in
            Hashtbl.add changed id b;
            b
      in
      Bytes.set buffer offset (Char.chr byte))
    t.cells;
  let image_table =
    Hashtbl.fold
      (fun id buffer table ->
        let image = Ids.find id table in
        Ids.add id { image with body = Bytes.to_string buffer } table)
      changed t.state.image_table
  in
  {
    t.state with
    image_table;
    last_work = !(t.cost) - t.initial_work;
    work_used = !(t.cost);
  }

let bind ?(limits = default_limits) state ~name ~value =
  checked (fun () ->
      let t = start limits "<binding>" state in
      spend t (String.length name + 1) None;
      if name = "" || String.contains name '\000' then
        fail "<binding>" "symbol must have a nonempty NUL-free name";
      let name = copy name in
      t.state <-
        {
          t.state with
          symbol_table = Names.add name value t.state.symbol_table;
        };
      resolve t name value;
      commit t)

let load ?(limits = default_limits) ?(flags = default_flags) ?base state
    (a : artifact) =
  checked (fun () ->
      let t = start limits a.name state in
      if String.length a.raw > limits.file_bytes then
        fail ~offset:0 a.name "file byte limit exceeded";
      spend t a.cost (Some 0);
      if Ids.cardinal state.image_table >= limits.modules then
        fail ~offset:0 a.name "module limit exceeded";
      if String.length a.data > limits.image_bytes - state.image_used then
        fail ~offset:16 a.name "cumulative image byte limit exceeded";
      let header =
        match base with
        | Some body ->
            if body < 32L then
              fail ~offset:8 a.name "body base must allow a 32-byte header";
            Int64.sub body 32L
        | None -> (
            match a.origin with
            | Some org -> org
            | None ->
                Int64.sub
                  (aligned a.name (Some 2)
                     (add_address a.name (Some 2) state.next_module 32L)
                     (max 4 a.alignment_bits))
                  32L)
      in
      let base = add_address a.name (Some 8) header 32L in
      let body_end =
        add_address a.name (Some 16) base (Int64.of_int (String.length a.data))
      in
      let file_end =
        add_address a.name (Some 24) header (Int64.of_int (String.length a.raw))
      in
      reserve state a.name (Some 8) header file_end;
      let id = Ids.cardinal state.image_table in
      let image = { id; name = a.name; base; body = a.data } in
      t.state <-
        {
          state with
          image_table = Ids.add id image state.image_table;
          module_ranges = (header, file_end) :: state.module_ranges;
          image_used = state.image_used + String.length a.data;
          next_module = max state.next_module file_end;
        };
      (* Check all allocations and address expressions before planning patches. *)
      let planned_heaps = ref [] in
      let code_cursor = ref state.next_code
      and data_cursor = ref state.next_data
      and used = ref state.heap_used in
      List.iter
        (fun (r : record) ->
          spend t 1 (Some r.offset);
          match r.kind with
          | Heap { code; zeroed; size; _ } ->
              if size > Int64.sub limits.heap_bytes !used then
                fail ~offset:r.offset a.name
                  "cumulative heap byte limit exceeded";
              used := Int64.add !used size;
              let cursor = if code then code_cursor else data_cursor in
              let address = aligned a.name (Some r.offset) !cursor 4 in
              let finish =
                add_address a.name (Some r.offset) address (max 16L size)
              in
              reserve t.state a.name (Some r.offset) address finish;
              if overlaps address finish header file_end then
                fail ~offset:r.offset a.name
                  "heap overlaps the complete module artifact";
              List.iter
                (fun (h : heap) ->
                  if
                    overlaps address finish h.base
                      (Int64.add h.base (max 16L h.size))
                  then
                    fail ~offset:r.offset a.name
                      "virtual heap allocations overlap")
                !planned_heaps;
              cursor := finish;
              planned_heaps :=
                {
                  module_id = id;
                  record_offset = r.offset;
                  name = r.name;
                  code;
                  zeroed;
                  base = address;
                  size;
                }
                :: !planned_heaps
          | Export { relative = true; value } ->
              ignore (add_address a.name (Some r.offset) base value)
          | Main target ->
              ignore
                (add_address a.name (Some r.offset) base (Int64.of_int target))
          | _ -> ())
        a.records;
      ignore body_end;
      let heap_table =
        List.fold_left
          (fun table (h : heap) -> Ids.add h.record_offset h table)
          Ids.empty !planned_heaps
      in
      let rec pass1 = function
        | [] -> ()
        | (r : record) :: rest -> (
            match r.kind with
            | Import _ ->
                let rec group acc = function
                  | (next : record) :: tail when next.name = "" ->
                      group (next :: acc) tail
                  | tail -> (List.rev acc, tail)
                in
                let group, tail = group [ r ] rest in
                let fields =
                  List.map
                    (fun (g : record) ->
                      match g.kind with
                      | Import { target; _ } -> (g.offset, target, g.wire_type)
                      | _ -> assert false)
                    group
                in
                let p =
                  {
                    module_id = id;
                    artifact = a.name;
                    record_offset = r.offset;
                    symbol = r.name;
                    fields;
                  }
                in
                (match Names.find_opt r.name t.state.symbol_table with
                | Some value -> replay t p value
                | None ->
                    t.state <-
                      {
                        t.state with
                        pending = p :: t.state.pending;
                        messages =
                          (if flags.silent then t.state.messages
                           else
                             Printf.sprintf "%s:%d: Unresolved Reference:%s"
                               a.name r.offset r.name
                             :: t.state.messages);
                      });
                pass1 tail
            | Export { relative; value } ->
                let value = if relative then Int64.add base value else value in
                t.state <-
                  {
                    t.state with
                    symbol_table = Names.add r.name value t.state.symbol_table;
                  };
                resolve t r.name value;
                pass1 rest
            | Absolute targets ->
                if not flags.no_abss then
                  List.iter
                    (fun target ->
                      patch t id r target 4 "absolute" "" base false true)
                    targets;
                pass1 rest
            | Heap { code; targets; _ } ->
                let heap = Ids.find r.offset heap_table in
                if r.name <> "" then
                  t.state <-
                    {
                      t.state with
                      symbol_table =
                        Names.add r.name heap.base t.state.symbol_table;
                    };
                List.iter
                  (fun target ->
                    patch t id r target
                      (if code then 4 else 8)
                      "heap" r.name heap.base false true)
                  targets;
                pass1 rest
            | Main _ -> pass1 rest)
      in
      pass1 a.records;
      if not flags.just_load then
        List.iter
          (fun (r : record) ->
            match r.kind with
            | Main target ->
                t.state <-
                  {
                    t.state with
                    mains =
                      {
                        module_id = id;
                        record_offset = r.offset;
                        address = Int64.add base (Int64.of_int target);
                      }
                      :: t.state.mains;
                  }
            | _ -> ())
          a.records;
      t.state <-
        {
          t.state with
          allocations = !planned_heaps @ t.state.allocations;
          heap_used = !used;
          next_code = !code_cursor;
          next_data = !data_cursor;
        };
      commit t)

let report w =
  let module_json (i : image) =
    `Assoc
      [
        ("id", `Int i.id);
        ("artifact", `String i.name);
        ("base", hex i.base);
        ("body_hex", `String (hex_bytes i.body));
      ]
  in
  let patch_json (r : relocation) =
    `Assoc
      [
        ("module_id", `Int r.module_id);
        ("artifact", `String r.artifact);
        ("record_offset", `Int r.record_offset);
        ("field_offset", `Int r.field_offset);
        ("width", `Int r.width);
        ("kind", `String r.kind);
        ("symbol", name_json r.symbol);
        ("symbol_hex", `String (hex_bytes r.symbol));
        ("operand", hex r.operand);
        ("before", hex r.before);
        ("after", hex r.after);
      ]
  in
  `Assoc
    [
      ("schema", `String "holyc-bin-loader-v1");
      ("transaction_work", `Int w.last_work);
      ("total_work", `Int w.work_used);
      ("images", `List (List.map module_json (images w)));
      ("relocations", `List (List.map patch_json (relocations w)));
      ( "symbols",
        `List
          (List.map
             (fun (name, value) ->
               `Assoc
                 [
                   ("name", name_json name);
                   ("name_hex", `String (hex_bytes name));
                   ("value", hex value);
                 ])
             (symbols w)) );
      ( "unresolved",
        `List
          (List.map
             (fun (p : pending) ->
               `Assoc
                 [
                   ("module_id", `Int p.module_id);
                   ("artifact", `String p.artifact);
                   ("record_offset", `Int p.record_offset);
                   ("symbol", name_json p.symbol);
                   ("symbol_hex", `String (hex_bytes p.symbol));
                   ( "fields",
                     `List
                       (List.map
                          (fun (at, target, wire_type) ->
                            `Assoc
                              [
                                ("record_offset", `Int at);
                                ("field_offset", `Int (32 + target));
                                ("wire_type", `Int wire_type);
                              ])
                          p.fields) );
                 ])
             (unresolved w)) );
      ( "heaps",
        `List
          (List.map
             (fun (h : heap) ->
               `Assoc
                 [
                   ("module_id", `Int h.module_id);
                   ("record_offset", `Int h.record_offset);
                   ("name", name_json h.name);
                   ("name_hex", `String (hex_bytes h.name));
                   ("base", hex h.base);
                   ("size", integer h.size);
                   ("code", `Bool h.code);
                   ("zeroed", `Bool h.zeroed);
                   ( "contents",
                     `String (if h.zeroed then "all-zero" else "unknown") );
                 ])
             (heaps w)) );
      ( "entries",
        `List
          (List.map
             (fun (e : entry) ->
               `Assoc
                 [
                   ("module_id", `Int e.module_id);
                   ("record_offset", `Int e.record_offset);
                   ("address", hex e.address);
                 ])
             (entries w)) );
      ("messages", `List (List.map name_json (List.rev w.messages)));
    ]

let map w =
  let b = Buffer.create 256 in
  List.iter
    (fun (i : image) ->
      Buffer.add_string b
        (Printf.sprintf "MODULE %d %S BASE 0x%016Lx BYTES %d\n" i.id i.name
           i.base (String.length i.body)))
    (images w);
  List.iter
    (fun (r : relocation) ->
      Buffer.add_string b
        (Printf.sprintf
           "PATCH %d RECORD %d FIELD %d WIDTH %d %s %S 0x%016Lx -> 0x%016Lx\n"
           r.module_id r.record_offset r.field_offset r.width r.kind r.symbol
           r.before r.after))
    (relocations w);
  List.iter
    (fun (name, value) ->
      Buffer.add_string b (Printf.sprintf "SYMBOL %S 0x%016Lx\n" name value))
    (symbols w);
  List.iter
    (fun (p : pending) ->
      Buffer.add_string b
        (Printf.sprintf "UNRESOLVED %d %S RECORD %d\n" p.module_id p.symbol
           p.record_offset))
    (unresolved w);
  List.iter
    (fun (h : heap) ->
      Buffer.add_string b
        (Printf.sprintf "HEAP %d RECORD %d %S 0x%016Lx SIZE %Ld %s %s\n"
           h.module_id h.record_offset h.name h.base h.size
           (if h.code then "CODE" else "DATA")
           (if h.zeroed then "ZERO" else "UNKNOWN")))
    (heaps w);
  List.iter
    (fun (e : entry) ->
      Buffer.add_string b
        (Printf.sprintf "MAIN %d RECORD %d 0x%016Lx\n" e.module_id
           e.record_offset e.address))
    (entries w);
  Buffer.contents b
