module Globals = Ir.Integer_globals
module Initialization = Ir.Global_initialization
module Scalar = Ir.Integer_scalar_storage
module Shape = Ir.Integer_storage_shape
module Opcode = Ir.Opcode
module Symbol = Sema.Symbol
module Symbol_map = Map.Make (Symbol.Id)

type error = { code : string; message : string; span : Common.Span.t option }

type slot = {
  source_slot : Globals.slot;
  symbol : Symbol.t;
  type_ : Sema.Type.t;
  scalar : Scalar.t;
  data_offset : int;
  flag_offset : int;
  initially_initialized : bool;
}

type t = {
  globals : Globals.t;
  entry : Ir.X87_stack.t;
  global_bytes : int;
  image : string;
  slots : slot Symbol_map.t;
}

let hard_max_global_bytes = 16 * 1024 * 1024
let hard_max_arena_bytes = 32 * 1024 * 1024
let error ?span code message = Error [ { code; message; span } ]

let validate_global_limit ~max_global_bytes =
  if max_global_bytes <= 0 || max_global_bytes > hard_max_global_bytes then
    error "HCBACK0001"
      (Printf.sprintf "max_global_bytes must be between 1 and %d"
         hard_max_global_bytes)
  else Ok ()

let span_of_symbol symbol =
  match Symbol.origin symbol with
  | Symbol.Source_location location -> Some location.span
  | Symbol.Pinned_source _ | Symbol.Synthesized _ -> None

let create_internal ?initializers ~max_global_bytes ~initialization ~entry () =
  let ( let* ) = Result.bind in
  let* () = validate_global_limit ~max_global_bytes in
  let globals = Initialization.globals initialization in
  let invalid ?span message = error ?span "HCBACK0003" message in
  let unsupported ?span message = error ?span "HCBACK0002" message in
  let resource ?span message = error ?span "HCBACK0001" message in
  let* () =
    if Initialization.matches initialization ~globals ~entry then Ok ()
    else
      invalid
        "native global layout belongs to another initialization/entry bundle"
  in
  let* () =
    if Globals.is_task_command globals then
      unsupported "native globals do not admit retained task storage"
    else if Globals.statics globals <> [] then
      unsupported "native globals do not admit static local storage"
    else if Globals.has_initializers globals && Option.is_none initializers then
      unsupported "native globals do not admit declaration initializers"
    else if
      Initialization.regions initialization <> []
      || Initialization.static_regions initialization <> []
      || Initialization.publications initialization <> []
      || Option.is_some (Initialization.publication_evidence initialization)
      || Initialization.prepared_steps initialization <> 0
         && Option.is_none initializers
    then
      unsupported
        "native globals require an initialization context without initializer \
         regions or preparation"
    else Ok ()
  in
  let* () =
    match initializers with
    | Some proof
      when not
             (Driver.Native_global_initializers.matches_storage proof
                ~initialization ~entry) ->
        invalid "native global preparation belongs to another storage bundle"
    | _ -> Ok ()
  in
  let declared_bytes = Globals.byte_size globals in
  let* () =
    if declared_bytes < 0 then invalid "native global byte size is negative"
    else if declared_bytes > max_global_bytes then
      resource
        (Printf.sprintf
           "global storage requires %d bytes, exceeding max_global_bytes (%d)"
           declared_bytes max_global_bytes)
    else Ok ()
  in
  let source_slots = Globals.slots globals in
  let slot_count = List.length source_slots in
  let* arena_bytes =
    if slot_count > hard_max_arena_bytes - declared_bytes then
      resource
        (Printf.sprintf
           "private global arena exceeds the hard allocation bound of %d bytes"
           hard_max_arena_bytes)
    else Ok (declared_bytes + slot_count)
  in
  let* () =
    if arena_bytes > hard_max_arena_bytes || arena_bytes > Sys.max_string_length
    then
      resource
        (Printf.sprintf
           "private global arena exceeds the hard allocation bound of %d bytes"
           hard_max_arena_bytes)
    else Ok ()
  in
  let image = Bytes.make arena_bytes '\000' in
  let rec collect ordinal byte_offset slots = function
    | [] ->
        if byte_offset <> declared_bytes then
          invalid
            "native global packed widths disagree with the sealed semantic \
             byte size"
        else
          Ok
            {
              globals;
              entry;
              global_bytes = declared_bytes;
              image = Bytes.to_string image;
              slots;
            }
    | source_slot :: rest ->
        let symbol = Globals.slot_symbol source_slot in
        let span = span_of_symbol symbol in
        let storage = Globals.global_storage source_slot in
        let type_ = Globals.slot_type source_slot in
        let* scalar =
          match
            ( Scalar.of_type type_,
              Scalar.public_byte_size type_,
              Globals.storage_dimensions storage )
          with
          | Some scalar, Some width, [] when width = Scalar.byte_size scalar ->
              Ok scalar
          | _ ->
              unsupported ?span
                "native globals require public nonzero scalar integer objects"
        in
        let width = Scalar.byte_size scalar in
        let* () =
          if Symbol.equal_kind (Symbol.kind symbol) Symbol.Global_variable then
            Ok ()
          else invalid ?span "native global slot does not own a global symbol"
        in
        let* () =
          if Globals.slot_reuses_declared_storage source_slot then
            unsupported ?span
              "native globals do not admit retained declared storage"
          else if Globals.storage_element_count storage <> 1 then
            unsupported ?span "native globals do not admit array storage"
          else if Globals.slot_index source_slot <> ordinal then
            invalid ?span
              "native global slot order disagrees with its sealed layout"
          else if width > declared_bytes - byte_offset then
            invalid ?span "native global width exceeds its sealed byte image"
          else Ok ()
        in
        let* initially_initialized =
          if
            Option.is_some (Globals.slot_initializer source_slot)
            && Option.is_some initializers
          then
            match Globals.slot_initial_bits source_slot with
            | Some bits
              when Globals.slot_initializer_materialized source_slot
                   && (Globals.slot_opcode source_slot = Opcode.Ic_imm_i64
                      || Globals.slot_opcode source_slot = Opcode.Ic_abs_addr)
              ->
                for byte = 0 to width - 1 do
                  Bytes.set image (byte_offset + byte)
                    (Char.chr
                       (Int64.to_int
                          (Int64.logand 255L
                             (Int64.shift_right_logical bits (byte * 8)))))
                done;
                Ok true
            | _ -> invalid ?span "native global has no prepared scalar image"
          else
            match
              ( Globals.slot_opcode source_slot,
                Globals.slot_initial_bits source_slot )
            with
            | Opcode.Ic_imm_i64, None -> Ok false
            | Opcode.Ic_abs_addr, Some bits when Int64.equal bits 0L -> Ok true
            | Opcode.Ic_imm_i64, Some _ | Opcode.Ic_abs_addr, None ->
                invalid ?span
                  "native global initial state disagrees with its checked \
                   address mode"
            | Opcode.Ic_abs_addr, Some _ ->
                unsupported ?span
                  "native globals do not admit prepared declaration values"
            | _ ->
                unsupported ?span
                  "native global uses an unsupported checked address opcode"
        in
        let flag_offset = declared_bytes + ordinal in
        if initially_initialized then Bytes.set image flag_offset '\001';
        let slot =
          {
            source_slot;
            symbol;
            type_;
            scalar;
            data_offset = byte_offset;
            flag_offset;
            initially_initialized;
          }
        in
        let id = Symbol.id symbol in
        if Symbol_map.mem id slots then
          invalid ?span "native global layout repeats a symbol identity"
        else
          collect (ordinal + 1) (byte_offset + width)
            (Symbol_map.add id slot slots)
            rest
  in
  collect 0 0 Symbol_map.empty source_slots

let create ~max_global_bytes ~initialization ~entry =
  create_internal ~max_global_bytes ~initialization ~entry ()

let create_prepared ~initializers ~max_global_bytes ~initialization ~entry =
  create_internal ~initializers ~max_global_bytes ~initialization ~entry ()

let globals layout = layout.globals
let entry layout = layout.entry
let global_bytes layout = layout.global_bytes
let image layout = layout.image
let is_empty layout = Symbol_map.is_empty layout.slots

let find_symbol layout symbol =
  match Symbol_map.find_opt (Symbol.id symbol) layout.slots with
  | Some slot when slot.symbol == symbol -> Some slot
  | Some _ | None -> None

let source_slot slot = slot.source_slot
let symbol slot = slot.symbol
let type_ slot = slot.type_
let scalar slot = slot.scalar
let data_offset slot = slot.data_offset
let flag_offset slot = slot.flag_offset
let initially_initialized slot = slot.initially_initialized
