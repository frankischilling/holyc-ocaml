module Globals = Ir.Integer_globals
module Initialization = Ir.Global_initialization
module Scalar = Ir.Integer_scalar_storage
module Shape = Ir.Integer_storage_shape
module Opcode = Ir.Opcode
module Symbol = Sema.Symbol
module Symbol_map = Map.Make (Symbol.Id)

type error = { code : string; message : string; span : Common.Span.t option }

type slot = {
  source_slot : Globals.storage_slot;
  owner : Ir.Function_body.t option;
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

let create_internal ?initializers ~functions ~max_global_bytes ~initialization
    ~entry () =
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
  let* static_owners =
    List.fold_left
      (fun checked (definition : Ir.Integer_interpreter.function_definition) ->
        let* owners = checked in
        let module Frame = Sema.Function_frame_layout in
        List.fold_left
          (fun checked location ->
            let* owners = checked in
            if Frame.location_kind location <> Frame.Static_local then Ok owners
            else if
              Option.is_none
                (Ir.Function_body.definition_declaration definition.body)
              || not
                   (Ir.Function_body.definition_matches_frame definition.body
                      definition.frame)
            then
              invalid "native static requires its exact compiled function frame"
            else
              let id = Symbol.id (Frame.location_symbol location) in
              if Symbol_map.mem id owners then
                invalid "native static repeats a function/location owner"
              else
                Ok
                  (Symbol_map.add id
                     (definition.body, definition.frame, location)
                     owners))
          (Ok owners)
          (Frame.function_locations definition.frame))
      (Ok Symbol_map.empty) functions
  in
  let* () =
    if Symbol_map.cardinal static_owners = List.length (Globals.statics globals)
    then Ok ()
    else invalid "native function requires every exact static storage location"
  in
  let source_slots =
    List.map
      (fun slot -> (Globals.global_storage slot, Some slot, None))
      (Globals.slots globals)
    @ List.map
        (fun slot -> (Globals.static_storage slot, None, Some slot))
        (Globals.statics globals)
  in
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
    | (source_slot, global, static) :: rest ->
        let symbol = Globals.storage_symbol source_slot in
        let span = span_of_symbol symbol in
        let storage = source_slot in
        let type_ = Globals.storage_type source_slot in
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
        let* owner, allocation_bytes =
          match (global, static) with
          | Some slot, None ->
              if
                not
                  (Symbol.equal_kind (Symbol.kind symbol) Symbol.Global_variable)
              then
                invalid ?span "native global slot does not own a global symbol"
              else if Globals.slot_reuses_declared_storage slot then
                unsupported ?span
                  "native globals do not admit retained declared storage"
              else Ok (None, width)
          | None, Some slot -> (
              let frame = Globals.static_frame slot in
              let location = Globals.static_location slot in
              let module Frame = Sema.Function_frame_layout in
              if
                Option.is_some (Globals.static_array_initializers slot)
                || (Globals.static_initializers slot <> []
                   || Globals.storage_preparation_steps storage <> 0)
                   && Option.is_none initializers
              then
                unsupported ?span
                  "native statics do not admit declaration initializers"
              else if
                Sema.Compiler_option.is_enabled
                  ~mask:(Globals.static_compiler_options slot)
                  Sema.Compiler_option.Globals_on_data_heap
              then
                unsupported ?span
                  "native statics do not admit data-heap options"
              else if
                Frame.location_kind location <> Frame.Static_local
                || (not
                      (Symbol.equal_kind (Symbol.kind symbol)
                         Symbol.Local_variable))
                || Frame.location_declarator_shape location <> Frame.Object
                || Frame.location_value_shape location <> Frame.Scalar
                || Frame.location_dimensions location <> []
                || Frame.location_allocated_size location <> Int64.of_int width
                || Frame.location_element_size location <> Int64.of_int width
                || Frame.location_alignment location <> 8
                || (match Frame.location_register_selection location with
                  | Sema.Register_request.Unspecified
                  | Sema.Register_request.Disabled -> false
                  | Sema.Register_request.Allocatable
                  | Sema.Register_request.Explicit _ -> true)
                || Frame.location_symbol location != symbol
                || (not
                      (Sema.Type.equal
                         (Frame.location_checked_type location)
                         type_))
                || Option.is_some (Frame.location_frame_slot location)
              then
                invalid ?span
                  "native static has another checked frame or location"
              else
                match Symbol_map.find_opt (Symbol.id symbol) static_owners with
                | Some (body, expected_frame, expected_location)
                  when expected_frame == frame && expected_location == location
                  -> Ok (Some body, 8)
                | _ ->
                    invalid ?span
                      "native static requires its unique exact compiled \
                       function")
          | _ -> invalid ?span "native storage has an invalid owner"
        in
        let* () =
          if Globals.storage_element_count storage <> 1 then
            unsupported ?span "native globals do not admit array storage"
          else if Globals.storage_index storage <> ordinal then
            invalid ?span
              "native storage order disagrees with its sealed layout"
          else if allocation_bytes > declared_bytes - byte_offset then
            invalid ?span "native storage width exceeds its sealed byte image"
          else Ok ()
        in
        let* initially_initialized =
          if
            (Option.fold ~none:false
               ~some:(fun slot ->
                 Option.is_some (Globals.slot_initializer slot))
               global
            || Option.fold ~none:false
                 ~some:(fun slot -> Globals.static_initializers slot <> [])
                 static)
            && Option.is_some initializers
          then
            match Globals.storage_initial_bits source_slot with
            | Some bits
              when (Option.fold ~none:false
                      ~some:Globals.slot_initializer_materialized global
                   || Option.fold ~none:false
                        ~some:(fun slot ->
                          List.for_all
                            (Globals.static_root_materialized slot)
                            (Globals.static_initializers slot))
                        static)
                   && (Globals.storage_opcode source_slot = Opcode.Ic_imm_i64
                      || Globals.storage_opcode source_slot = Opcode.Ic_abs_addr
                      ) ->
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
              ( Globals.storage_opcode source_slot,
                Globals.storage_initial_bits source_slot )
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
            owner;
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
          collect (ordinal + 1)
            (byte_offset + allocation_bytes)
            (Symbol_map.add id slot slots)
            rest
  in
  collect 0 0 Symbol_map.empty source_slots

let create ~functions ~max_global_bytes ~initialization ~entry =
  create_internal ~functions ~max_global_bytes ~initialization ~entry ()

let create_prepared ~functions ~initializers ~max_global_bytes ~initialization
    ~entry =
  create_internal ~functions ~initializers ~max_global_bytes ~initialization
    ~entry ()

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

let owns_address slot runtime_owner =
  match (slot.owner, runtime_owner) with
  | None, _ -> true
  | Some expected, Ir.Runtime_call_context.Function actual -> expected == actual
  | Some _, _ -> false
