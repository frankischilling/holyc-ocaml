module Globals = Ir.Integer_globals
module Initialization = Ir.Global_initialization
module Scalar = Ir.Integer_scalar_storage
module Shape = Ir.Integer_storage_shape
module Arrays = Ir.Integer_array_initializers
module Layout = Ir.Integer_initializer_layout
module Opcode = Ir.Opcode
module Symbol = Sema.Symbol
module Symbol_map = Map.Make (Symbol.Id)
module Literals = X86_64_literal_storage

type error = { code : string; message : string; span : Common.Span.t option }

type slot = {
  source_slot : Globals.storage_slot;
  owner : Ir.Function_body.t option;
  static_source : Ir.Integer_static_allocation.t option;
  symbol : Symbol.t;
  type_ : Sema.Type.t;
  callback : Sema.Function_type_resolution.function_pointer option;
  code_owner_offset : int option;
  scalar : Scalar.t;
  dimensions : int64 list;
  strides : int64 list;
  element_count : int;
  extent_bytes : int;
  data_offset : int;
  flag_offset : int;
  initially_initialized : bool;
}

type t = {
  globals : Globals.t;
  entry : Ir.X87_stack.t;
  global_bytes : int;
  image : string;
  zero_bytes : int option;
  slots : slot Symbol_map.t;
  retained_slots : (Ir.Retained_global.t * slot) Symbol_map.t;
}

type task_layout_state = {
  task_owner : Globals.t option;
  task_slots : (Ir.Retained_global.t * slot) Symbol_map.t;
  task_symbols : slot Symbol_map.t;
  task_global_bytes : int;
  task_arena_bytes : int;
  task_layout_work : int;
  task_literals : Literals.t;
}

type task_layout = {
  max_task_global_bytes : int;
  max_task_layout_work : int;
  max_task_literal_bytes : int;
  task_state : task_layout_state Atomic.t;
  task_arena_claimed : bool Atomic.t;
}

type task_snapshot = {
  task_layout : task_layout;
  task_storage : t;
  task_state_snapshot : task_layout_state;
}

type static_reservation = {
  reservation_layout : task_layout;
  reservation_state : task_layout_state;
  reservation_request : Driver.Integer_task.Native_static_allocation.request;
}

type static_copy = {
  copy_layout : task_layout;
  copy_state : task_layout_state;
  copy_request : Driver.Integer_task.Native_static_copy.request;
  copy_data : int;
  copy_flag : int;
  copy_bytes : string;
}

let hard_max_global_bytes = 16 * 1024 * 1024
let hard_max_arena_bytes = 32 * 1024 * 1024
let hard_max_task_layout_work = 1_000_000
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

let storage_shape_type ?span storage =
  let type_ = Globals.storage_type storage in
  match Globals.storage_callback_pointer storage with
  | None -> Ok type_
  | Some pointer ->
      let module Headers = Sema.Function_type_resolution in
      if
        List.length (Headers.function_pointer_indirection_origins pointer) <> 1
        || not
             (Sema.Type.equal type_
                (Headers.function_pointer_storage_type pointer |> Result.get_ok))
      then
        error ?span "HCBACK0003"
          "native callback storage requires its original one-star header"
      else
        Ok
          (Sema.Type.make_primitive ~form:Sema.Type.Public_spelling
             ~primitive:Sema.Primitive_type.I64 ~pointer_depth:0
          |> Result.get_ok)

let write_word image ~offset ~width bits =
  for byte = 0 to width - 1 do
    Bytes.set image (offset + byte)
      (Char.chr
         (Int64.to_int
            (Int64.logand 255L (Int64.shift_right_logical bits (byte * 8)))))
  done

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
      Initialization.static_regions initialization <> []
      || Initialization.regions initialization <> []
         && Option.is_none initializers
      || (Initialization.publications initialization <> []
         || Option.is_some (Initialization.publication_evidence initialization)
         )
         && Option.is_none initializers
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
  let* array_flag_bytes =
    List.fold_left
      (fun checked (storage, _, _) ->
        let* total = checked in
        if Globals.storage_dimensions storage = [] then Ok total
        else
          let elements = Globals.storage_element_count storage in
          if elements <= 0 || elements > (hard_max_arena_bytes - total) / 8 then
            resource
              (Printf.sprintf
                 "private global arena exceeds the hard allocation bound of %d \
                  bytes"
                 hard_max_arena_bytes)
          else Ok (total + (elements * 8)))
      (Ok 0) source_slots
  in
  let* code_owner_bytes =
    List.fold_left
      (fun checked (storage, _, _) ->
        let* total = checked in
        if Option.is_none (Globals.storage_callback_pointer storage) then
          Ok total
        else
          let elements = Globals.storage_element_count storage in
          if elements <= 0 || elements > (hard_max_arena_bytes - total) / 8 then
            resource
              "private callback ownership exceeds the arena allocation bound"
          else Ok (total + (elements * 8)))
      (Ok 0) source_slots
  in
  let* arena_bytes =
    if slot_count > hard_max_arena_bytes - declared_bytes then
      resource
        (Printf.sprintf
           "private global arena exceeds the hard allocation bound of %d bytes"
           hard_max_arena_bytes)
    else
      let prefix_bytes = declared_bytes + slot_count in
      if array_flag_bytes > hard_max_arena_bytes - prefix_bytes then
        resource
          (Printf.sprintf
             "private global arena exceeds the hard allocation bound of %d \
              bytes"
             hard_max_arena_bytes)
      else if
        code_owner_bytes
        > hard_max_arena_bytes - prefix_bytes - array_flag_bytes
      then
        resource "private callback ownership exceeds the arena allocation bound"
      else Ok (prefix_bytes + array_flag_bytes + code_owner_bytes)
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
  let prepared_array_image ?span global static =
    let collect entries root_materialized is_load =
      List.fold_left
        (fun checked entry ->
          let* reversed = checked in
          let root = Arrays.root entry in
          match Arrays.prepared entry with
          | Some (payload, _) when root_materialized root ->
              Ok
                ((Layout.cell_offset (Arrays.destination entry), payload)
                :: reversed)
          | Some _ ->
              invalid ?span
                "native array image does not retain its exact materialized root"
          | None when is_load root -> Ok reversed
          | None -> invalid ?span "native array has no prepared source image")
        (Ok []) entries
      |> Result.map List.rev
    in
    match (global, static) with
    | Some slot, None -> (
        match Globals.slot_array_initializers slot with
        | None -> Ok []
        | Some arrays ->
            collect (Arrays.entries arrays)
              (Globals.slot_root_materialized slot) (fun root ->
                Option.fold ~none:false
                  ~some:(fun proof ->
                    Driver.Native_global_initializers.is_load_root proof slot
                      root)
                  initializers))
    | None, Some slot -> (
        match Globals.static_array_initializers slot with
        | None -> Ok []
        | Some arrays ->
            collect (Arrays.entries arrays)
              (Globals.static_root_materialized slot) (fun _ -> false))
    | _ -> invalid ?span "native storage has an invalid initializer owner"
  in
  let rec collect ordinal cell_index byte_offset array_flag_cursor owner_cursor
      slots = function
    | [] ->
        if
          byte_offset <> declared_bytes
          || cell_index <> Globals.cell_count globals
          || array_flag_cursor <> arena_bytes - code_owner_bytes
          || owner_cursor <> arena_bytes
        then
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
              zero_bytes = None;
              slots;
              retained_slots = Symbol_map.empty;
            }
    | (source_slot, global, static) :: rest ->
        let symbol = Globals.storage_symbol source_slot in
        let span = span_of_symbol symbol in
        let storage = source_slot in
        let type_ = Globals.storage_type source_slot in
        let callback = Globals.storage_callback_pointer source_slot in
        let* shape_type = storage_shape_type ?span source_slot in
        let dimensions = Globals.storage_dimensions storage in
        let* shape =
          match Shape.create ~type_:shape_type ~dimensions with
          | Ok shape -> Ok shape
          | Error Shape.Unsupported_type ->
              unsupported ?span
                "native globals require public nonzero integer objects"
          | Error (Shape.Invalid_extent | Shape.Overflow) ->
              invalid ?span "native storage has an invalid checked array shape"
        in
        let scalar = Shape.scalar shape in
        let width = Scalar.byte_size scalar in
        let strides = Shape.strides shape in
        let element_count = Shape.element_count shape in
        let extent_bytes = Shape.byte_size shape in
        let is_array = dimensions <> [] in
        let* () =
          if
            dimensions <> Shape.dimensions shape
            || strides <> Globals.storage_strides storage
            || element_count <> Globals.storage_element_count storage
          then
            invalid ?span
              "native storage shape disagrees with its sealed layout"
          else Ok ()
        in
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
              else Ok (None, extent_bytes)
          | None, Some slot -> (
              let frame = Globals.static_frame slot in
              let location = Globals.static_location slot in
              let module Frame = Sema.Function_frame_layout in
              if
                (Globals.static_initializers slot <> []
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
                let frame_dimensions =
                  Frame.location_dimensions location
                  |> List.map Frame.dimension_value
                in
                Frame.location_kind location <> Frame.Static_local
                || (not
                      (Symbol.equal_kind (Symbol.kind symbol)
                         Symbol.Local_variable))
                || (Frame.location_declarator_shape location
                   <>
                   if Option.is_some callback then Frame.Function_pointer
                   else Frame.Object)
                || (match
                      (Frame.location_callback_pointer location, callback)
                    with
                  | None, None -> false
                  | Some actual, Some expected -> actual != expected
                  | _ -> true)
                || (Frame.location_value_shape location
                   <> if is_array then Frame.Array else Frame.Scalar)
                || frame_dimensions <> dimensions
                || is_array
                   && not (Frame.location_source_dimensions_checked location)
                || Frame.location_allocated_size location
                   <> Int64.of_int extent_bytes
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
                         (Frame.location_storage_type location |> Result.get_ok)
                         type_))
                || Option.is_some (Frame.location_frame_slot location)
              then
                invalid ?span
                  "native static has another checked frame or location"
              else
                let* padded =
                  match Shape.padded_byte_size shape with
                  | Some bytes -> Ok bytes
                  | None ->
                      invalid ?span
                        "native static padded storage exceeds the host integer \
                         range"
                in
                match Symbol_map.find_opt (Symbol.id symbol) static_owners with
                | Some (body, expected_frame, expected_location)
                  when expected_frame == frame && expected_location == location
                  -> Ok (Some body, padded)
                | _ ->
                    invalid ?span
                      "native static requires its unique exact compiled \
                       function")
          | _ -> invalid ?span "native storage has an invalid owner"
        in
        let* () =
          if Globals.storage_index storage <> cell_index then
            invalid ?span
              "native storage order disagrees with its sealed layout"
          else if allocation_bytes > declared_bytes - byte_offset then
            invalid ?span "native storage width exceeds its sealed byte image"
          else Ok ()
        in
        let flag_offset, next_array_flag_cursor =
          if is_array then
            ( array_flag_cursor + ((element_count - 1) * 8),
              array_flag_cursor + (element_count * 8) )
          else (declared_bytes + ordinal, array_flag_cursor)
        in
        let flag_at index =
          if is_array then flag_offset - (index * 8) else flag_offset
        in
        let has_initializer =
          Option.fold ~none:false
            ~some:(fun slot -> Globals.slot_initializers slot <> [])
            global
          || Option.fold ~none:false
               ~some:(fun slot -> Globals.static_initializers slot <> [])
               static
        in
        let scalar_materialized =
          Option.fold ~none:false ~some:Globals.slot_initializer_materialized
            global
          || Option.fold ~none:false
               ~some:(fun slot ->
                 List.for_all
                   (Globals.static_root_materialized slot)
                   (Globals.static_initializers slot))
               static
        in
        let* base_initialized =
          if (not is_array) && has_initializer && Option.is_some initializers
          then
            if
              Option.fold ~none:false
                ~some:(fun slot ->
                  Driver.Native_global_initializers.is_load_slot
                    (Option.get initializers) slot)
                global
            then (
              write_word image ~offset:byte_offset ~width 0L;
              Ok true)
            else
              match Globals.storage_initial_bits source_slot with
              | Some bits
                when scalar_materialized
                     && (Globals.storage_opcode source_slot = Opcode.Ic_imm_i64
                        || Globals.storage_opcode source_slot
                           = Opcode.Ic_abs_addr) ->
                  write_word image ~offset:byte_offset ~width bits;
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
        if base_initialized then
          for index = 0 to element_count - 1 do
            Bytes.set image (flag_at index) '\001'
          done;
        let* array_image =
          if is_array then prepared_array_image ?span global static else Ok []
        in
        let seen = Bytes.make element_count '\000' in
        let initialized_count =
          ref (if base_initialized then element_count else 0)
        in
        let mark index =
          Bytes.set seen index '\001';
          if not base_initialized then incr initialized_count;
          Bytes.set image (flag_at index) '\001'
        in
        let* () =
          List.fold_left
            (fun checked (cell_offset, payload) ->
              let* () = checked in
              match payload with
              | Arrays.Word bits ->
                  if
                    cell_offset < 0
                    || cell_offset >= element_count
                    || Bytes.get seen cell_offset <> '\000'
                  then
                    invalid ?span
                      "native array word image has an invalid or repeated \
                       destination"
                  else (
                    write_word image
                      ~offset:(byte_offset + (cell_offset * width))
                      ~width bits;
                    mark cell_offset;
                    Ok ())
              | Arrays.Bytes bytes ->
                  let length = String.length bytes in
                  if
                    width <> 1 || cell_offset < 0 || length <= 0
                    || cell_offset > element_count - length
                  then
                    invalid ?span
                      "native array byte image exceeds its exact object extent"
                  else
                    let duplicate = ref false in
                    for index = cell_offset to cell_offset + length - 1 do
                      if Bytes.get seen index <> '\000' then duplicate := true
                    done;
                    if !duplicate then
                      invalid ?span
                        "native array byte image repeats a prepared destination"
                    else (
                      Bytes.blit_string bytes 0 image
                        (byte_offset + cell_offset)
                        length;
                      for index = cell_offset to cell_offset + length - 1 do
                        mark index
                      done;
                      Ok ()))
            (Ok ()) array_image
        in
        let initially_initialized = !initialized_count = element_count in
        let code_owner_offset = Option.map (fun _ -> owner_cursor) callback in
        let next_owner_cursor =
          if Option.is_some callback then owner_cursor + (element_count * 8)
          else owner_cursor
        in
        let slot =
          {
            source_slot;
            owner;
            static_source = None;
            symbol;
            type_;
            callback;
            code_owner_offset;
            scalar;
            dimensions;
            strides;
            element_count;
            extent_bytes;
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
            (cell_index + element_count)
            (byte_offset + allocation_bytes)
            next_array_flag_cursor next_owner_cursor
            (Symbol_map.add id slot slots)
            rest
  in
  collect 0 0 0
    (declared_bytes + slot_count)
    (arena_bytes - code_owner_bytes)
    Symbol_map.empty source_slots

let create ~functions ~max_global_bytes ~initialization ~entry =
  create_internal ~functions ~max_global_bytes ~initialization ~entry ()

let create_prepared ~functions ~initializers ~max_global_bytes ~initialization
    ~entry =
  create_internal ~functions ~initializers ~max_global_bytes ~initialization
    ~entry ()

let create_task_layout ?(max_layout_work = hard_max_task_layout_work)
    ?(max_literal_bytes = 1_048_576) ~max_global_bytes () =
  let ( let* ) = Result.bind in
  let* () = validate_global_limit ~max_global_bytes in
  let* () =
    Literals.validate_limit ~max_literal_bytes
    |> Result.map_error
         (List.map (fun (error : Literals.error) ->
              { code = error.code; message = error.message; span = error.span }))
  in
  let* () =
    if max_layout_work <= 0 || max_layout_work > hard_max_task_layout_work then
      error "HCBACK0001"
        "native task layout work limit is outside its positive host bound"
    else Ok ()
  in
  Ok
    {
      max_task_global_bytes = max_global_bytes;
      max_task_layout_work = max_layout_work;
      max_task_literal_bytes = max_literal_bytes;
      task_arena_claimed = Atomic.make false;
      task_state =
        Atomic.make
          {
            task_owner = None;
            task_slots = Symbol_map.empty;
            task_symbols = Symbol_map.empty;
            task_global_bytes = 0;
            task_arena_bytes = 0;
            task_layout_work = 0;
            task_literals = Literals.empty;
          };
    }

let claim_task_arena layout =
  if Atomic.compare_and_set layout.task_arena_claimed false true then Ok ()
  else Error "native task layout already has its original arena owner"

let append_task_globals layout ~globals =
  let ( let* ) = Result.bind in
  let invalid ?span message = error ?span "HCBACK0003" message in
  let unsupported ?span message = error ?span "HCBACK0002" message in
  let resource ?span message = error ?span "HCBACK0001" message in
  let before = Atomic.get layout.task_state in
  let* () =
    if
      (not (Globals.is_task_command globals))
      || Globals.compilation_mode globals <> Sema.Global_resolution.Jit
    then
      invalid "native task storage requires an original retained JIT snapshot"
    else if
      Option.fold ~none:false
        ~some:(fun owner -> not (Globals.same_task_storage owner globals))
        before.task_owner
    then invalid "native task storage belongs to another original task"
    else Ok ()
  in
  let bindings = Globals.retained_storage_bindings globals in
  let declared =
    Globals.storage_slots globals
    |> List.filter (fun storage ->
        Option.is_none (Globals.storage_frame storage))
  in
  let required_work = List.length bindings + List.length declared in
  let* () =
    if required_work > layout.max_task_layout_work - before.task_layout_work
    then
      resource
        "native task layout exceeds its cumulative retained-binding work limit"
    else Ok ()
  in
  let* after, visible =
    List.fold_left
      (fun checked (reference, storage) ->
        let* state, visible = checked in
        let symbol = Globals.storage_symbol storage in
        let span = span_of_symbol symbol in
        let* () =
          if Ir.Retained_global.symbol reference != symbol then
            invalid ?span
              "native task reference has another original storage symbol"
          else if Symbol_map.mem (Symbol.id symbol) visible then
            invalid ?span
              "native task snapshot repeats a retained storage identity"
          else Ok ()
        in
        let* state, slot =
          match Symbol_map.find_opt (Symbol.id symbol) state.task_slots with
          | Some (candidate, slot) ->
              if
                Ir.Retained_global.same candidate reference
                && Globals.same_storage slot.source_slot storage
                && slot.symbol == symbol
                && Sema.Type.equal slot.type_ (Globals.storage_type storage)
                && Globals.storage_opcode slot.source_slot
                   = Globals.storage_opcode storage
              then
                let callback = Globals.storage_callback_pointer storage in
                let same_origin =
                  match (slot.callback, callback) with
                  | None, None -> true
                  | Some earlier, Some later -> (
                      let module Headers = Sema.Function_type_resolution in
                      match
                        ( Headers.function_pointer_source earlier,
                          Headers.function_pointer_source later )
                      with
                      | Some earlier, Some later -> earlier == later
                      | _ -> earlier == later)
                  | _ -> false
                in
                if not same_origin then
                  invalid ?span
                    "native task callback replaced its original source header"
                else
                  let slot = { slot with callback } in
                  Ok
                    ( {
                        state with
                        task_slots =
                          Symbol_map.add (Symbol.id symbol) (candidate, slot)
                            state.task_slots;
                        task_symbols =
                          Symbol_map.add (Symbol.id symbol) slot
                            state.task_symbols;
                      },
                      slot )
              else
                invalid ?span
                  "native task reference replaced its original storage object"
          | None ->
              let* () =
                if Option.is_some (Globals.storage_frame storage) then
                  unsupported ?span
                    "native task storage requires original global ownership"
                else if
                  Globals.storage_opcode storage <> Opcode.Ic_imm_i64
                  || Option.is_some (Globals.storage_initial_bits storage)
                  || Globals.storage_array_image storage <> []
                then
                  invalid ?span
                    "native task allocation must retain its original \
                     uninitialized JIT storage"
                else Ok ()
              in
              let type_ = Globals.storage_type storage in
              let callback = Globals.storage_callback_pointer storage in
              let* shape_type = storage_shape_type ?span storage in
              let dimensions = Globals.storage_dimensions storage in
              let* shape =
                match Shape.create ~type_:shape_type ~dimensions with
                | Ok shape -> Ok shape
                | Error Shape.Unsupported_type ->
                    unsupported ?span
                      "native task globals require nonzero public integer \
                       storage"
                | Error (Shape.Invalid_extent | Shape.Overflow) ->
                    invalid ?span
                      "native task storage has an invalid checked array shape"
              in
              let scalar = Shape.scalar shape in
              let width = Scalar.byte_size scalar in
              let strides = Shape.strides shape in
              let element_count = Shape.element_count shape in
              let extent_bytes = Shape.byte_size shape in
              let is_array = dimensions <> [] in
              let* () =
                if
                  strides <> Globals.storage_strides storage
                  || element_count <> Globals.storage_element_count storage
                then
                  invalid ?span
                    "native task storage shape disagrees with its original \
                     checked layout"
                else if
                  extent_bytes
                  > layout.max_task_global_bytes - state.task_global_bytes
                then
                  resource ?span "native task globals exceed max_global_bytes"
                else Ok ()
              in
              let* data_and_flags_bytes, flag_offset =
                if is_array then
                  if
                    extent_bytes > hard_max_arena_bytes - state.task_arena_bytes
                  then
                    resource ?span
                      "native task data and initialization flags exceed the \
                       arena bound"
                  else
                    let remaining =
                      hard_max_arena_bytes - state.task_arena_bytes
                      - extent_bytes
                    in
                    if element_count > remaining / 8 then
                      resource ?span
                        "native task data and initialization flags exceed the \
                         arena bound"
                    else
                      let flag_bytes = element_count * 8 in
                      Ok
                        ( extent_bytes + flag_bytes,
                          state.task_arena_bytes + extent_bytes + flag_bytes - 8
                        )
                else if
                  width + 1 > hard_max_arena_bytes - state.task_arena_bytes
                then
                  resource ?span
                    "native task data and initialization flags exceed the \
                     arena bound"
                else Ok (width + 1, state.task_arena_bytes + width)
              in
              let* arena_bytes, code_owner_offset =
                if Option.is_none callback then Ok (data_and_flags_bytes, None)
                else if
                  element_count
                  > (hard_max_arena_bytes - state.task_arena_bytes
                   - data_and_flags_bytes)
                    / 8
                then
                  resource ?span
                    "native task callback owners exceed the arena bound"
                else
                  Ok
                    ( data_and_flags_bytes + (element_count * 8),
                      Some (state.task_arena_bytes + data_and_flags_bytes) )
              in
              let slot =
                {
                  source_slot = storage;
                  owner = None;
                  static_source = None;
                  symbol;
                  type_;
                  callback;
                  code_owner_offset;
                  scalar;
                  dimensions;
                  strides;
                  element_count;
                  extent_bytes;
                  data_offset = state.task_arena_bytes;
                  flag_offset;
                  initially_initialized = false;
                }
              in
              Ok
                ( {
                    state with
                    task_slots =
                      Symbol_map.add (Symbol.id symbol) (reference, slot)
                        state.task_slots;
                    task_symbols =
                      Symbol_map.add (Symbol.id symbol) slot state.task_symbols;
                    task_global_bytes = state.task_global_bytes + extent_bytes;
                    task_arena_bytes = state.task_arena_bytes + arena_bytes;
                  },
                  slot )
        in
        Ok (state, Symbol_map.add (Symbol.id symbol) slot visible))
      (Ok (before, Symbol_map.empty))
      bindings
  in
  let* () =
    if
      List.for_all
        (fun storage ->
          match
            Symbol_map.find_opt
              (Symbol.id (Globals.storage_symbol storage))
              visible
          with
          | Some slot -> Globals.same_storage storage slot.source_slot
          | None -> false)
        declared
    then Ok ()
    else
      invalid "native task fragment has storage outside its retained snapshot"
  in
  let after =
    {
      after with
      task_owner = Some globals;
      task_layout_work = before.task_layout_work + required_work;
    }
  in
  if Atomic.compare_and_set layout.task_state before after then Ok after
  else invalid "native task storage changed during fragment admission"

let create_task_snapshot ?(functions = []) layout ~initialization ~entry =
  let ( let* ) = Result.bind in
  let globals = Initialization.globals initialization in
  let* () =
    if not (Initialization.matches initialization ~globals ~entry) then
      error "HCBACK0003"
        "native task storage requires its original entry and initialization"
    else if
      List.exists
        (fun slot ->
          Sema.Compiler_option.is_enabled
            ~mask:(Globals.static_compiler_options slot)
            Sema.Compiler_option.Globals_on_data_heap
          || Option.is_none (Globals.static_source_allocation slot)
          || not
               (List.for_all
                  (Globals.static_root_executed slot)
                  (Globals.static_initializers slot)))
        (Globals.statics globals)
      || Initialization.static_regions initialization <> []
      || Initialization.publications initialization <> []
      || Option.is_some (Initialization.publication_evidence initialization)
      || Initialization.prepared_steps initialization <> 0
    then
      error "HCBACK0002"
        "native task fragments do not admit static storage or prepared image \
         publications"
    else Ok ()
  in
  let* after = append_task_globals layout ~globals in
  let* after =
    let* symbols =
      List.fold_left
        (fun checked static ->
          let* symbols = checked in
          let allocation =
            Option.get (Globals.static_source_allocation static)
          in
          let symbol = Globals.storage_symbol (Globals.static_storage static) in
          let* definition =
            match
              List.filter
                (fun (definition : Ir.Integer_interpreter.function_definition)
                   ->
                  definition.frame == Globals.static_frame static
                  && Ir.Function_body.definition_matches_frame definition.body
                       definition.frame
                  && Option.is_some
                       (Ir.Function_body.definition_declaration definition.body))
                functions
            with
            | [ definition ] -> Ok definition
            | _ ->
                error "HCBACK0003"
                  "native task static lacks its unique original completed \
                   function body"
          in
          match Symbol_map.find_opt (Symbol.id symbol) symbols with
          | Some slot
            when slot.symbol == symbol
                 && Option.fold ~none:false ~some:(( == ) allocation)
                      slot.static_source
                 && Globals.same_storage slot.source_slot
                      (Globals.static_storage static)
                 && Option.fold ~none:true ~some:(( == ) definition.body)
                      slot.owner ->
              Ok
                (Symbol_map.add (Symbol.id symbol)
                   {
                     slot with
                     source_slot = Globals.static_storage static;
                     owner = Some definition.body;
                   }
                   symbols)
          | _ ->
              error "HCBACK0003"
                "native task static replaced its original arena allocation")
        (Ok after.task_symbols) (Globals.statics globals)
    in
    if symbols == after.task_symbols then Ok after
    else
      let joined = { after with task_symbols = symbols } in
      if Atomic.compare_and_set layout.task_state after joined then Ok joined
      else
        error "HCBACK0003"
          "native task storage changed during original static completion"
  in
  let task_storage =
    {
      globals;
      entry;
      global_bytes = after.task_global_bytes;
      image = "";
      zero_bytes = Some after.task_arena_bytes;
      slots = after.task_symbols;
      retained_slots = after.task_slots;
    }
  in
  Ok { task_layout = layout; task_storage; task_state_snapshot = after }

let reserve_static layout request =
  let ( let* ) = Result.bind in
  let module Request = Driver.Integer_task.Native_static_allocation in
  let* () =
    Request.check request
    |> Result.map_error (fun message ->
        [ { code = "HCBACK0003"; message; span = None } ])
  in
  let globals = Request.context request in
  let allocation = Request.allocation request in
  let* before = append_task_globals layout ~globals in
  let symbol = Ir.Integer_static_allocation.symbol allocation in
  let span = span_of_symbol symbol in
  let invalid message = error ?span "HCBACK0003" message in
  let resource message = error ?span "HCBACK0001" message in
  let* after =
    match Symbol_map.find_opt (Symbol.id symbol) before.task_symbols with
    | Some slot ->
        if
          slot.symbol == symbol
          && Option.fold ~none:false ~some:(( == ) allocation)
               slot.static_source
        then Ok before
        else
          invalid "static allocation replaced its original native storage owner"
    | None ->
        let shape = Ir.Integer_static_allocation.shape allocation in
        let scalar = Shape.scalar shape in
        let elements = Shape.element_count shape in
        let dimensions = Shape.dimensions shape in
        let* padded =
          match Shape.padded_byte_size shape with
          | Some bytes -> Ok bytes
          | None -> resource "native static padded extent overflows"
        in
        let* () =
          if before.task_layout_work >= layout.max_task_layout_work then
            resource "native static allocation exceeds cumulative layout work"
          else if
            padded > layout.max_task_global_bytes - before.task_global_bytes
          then resource "native statics exceed max_global_bytes"
          else Ok ()
        in
        let flag_width = if dimensions = [] then 1 else 8 in
        let* arena_bytes =
          if
            padded > hard_max_arena_bytes - before.task_arena_bytes
            || elements
               > (hard_max_arena_bytes - before.task_arena_bytes - padded)
                 / flag_width
          then resource "native static data and flags exceed the arena bound"
          else Ok (padded + (elements * flag_width))
        in
        let slot =
          {
            source_slot = Globals.declared_static_storage allocation;
            owner = None;
            static_source = Some allocation;
            symbol;
            type_ = Ir.Integer_static_allocation.type_ allocation;
            callback = None;
            code_owner_offset = None;
            scalar;
            dimensions;
            strides = Shape.strides shape;
            element_count = elements;
            extent_bytes = Shape.byte_size shape;
            data_offset = before.task_arena_bytes;
            flag_offset =
              before.task_arena_bytes + padded + ((elements - 1) * flag_width);
            initially_initialized = false;
          }
        in
        Ok
          {
            before with
            task_symbols =
              Symbol_map.add (Symbol.id symbol) slot before.task_symbols;
            task_global_bytes = before.task_global_bytes + padded;
            task_arena_bytes = before.task_arena_bytes + arena_bytes;
            task_layout_work = before.task_layout_work + 1;
          }
  in
  if after == before || Atomic.compare_and_set layout.task_state before after
  then
    Ok
      {
        reservation_layout = layout;
        reservation_state = after;
        reservation_request = request;
      }
  else invalid "native task storage changed during static allocation"

let check_static_reservation reservation ~layout ~request =
  if
    reservation.reservation_layout != layout
    || reservation.reservation_request != request
    || Atomic.get layout.task_state != reservation.reservation_state
  then
    Error "native static reservation is foreign or precedes the current layout"
  else Driver.Integer_task.Native_static_allocation.check request

let static_reservation_arena_bytes reservation =
  reservation.reservation_state.task_arena_bytes

let static_reservation_initializations_since reservation ~arena_prefix_bytes =
  Literals.initializations_since reservation.reservation_state.task_literals
    ~arena_prefix_bytes

let prepare_static_copy layout request ~admitted_arena_bytes =
  let ( let* ) = Result.bind in
  let module Request = Driver.Integer_task.Native_static_copy in
  let module Destination = Ir.Static_initializer_destination in
  let* () = Request.check request in
  let destination = Request.destination request in
  let allocation = Destination.allocation destination in
  let symbol = Ir.Integer_static_allocation.symbol allocation in
  let state = Atomic.get layout.task_state in
  let* () =
    if
      state.task_arena_bytes <> admitted_arena_bytes
      || not
           (Option.fold ~none:false
              ~some:(fun owner ->
                Globals.same_task_storage owner
                  (Destination.globals destination))
              state.task_owner)
    then Error "native static copy requires its original admitted task arena"
    else Ok ()
  in
  let* slot =
    match Symbol_map.find_opt (Symbol.id symbol) state.task_symbols with
    | Some slot
      when slot.symbol == symbol
           && Option.fold ~none:false ~some:(( == ) allocation)
                slot.static_source
           && Globals.same_storage slot.source_slot
                (Destination.storage destination) -> Ok slot
    | _ -> Error "native static copy replaced its original private allocation"
  in
  let* bytes =
    match Destination.operation destination with
    | Layout.Copy_bytes bytes -> Ok bytes
    | Scalar_store ->
        Error "native static copy requires its original byte-copy operation"
  in
  let cell = Destination.cell_offset destination in
  let byte = Destination.byte_offset destination in
  let count = String.length bytes in
  if
    Scalar.byte_size slot.scalar <> 1
    || slot.dimensions = [] || cell < 0 || byte <> cell || count <= 0
    || cell > slot.element_count
    || count > slot.element_count - cell
    || byte > slot.extent_bytes
    || count > slot.extent_bytes - byte
  then
    Error "native static copy leaves its checked accessible byte-array extent"
  else
    Ok
      {
        copy_layout = layout;
        copy_state = state;
        copy_request = request;
        copy_data = slot.data_offset + byte;
        copy_flag = slot.flag_offset - (cell * 8);
        copy_bytes = bytes;
      }

let check_static_copy copy ~layout ~request =
  if
    copy.copy_layout != layout
    || copy.copy_request != request
    || Atomic.get layout.task_state != copy.copy_state
  then Error "native static copy belongs to another request or arena layout"
  else Driver.Integer_task.Native_static_copy.check request

let static_copy_payload copy =
  ( copy.copy_state.task_arena_bytes,
    copy.copy_data,
    copy.copy_flag,
    Bytes.to_string (Bytes.of_string copy.copy_bytes) )

let append_task_literals snapshot ~sources ~work =
  let ( let* ) = Result.bind in
  let layout = snapshot.task_layout in
  let before = snapshot.task_state_snapshot in
  let* () =
    if Atomic.get layout.task_state != before then
      error "HCBACK0003"
        "native task literal snapshot precedes current layout admission"
    else if
      work < 0 || work > layout.max_task_layout_work - before.task_layout_work
    then
      error "HCBACK0001"
        "native task literal admission exceeds cumulative layout work"
    else Ok ()
  in
  let* literals =
    Literals.append before.task_literals
      ~max_literal_bytes:layout.max_task_literal_bytes
      ~max_arena_bytes:hard_max_arena_bytes
      ~arena_prefix_bytes:before.task_arena_bytes ~sources
    |> Result.map_error
         (List.map (fun (error : Literals.error) ->
              { code = error.code; message = error.message; span = error.span }))
  in
  let after =
    {
      before with
      task_literals = literals;
      task_arena_bytes = Literals.arena_bytes literals;
      task_layout_work = before.task_layout_work + work;
    }
  in
  let task_storage =
    { snapshot.task_storage with zero_bytes = Some after.task_arena_bytes }
  in
  if Atomic.compare_and_set layout.task_state before after then
    Ok { snapshot with task_storage; task_state_snapshot = after }
  else error "HCBACK0003" "native task storage changed during literal admission"

let task_snapshot_matches_layout snapshot layout =
  snapshot.task_layout == layout

let task_layout_work layout = (Atomic.get layout.task_state).task_layout_work

let arena_bytes layout =
  match layout.zero_bytes with
  | Some count -> count
  | None -> String.length layout.image

let task_snapshot_arena_image snapshot =
  String.make (Option.get snapshot.task_storage.zero_bytes) '\000'

let task_snapshot_arena_bytes snapshot = arena_bytes snapshot.task_storage
let task_snapshot_global_bytes snapshot = snapshot.task_storage.global_bytes
let task_snapshot_literals snapshot = snapshot.task_state_snapshot.task_literals

let task_snapshot_literal_bytes snapshot =
  Literals.literal_bytes (task_snapshot_literals snapshot)

let task_snapshot_initializations_since snapshot ~arena_prefix_bytes =
  Literals.initializations_since
    (task_snapshot_literals snapshot)
    ~arena_prefix_bytes

let task_snapshot_storage snapshot = snapshot.task_storage

let task_snapshot_matches snapshot ~initialization ~entry =
  let storage = snapshot.task_storage in
  storage.entry == entry
  && storage.globals == Initialization.globals initialization
  && Initialization.matches initialization ~globals:storage.globals ~entry

let globals layout = layout.globals
let entry layout = layout.entry
let global_bytes layout = layout.global_bytes

let image layout =
  match layout.zero_bytes with
  | Some count -> String.make count '\000'
  | None -> Bytes.to_string (Bytes.of_string layout.image)

let is_empty layout = Symbol_map.is_empty layout.slots

let find_symbol layout symbol =
  match Symbol_map.find_opt (Symbol.id symbol) layout.slots with
  | Some slot when slot.symbol == symbol ->
      if Option.is_none layout.zero_bytes then Some slot
      else
        Option.bind (Globals.find_storage layout.globals symbol)
          (fun original ->
            if Globals.same_storage original slot.source_slot then Some slot
            else None)
  | Some _ | None -> None

let find_symbol_from_source layout ~source_globals symbol =
  if not (Globals.same_task_storage layout.globals source_globals) then None
  else
    match Symbol_map.find_opt (Symbol.id symbol) layout.slots with
    | Some slot when slot.symbol == symbol ->
        Option.bind (Globals.find_storage source_globals symbol)
          (fun original ->
            if Globals.same_storage original slot.source_slot then Some slot
            else None)
    | Some _ | None -> None

let find_retained layout reference =
  match Globals.retained_slot layout.globals reference with
  | None -> None
  | Some source ->
      Option.bind
        (Symbol_map.find_opt
           (Symbol.id (Ir.Retained_global.symbol reference))
           layout.retained_slots)
        (fun (candidate, slot) ->
          if
            Ir.Retained_global.same candidate reference
            && Globals.same_storage source slot.source_slot
          then Some slot
          else None)

let find_retained_from_source layout ~source_globals reference =
  if not (Globals.same_task_storage layout.globals source_globals) then None
  else
    match Globals.retained_slot source_globals reference with
    | None -> None
    | Some source ->
        Option.bind
          (Symbol_map.find_opt
             (Symbol.id (Ir.Retained_global.symbol reference))
             layout.retained_slots)
          (fun (candidate, slot) ->
            if
              Ir.Retained_global.same candidate reference
              && slot.symbol == Ir.Retained_global.symbol reference
              && Globals.same_storage source slot.source_slot
            then Some slot
            else None)

let source_slot slot = slot.source_slot
let symbol slot = slot.symbol
let type_ slot = slot.type_
let callback slot = slot.callback
let code_owner_offset slot = slot.code_owner_offset
let scalar slot = slot.scalar
let dimensions slot = slot.dimensions
let strides slot = slot.strides
let element_count slot = slot.element_count
let extent_bytes slot = slot.extent_bytes
let data_offset slot = slot.data_offset
let flag_offset slot = slot.flag_offset
let initially_initialized slot = slot.initially_initialized

let owns_address ?source_globals slot runtime_owner =
  match (slot.owner, slot.static_source, runtime_owner) with
  | _, Some allocation, Ir.Runtime_call_context.Entry ->
      Option.fold ~none:false
        ~some:(fun globals ->
          Option.fold ~none:false
            ~some:(fun fragment ->
              let original = Ir.Integer_static_allocation.source allocation in
              let receipt =
                Sema.Compiler_record.static_allocation_receipt original
              in
              let destination =
                (Sema.Static_initializer_fragment.receipt fragment)
                  .static_allocation
              in
              destination.allocation_function == receipt.allocation_function
              && (destination == receipt
                 || List.exists
                      (fun (_, selection) ->
                        match Sema.Reference_selection.kind selection with
                        | Sema.Reference_selection.Static_local reference ->
                            Sema.Static_reference.allocation reference
                            == original
                        | _ -> false)
                      (Sema.Static_initializer_fragment.references fragment)))
            (Globals.static_fragment globals))
        source_globals
  | None, Some _, _ -> false
  | None, None, _ -> true
  | Some expected, _, Ir.Runtime_call_context.Function actual ->
      expected == actual
  | Some _, _, _ -> false
