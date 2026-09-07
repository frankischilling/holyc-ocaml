module Sequence = Instruction_sequence
module Typed = Sema.Function_call_expression_result
module Symbol = Sema.Symbol
module Type = Sema.Type
module Values = Set.Make (Sequence.Value_id)

type phase = Compile_initializer | Load_initializer

type region_description = {
  root : Typed.top_level_root_result;
  first : Sequence.Instruction_id.t;
  last : Sequence.Instruction_id.t;
}

type region = {
  description : region_description;
  slot : Integer_globals.slot;
  phase_ : phase;
}

type static_region_description = {
  static_root : Typed.initializer_result;
  static_slot : Integer_globals.static_slot;
  first : Sequence.Instruction_id.t;
  last : Sequence.Instruction_id.t;
}

type static_region = {
  static_description : static_region_description;
  static_phase_ : phase;
}

type storage_region = Global_region of region | Static_region of static_region

type pending =
  | Global_pending of Integer_globals.slot
  | Static_pending of Integer_globals.static_slot

type description =
  | Global_description of region_description
  | Static_description of static_region_description

type t = {
  globals : Integer_globals.t;
  entry : X87_stack.t;
  regions_ : storage_region list;
  region_index : storage_region array;
  prepared_steps_ : int;
}

let root region = region.description.root
let describe region = region.description
let symbol region = Integer_globals.slot_symbol region.slot
let phase region = region.phase_

let phase_name = function
  | Compile_initializer -> "compile-initializer"
  | Load_initializer -> "load-initializer"

let static_root region = region.static_description.static_root
let static_slot region = region.static_description.static_slot
let describe_static region = region.static_description
let static_phase region = region.static_phase_
let storage_regions context = context.regions_

let regions context =
  List.filter_map
    (function
      | Global_region region -> Some region
      | _ -> None)
    context.regions_

let static_regions context =
  List.filter_map
    (function
      | Static_region region -> Some region
      | _ -> None)
    context.regions_

let storage_symbol = function
  | Global_region region -> symbol region
  | Static_region region ->
      Integer_globals.static_location (static_slot region)
      |> Sema.Function_frame_layout.location_symbol

let storage_phase = function
  | Global_region region -> phase region
  | Static_region region -> static_phase region

let storage_frame = function
  | Global_region _ -> None
  | Static_region region ->
      Some (Integer_globals.static_frame (static_slot region))

let storage_first = function
  | Global_region region -> region.description.first
  | Static_region region -> region.static_description.first

let storage_last = function
  | Global_region region -> region.description.last
  | Static_region region -> region.static_description.last

let prepared_steps context = context.prepared_steps_

let matches context ~globals ~entry =
  context.globals == globals && context.entry == entry

let find_storage context instruction =
  let rec search lower upper =
    if lower >= upper then None
    else
      let middle = lower + ((upper - lower) / 2) in
      let region = context.region_index.(middle) in
      if Sequence.Instruction_id.compare instruction (storage_first region) < 0
      then search lower middle
      else if
        Sequence.Instruction_id.compare instruction (storage_last region) > 0
      then search (middle + 1) upper
      else Some region
  in
  search 0 (Array.length context.region_index)

let find context instruction =
  match find_storage context instruction with
  | Some (Global_region region) -> Some region
  | _ -> None

let create ?(static_descriptions = []) ~span:context_span ~globals ~entry
    descriptions =
  let ( let* ) = Result.bind in
  let invalid ?span message =
    Error
      [
        Common.Diagnostic.make ~code:"HCIRVM0017"
          ~severity:Common.Diagnostic.Error ~message
          ~primary:(Option.value span ~default:context_span)
          ();
      ]
  in
  let blocks = X87_stack.graph entry |> Block_graph.blocks in
  let* prepared_steps =
    List.fold_left
      (fun total slot ->
        let* total = total in
        let steps = Integer_globals.storage_preparation_steps slot in
        if steps < 0 || steps > Int.max_int - total then
          invalid "initializer preparation step count overflows"
        else Ok (total + steps))
      (Ok 0)
      (Integer_globals.storage_slots globals)
  in
  let instructions block =
    Block_graph.instructions block
    |> Sequence.instructions
    |> List.map Sequence.description
  in
  let rec monotonic previous = function
    | [] -> true
    | (item : Sequence.description) :: rest ->
        Option.fold ~none:true
          ~some:(fun previous ->
            Sequence.Instruction_id.compare previous item.instruction_id < 0)
          previous
        && monotonic (Some item.instruction_id) rest
  in
  let* () =
    if monotonic None (List.concat_map instructions blocks) then Ok ()
    else
      invalid
        "initializer entry instruction IDs must increase in physical source \
         order"
  in
  let closed sequence =
    let rec loop defined depth = function
      | [] -> depth = 0
      | (item : Sequence.description) :: rest ->
          if
            not
              (List.for_all
                 (fun operand -> Values.mem operand defined)
                 item.operands)
          then false
          else
            let depth =
              match item.opcode with
              | Opcode.Ic_call_start -> depth + 1
              | Ic_call_end -> depth - 1
              | _ -> depth
            in
            let scoped =
              (* ICF_PUSH_RES applies after CALL_END closes its own scope. *)
              (Int64.logand item.flags 0x2000L = 0L || depth > 0)
              &&
              match item.opcode with
              | Opcode.Ic_call | Ic_add_rsp | Ic_add_rsp1 -> depth > 0
              | _ -> true
            in
            let defined =
              match item.result with
              | Some value -> Values.add value.value_id defined
              | None -> defined
            in
            depth >= 0 && scoped && loop defined depth rest
    in
    loop Values.empty 0 sequence
  in
  let pending =
    Integer_globals.slots globals
    |> List.filter (fun slot ->
        Option.is_some (Integer_globals.slot_initializer slot)
        && not (Integer_globals.slot_initializer_materialized slot))
    |> List.map (fun slot -> Global_pending slot)
  in
  let pending =
    pending
    @ (Integer_globals.statics globals
      |> List.filter (fun slot ->
          Option.is_some (Integer_globals.static_initializer slot)
          && Integer_globals.storage_preparation_steps
               (Integer_globals.static_storage slot)
             = 0)
      |> List.map (fun slot -> Static_pending slot))
    |> List.stable_sort (fun left right ->
        let index = function
          | Global_pending slot ->
              Integer_globals.slot_record slot
              |> Sema.Global_record_classification.classified_record_source
              |> Sema.Global_resolution.global_record_global
              |> Sema.Global_type_resolution.global_item_index
          | Static_pending slot ->
              Integer_globals.static_frame slot
              |> Sema.Function_frame_layout.function_item_index
        in
        Int.compare (index left) (index right))
  in
  let first = function
    | Global_description d -> d.first
    | Static_description d -> d.first
  in
  let ordered descriptions =
    let rec loop = function
      | a :: (b :: _ as rest) ->
          Sequence.Instruction_id.compare (first a) (first b) < 0 && loop rest
      | _ -> true
    in
    loop descriptions
  in
  let global_descriptions =
    List.map (fun d -> Global_description d) descriptions
  in
  let static_descriptions =
    List.map (fun d -> Static_description d) static_descriptions
  in
  let* () =
    if ordered global_descriptions && ordered static_descriptions then Ok ()
    else invalid "initializer descriptions do not retain declaration order"
  in
  let descriptions =
    List.sort
      (fun a b -> Sequence.Instruction_id.compare (first a) (first b))
      (global_descriptions @ static_descriptions)
  in
  let rec check previous reversed slots descriptions =
    match (slots, descriptions) with
    | [], [] -> Ok (List.rev reversed)
    | slot :: slots, description :: descriptions ->
        let* storage, first, last, value, make_region =
          match (slot, description) with
          | Global_pending slot, Global_description description
            when Option.fold ~none:false
                   ~some:(fun root -> root == description.root)
                   (Integer_globals.slot_initializer slot) ->
              Ok
                ( Integer_globals.global_storage slot,
                  description.first,
                  description.last,
                  Typed.top_level_root_value description.root,
                  fun phase_ -> Global_region { description; slot; phase_ } )
          | Static_pending slot, Static_description description
            when slot == description.static_slot
                 && Option.fold ~none:false
                      ~some:(fun root -> root == description.static_root)
                      (Integer_globals.static_initializer slot) ->
              let storage = Integer_globals.static_storage slot in
              if
                Integer_globals.storage_opcode storage = Opcode.Ic_abs_addr
                && Sema.Compiler_option.is_enabled
                     ~mask:(Integer_globals.static_compiler_options slot)
                     Sema.Compiler_option.Globals_on_data_heap
              then
                invalid
                  "nonconstant AOT static initialization with \
                   globals-on-data-heap requires a separate compile-time phase"
              else
                Ok
                  ( storage,
                    description.first,
                    description.last,
                    Typed.initializer_value description.static_root,
                    fun static_phase_ ->
                      Static_region
                        { static_description = description; static_phase_ } )
          | _ ->
              invalid
                "initializer regions do not match their exact declaration order"
        in
        let span =
          match Typed.result_origin value with
          | Symbol.Source_location location -> Some location.span
          | _ -> None
        in
        if
          Sequence.Instruction_id.compare first last >= 0
          || Option.fold ~none:false
               ~some:(fun last ->
                 Sequence.Instruction_id.compare first last <= 0)
               previous
        then
          invalid ?span
            "initializer regions overlap or have invalid instruction bounds"
        else
          let selected =
            List.filter_map
              (fun block ->
                let sequence = instructions block in
                if
                  List.exists
                    (fun (item : Sequence.description) ->
                      Sequence.Instruction_id.equal item.instruction_id first)
                    sequence
                then
                  Some
                    (List.filter
                       (fun (item : Sequence.description) ->
                         Sequence.Instruction_id.compare item.instruction_id
                           first
                         >= 0
                         && Sequence.Instruction_id.compare item.instruction_id
                              last
                            <= 0)
                       sequence)
                else None)
              blocks
          in
          let* () =
            match selected with
            | [ address :: rest ] -> (
                match List.rev rest with
                | ending :: store :: reversed_value when reversed_value <> [] ->
                    let expected_type = Integer_globals.storage_type storage in
                    let same_type expected = function
                      | Some actual -> Type.equal expected actual
                      | None -> false
                    in
                    let address_type =
                      match Type.pointer_to expected_type with
                      | Ok value -> Some value
                      | Error _ -> None
                    in
                    let address_valid =
                      address.opcode = Integer_globals.storage_opcode storage
                      && address.operands = [] && address.flags = 0L
                      && (match address.payload with
                        | Some (Sequence.Symbol actual) ->
                            actual == Integer_globals.storage_symbol storage
                        | _ -> false)
                      && Option.fold ~none:false
                           ~some:(fun type_ ->
                             same_type type_ address.target_type)
                           address_type
                    in
                    let store_valid =
                      store.opcode = Opcode.Ic_assign
                      && store.flags = 0L && store.payload = None
                      && same_type expected_type store.target_type
                      &&
                      match (address.result, store.operands) with
                      | Some address, [ target; _ ] ->
                          Sequence.Value_id.equal address.value_id target
                      | _ -> false
                    in
                    let end_valid =
                      ending.opcode = Opcode.Ic_end_exp
                      && ending.flags = 0x200L && ending.payload = None
                      && ending.result = None && ending.target_type = None
                      && Sequence.Instruction_id.equal ending.instruction_id
                           last
                      &&
                      match (store.result, ending.operands) with
                      | Some store, [ operand ] ->
                          Sequence.Value_id.equal store.value_id operand
                      | _ -> false
                    in
                    let interior_valid =
                      List.for_all
                        (fun (item : Sequence.description) ->
                          match item.opcode with
                          | Opcode.Ic_end_exp
                          | Ic_end
                          | Ic_ret
                          | Ic_return_val
                          | Ic_jmp
                          | Ic_br_zero
                          | Ic_br_not_zero -> false
                          | _ -> true)
                        reversed_value
                    in
                    if
                      address_valid && store_valid && end_valid
                      && interior_valid
                      && closed (address :: rest)
                    then Ok ()
                    else
                      invalid ?span
                        "initializer region has a noncanonical destination, \
                         store or expression boundary"
                | _ -> invalid ?span "initializer region is incomplete")
            | _ ->
                invalid ?span
                  "initializer region is absent or crosses a block boundary"
          in
          let phase_ =
            match Integer_globals.storage_opcode storage with
            | Opcode.Ic_imm_i64 -> Compile_initializer
            | _ -> Load_initializer
          in
          check (Some last) (make_region phase_ :: reversed) slots descriptions
    | _ ->
        invalid
          "initializer context is missing or duplicates a pending declaration"
  in
  let* regions_ = check None [] pending descriptions in
  Ok
    {
      globals;
      entry;
      regions_;
      region_index = Array.of_list regions_;
      prepared_steps_ = prepared_steps;
    }

let human context =
  match regions context with
  | [] -> ""
  | regions ->
      "holyc-global-initialization-v1\n"
      ^ String.concat ""
          (List.map
             (fun region ->
               Printf.sprintf
                 "initializer symbol=%d:%s phase=%s instructions=%d..%d\n"
                 (Symbol.id (symbol region) |> Symbol.Id.to_int)
                 (Symbol.name (symbol region))
                 (phase_name region.phase_)
                 (Sequence.Instruction_id.to_int region.description.first)
                 (Sequence.Instruction_id.to_int region.description.last))
             regions)

let global_human = human

let human context =
  global_human context
  ^
  match static_regions context with
  | [] -> ""
  | regions ->
      "holyc-static-initialization-v1\n"
      ^ String.concat ""
          (List.map
             (fun region ->
               let symbol =
                 Integer_globals.static_location (static_slot region)
                 |> Sema.Function_frame_layout.location_symbol
               in
               Printf.sprintf
                 "static-initializer function=%s symbol=%d:%s phase=%s \
                  instructions=%d..%d\n"
                 (Integer_globals.static_frame (static_slot region)
                 |> Sema.Function_frame_layout.function_symbol |> Symbol.name)
                 (Symbol.id symbol |> Symbol.Id.to_int)
                 (Symbol.name symbol)
                 (phase_name (static_phase region))
                 (Sequence.Instruction_id.to_int region.static_description.first)
                 (Sequence.Instruction_id.to_int region.static_description.last))
             regions)
