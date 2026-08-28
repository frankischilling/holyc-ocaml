module Sequence = Instruction_sequence
module Labels = Sema.Label_resolution
module Symbol = Sema.Symbol

type label_block = Symbol.t * Sequence.Block_id.t

type t = {
  sequence_ : Sequence.t;
  label_blocks_ : label_block list;
  next_instruction_id_ : Sequence.Instruction_id.t;
  next_block_id_ : Sequence.Block_id.t;
}

let reference_commit = Opcode.reference_commit

let lowering_error ?span code message =
  { Sequence.code; message; instruction_id = None; span }

let metadata_error message = lowering_error "HCIRL0004" message
let unsupported_code = "HCIRL0006"

let unsupported_error ?span message =
  lowering_error ?span unsupported_code message

let span_of_origin description = function
  | Symbol.Source_location location -> Ok location.span
  | Symbol.Pinned_source _ | Symbol.Synthesized _ ->
      let message = description ^ " has no complete source location" in
      Error (metadata_error message)

let optional_span = function
  | Symbol.Source_location location -> Some location.span
  | Symbol.Pinned_source _ | Symbol.Synthesized _ -> None

let next_instruction_id ~span instruction_id =
  let current = Sequence.Instruction_id.to_int instruction_id in
  if current = Int.max_int then
    let message = "cannot allocate the next goto or label instruction" in
    Error (lowering_error ~span "HCIRL0005" message)
  else Sequence.Instruction_id.of_int (current + 1)

let next_block_id ?span block_id =
  let current = Sequence.Block_id.to_int block_id in
  if current = Int.max_int then
    let message = "cannot allocate the next function label block" in
    Error (lowering_error ?span "HCIRL0005" message)
  else Sequence.Block_id.of_int (current + 1)

let label_error label message =
  let symbol = Labels.label_symbol label in
  let span = optional_span (Symbol.origin symbol) in
  unsupported_error ?span message

let unsupported_assembly_message name =
  Printf.sprintf "label %S is an unsupported assembly form" name

let validate_label label =
  let name = label |> Labels.label_symbol |> Symbol.name in
  let kind = Labels.label_definition_kind label in
  let count = Labels.label_definition_count label in
  if kind <> Labels.Language_label then
    let message = unsupported_assembly_message name in
    Error (label_error label message)
  else if count <> 1 then
    let message = Printf.sprintf "label %S has %d definitions" name count in
    Error (label_error label message)
  else Ok ()

let allocate_label_block current label reversed =
  match validate_label label with
  | Error _ as error -> error
  | Ok () -> (
      let symbol = Labels.label_symbol label in
      let span = optional_span (Symbol.origin symbol) in
      match next_block_id ?span current with
      | Error _ as error -> error
      | Ok next -> Ok (next, (symbol, current) :: reversed))

let allocate_label_blocks start labels =
  let rec loop current reversed = function
    | [] -> Ok (List.rev reversed, current)
    | label :: rest -> (
        match allocate_label_block current label reversed with
        | Error _ as error -> error
        | Ok (next, reversed) -> loop next reversed rest)
  in
  loop start [] labels

let symbol_number symbol = Symbol.Id.to_int (Symbol.id symbol)

let block_index bindings =
  let index = Hashtbl.create (List.length bindings) in
  List.iter
    (fun (symbol, block) -> Hashtbl.replace index (symbol_number symbol) block)
    bindings;
  index

let block_for_symbol index sought =
  match Hashtbl.find_opt index (symbol_number sought) with
  | Some block -> Ok block
  | None ->
      let name = Symbol.name sought in
      let message = Printf.sprintf "label %S has no assigned block" name in
      Error (unsupported_error message)

let occurrence_opcode occurrence =
  match Labels.occurrence_kind occurrence with
  | Labels.Definition Labels.Language_label -> Ok Opcode.Ic_label
  | Labels.Goto_reference -> Ok Opcode.Ic_jmp
  | Labels.Definition _ ->
      let symbol = Labels.occurrence_symbol occurrence in
      let name = Symbol.name symbol in
      let message = unsupported_assembly_message name in
      let span = optional_span (Labels.occurrence_origin occurrence) in
      Error (unsupported_error ?span message)

let lower_occurrence index instruction_id occurrence =
  let origin = Labels.occurrence_origin occurrence in
  match span_of_origin "goto or label occurrence" origin with
  | Error _ as error -> error
  | Ok span -> (
      match occurrence_opcode occurrence with
      | Error _ as error -> error
      | Ok opcode -> (
          let symbol = Labels.occurrence_symbol occurrence in
          match block_for_symbol index symbol with
          | Error _ as error -> error
          | Ok block -> (
              match next_instruction_id ~span instruction_id with
              | Error _ as error -> error
              | Ok next ->
                  let description : Sequence.description =
                    {
                      instruction_id;
                      opcode;
                      operands = [];
                      result = None;
                      target_type = None;
                      payload = Some (Sequence.Block block);
                      flags = 0L;
                      span = Some span;
                    }
                  in
                  Ok (description, next))))

let lower_occurrences index start occurrences =
  let rec loop current reversed = function
    | [] -> Ok (List.rev reversed, current)
    | occurrence :: rest -> (
        match lower_occurrence index current occurrence with
        | Error _ as error -> error
        | Ok (description, next) ->
            let reversed = description :: reversed in
            loop next reversed rest)
  in
  loop start [] occurrences

let make_lowered sequence_ label_blocks_ next_instruction_id_ next_block_id_ =
  { sequence_; label_blocks_; next_instruction_id_; next_block_id_ }

let finish_lowering label_blocks_ next_block_id_ instruction_id occurrences =
  let index = block_index label_blocks_ in
  match lower_occurrences index instruction_id occurrences with
  | Error item -> Error [ item ]
  | Ok (items, next_instruction_id_) -> (
      match Sequence.create items with
      | Error errors -> Error errors
      | Ok sequence_ ->
          let lowered =
            make_lowered sequence_ label_blocks_ next_instruction_id_
              next_block_id_
          in
          Ok lowered)

let lower_function_labels ~instruction_id ~block_id function_ =
  let labels = Labels.function_labels function_ in
  match allocate_label_blocks block_id labels with
  | Error item -> Error [ item ]
  | Ok (label_blocks_, next_block_id_) ->
      let occurrences = Labels.function_occurrences function_ in
      finish_lowering label_blocks_ next_block_id_ instruction_id occurrences

let sequence lowered = lowered.sequence_
let label_blocks lowered = lowered.label_blocks_
let next_instruction_id lowered = lowered.next_instruction_id_
let next_block_id lowered = lowered.next_block_id_

let label_block_name (symbol, block) =
  Printf.sprintf "@s%d:%S=^b%d" (symbol_number symbol) (Symbol.name symbol)
    (Sequence.Block_id.to_int block)

let human lowered =
  let bindings =
    lowered.label_blocks_ |> List.map label_block_name |> String.concat ","
  in
  Printf.sprintf
    "holyc-ir-goto-label-v1 reference=%s\n\
     labels=[%s] next-instruction=%d next-block=%d\n\
     %s"
    reference_commit bindings
    (Sequence.Instruction_id.to_int lowered.next_instruction_id_)
    (Sequence.Block_id.to_int lowered.next_block_id_)
    (Sequence.human_body lowered.sequence_)
