module Sequence = Instruction_sequence
module Breaks = Sema.Break_resolution
module Symbol = Sema.Symbol
module Region_id = Breaks.Region_id
module Block_id = Sequence.Block_id

type region_block = Region_id.t * Block_id.t

type t = {
  sequence_ : Sequence.t;
  region_blocks_ : region_block list;
  next_instruction_id_ : Sequence.Instruction_id.t;
  next_block_id_ : Sequence.Block_id.t;
}

let reference_commit = Opcode.reference_commit

let lowering_error ?span code message =
  { Sequence.code; message; instruction_id = None; span }

let metadata_error message = lowering_error "HCIRL0004" message
let unsupported_error message = lowering_error "HCIRL0006" message

let span_of_origin description = function
  | Symbol.Source_location location -> Ok location.span
  | Symbol.Pinned_source _ | Symbol.Synthesized _ ->
      let suffix = " has no complete source location" in
      Error (metadata_error (description ^ suffix))

let next_instruction_id ~span instruction_id =
  let current = Sequence.Instruction_id.to_int instruction_id in
  if current = Int.max_int then
    let message = "cannot allocate the next break instruction" in
    Error (lowering_error ~span "HCIRL0005" message)
  else Sequence.Instruction_id.of_int (current + 1)

let next_block_id ~span block_id =
  let current = Sequence.Block_id.to_int block_id in
  if current = Int.max_int then
    let message = "cannot allocate the next break region block" in
    Error (lowering_error ~span "HCIRL0005" message)
  else Sequence.Block_id.of_int (current + 1)

let allocate_region current reversed region =
  let origin = Breaks.region_origin region in
  match span_of_origin "break region" origin with
  | Error _ as error -> error
  | Ok span -> (
      match next_block_id ~span current with
      | Error _ as error -> error
      | Ok next ->
          let region_id = Breaks.region_id region in
          Ok (next, (region_id, current) :: reversed))

let allocate_regions start regions =
  let rec loop current reversed = function
    | [] -> Ok (List.rev reversed, current)
    | region :: rest -> (
        match allocate_region current reversed region with
        | Error _ as error -> error
        | Ok (next, reversed) -> loop next reversed rest)
  in
  loop start [] regions

let region_number region_id = Breaks.Region_id.to_int region_id

let block_index bindings =
  let index = Hashtbl.create (List.length bindings) in
  List.iter
    (fun (region_id, block) ->
      Hashtbl.replace index (region_number region_id) block)
    bindings;
  index

let block_for_target index target =
  match Hashtbl.find_opt index (region_number target) with
  | Some block -> Ok block
  | None ->
      let target = region_number target in
      let name = string_of_int target in
      let message = "break region " ^ name ^ " has no assigned block" in
      Error (unsupported_error message)

let lower_break index instruction_id occurrence =
  let origin = Breaks.break_origin occurrence in
  match span_of_origin "break occurrence" origin with
  | Error _ as error -> error
  | Ok span -> (
      let target = Breaks.break_target occurrence in
      match block_for_target index target with
      | Error _ as error -> error
      | Ok block -> (
          match next_instruction_id ~span instruction_id with
          | Error _ as error -> error
          | Ok next ->
              let description : Sequence.description =
                {
                  instruction_id;
                  opcode = Opcode.Ic_jmp;
                  operands = [];
                  result = None;
                  target_type = None;
                  payload = Some (Sequence.Block block);
                  flags = 0L;
                  span = Some span;
                }
              in
              Ok (description, next)))

let lower_breaks index start occurrences =
  let rec loop current reversed = function
    | [] -> Ok (List.rev reversed, current)
    | occurrence :: rest -> (
        match lower_break index current occurrence with
        | Error _ as error -> error
        | Ok (description, next) ->
            let reversed = description :: reversed in
            loop next reversed rest)
  in
  loop start [] occurrences

let make_lowered sequence_ region_blocks_ instruction block =
  let next_instruction_id_ = instruction in
  let next_block_id_ = block in
  { sequence_; region_blocks_; next_instruction_id_; next_block_id_ }

let finish_lowering regions next_block instruction_id occurrences =
  let index = block_index regions in
  match lower_breaks index instruction_id occurrences with
  | Error item -> Error [ item ]
  | Ok (items, next_instruction_id_) -> (
      match Sequence.create items with
      | Error errors -> Error errors
      | Ok sequence_ ->
          let make = make_lowered sequence_ regions in
          let lowered = make next_instruction_id_ next_block in
          Ok lowered)

let lower_function_breaks ~instruction_id ~block_id function_ =
  let regions = Breaks.function_regions function_ in
  match allocate_regions block_id regions with
  | Error item -> Error [ item ]
  | Ok (region_blocks_, next_block_id_) ->
      let occurrences = Breaks.function_breaks function_ in
      let finish = finish_lowering region_blocks_ next_block_id_ in
      finish instruction_id occurrences

let sequence lowered = lowered.sequence_
let region_blocks lowered = lowered.region_blocks_
let next_instruction_id lowered = lowered.next_instruction_id_
let next_block_id lowered = lowered.next_block_id_

let region_block_name (region_id, block) =
  Printf.sprintf "region=%d:^b%d" (region_number region_id)
    (Sequence.Block_id.to_int block)

let human lowered =
  let names = List.map region_block_name lowered.region_blocks_ in
  let bindings = String.concat "," names in
  Printf.sprintf
    "holyc-ir-break-v1 reference=%s\n\
     regions=[%s] next-instruction=%d next-block=%d\n\
     %s"
    reference_commit bindings
    (Sequence.Instruction_id.to_int lowered.next_instruction_id_)
    (Sequence.Block_id.to_int lowered.next_block_id_)
    (Sequence.human_body lowered.sequence_)
