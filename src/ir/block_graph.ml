module Sequence = Instruction_sequence
module Flow = Control_flow
module Block_id = Sequence.Block_id
module Instruction_id = Sequence.Instruction_id

type error = {
  code : string;
  message : string;
  block_id : int option;
  instruction_id : int option;
  span : Common.Span.t option;
}

type block_description = {
  block_id : Block_id.t;
  instructions : Sequence.description list;
}

type block = {
  block_id : Block_id.t;
  instructions : Sequence.t;
  successors : Block_id.t list;
}

module Block_map = Map.Make (struct
  type t = Block_id.t

  let compare = Block_id.compare
end)

module Int_map = Map.Make (Int)
module Int_set = Set.Make (Int)

type t = { entry : block; blocks : block list; index : block Block_map.t }

type checked_block = {
  checked_id : Block_id.t;
  checked_instructions : Sequence.t;
}

let block_number block_id = Block_id.to_int block_id

let instruction_number description =
  Instruction_id.to_int description.Sequence.instruction_id

let error ?block_id ?instruction_id ?span code message =
  {
    code;
    message;
    block_id = Option.map block_number block_id;
    instruction_id = Option.map Instruction_id.to_int instruction_id;
    span;
  }

let child_error block_id (child : Sequence.error) =
  {
    code = child.code;
    message = child.message;
    block_id = Some (block_number block_id);
    instruction_id = child.instruction_id;
    span = child.span;
  }

let validate_children descriptions =
  let errors = ref [] in
  let checked =
    List.filter_map
      (fun (description : block_description) ->
        match Sequence.create description.instructions with
        | Ok checked_instructions ->
            Some { checked_id = description.block_id; checked_instructions }
        | Error child_errors ->
            errors :=
              List.rev_append
                (List.map (child_error description.block_id) child_errors)
                !errors;
            None)
      descriptions
  in
  (checked, List.rev !errors)

let duplicate_block_errors descriptions =
  let seen = ref Int_set.empty in
  List.filter_map
    (fun (description : block_description) ->
      let id = block_number description.block_id in
      if Int_set.mem id !seen then
        Some
          (error ~block_id:description.block_id "HCIR0011"
             (Printf.sprintf "block ^b%d is defined more than once" id))
      else (
        seen := Int_set.add id !seen;
        None))
    descriptions

let duplicate_identity_errors blocks =
  let instruction_owners = ref Int_map.empty in
  let value_owners = ref Int_map.empty in
  let errors = ref [] in
  let add item = errors := item :: !errors in
  List.iter
    (fun block ->
      List.iter
        (fun instruction ->
          let description = Sequence.description instruction in
          let instruction_id = instruction_number description in
          (match Int_map.find_opt instruction_id !instruction_owners with
          | Some owner ->
              add
                (error ~block_id:block.checked_id
                   ~instruction_id:description.instruction_id
                   ?span:description.span "HCIR0012"
                   (Printf.sprintf
                      "instruction !i%d is already defined in block ^b%d"
                      instruction_id owner))
          | None ->
              instruction_owners :=
                Int_map.add instruction_id
                  (block_number block.checked_id)
                  !instruction_owners);
          match description.result with
          | None -> ()
          | Some result -> (
              let value_id = Sequence.Value_id.to_int result.value_id in
              match Int_map.find_opt value_id !value_owners with
              | Some owner ->
                  add
                    (error ~block_id:block.checked_id
                       ~instruction_id:description.instruction_id
                       ?span:description.span "HCIR0013"
                       (Printf.sprintf
                          "value %%v%d is already defined in block ^b%d"
                          value_id owner))
              | None ->
                  value_owners :=
                    Int_map.add value_id
                      (block_number block.checked_id)
                      !value_owners))
        (Sequence.instructions block.checked_instructions))
    blocks;
  List.rev !errors

let description_targets description =
  match
    (Flow.target_shape description.Sequence.opcode, description.payload)
  with
  | Flow.No_target, _ -> Ok []
  | Flow.Single_target, Some (Sequence.Block target) -> Ok [ target ]
  | Flow.Switch_targets, Some (Sequence.Block_targets targets) -> Ok targets
  | Flow.Single_target, _ ->
      Error
        (Printf.sprintf "%s requires one block target"
           (Opcode.to_source_name description.opcode))
  | Flow.Switch_targets, _ ->
      Error
        (Printf.sprintf "%s requires an ordered block-target list"
           (Opcode.to_source_name description.opcode))

let ordered_unique blocks =
  let _, reversed =
    List.fold_left
      (fun (seen, result) block ->
        let id = block_number block in
        if Int_set.mem id seen then (seen, result)
        else (Int_set.add id seen, block :: result))
      (Int_set.empty, []) blocks
  in
  List.rev reversed

let block_index blocks =
  List.fold_left
    (fun index block -> Block_map.add block.checked_id block index)
    Block_map.empty blocks

let control_flow_errors blocks index =
  let errors = ref [] in
  let end_seen = ref false in
  let add item = errors := item :: !errors in
  let validate_target block description target =
    if not (Block_map.mem target index) then
      add
        (error ~block_id:block.checked_id
           ~instruction_id:description.Sequence.instruction_id
           ?span:description.span "HCIR0017"
           (Printf.sprintf "%s targets undefined block ^b%d"
              (Opcode.to_source_name description.opcode)
              (block_number target)))
  in
  List.iter
    (fun block ->
      let terminated = ref None in
      let block_after_end = !end_seen in
      if block_after_end && Sequence.length block.checked_instructions = 0 then
        add
          (error ~block_id:block.checked_id "HCIR0020"
             (Printf.sprintf "block ^b%d appears after IC_END"
                (block_number block.checked_id)));
      List.iter
        (fun instruction ->
          let description = Sequence.description instruction in
          if block_after_end then
            add
              (error ~block_id:block.checked_id
                 ~instruction_id:description.instruction_id
                 ?span:description.span "HCIR0020"
                 "instruction appears after IC_END");
          (match !terminated with
          | Some terminator ->
              add
                (error ~block_id:block.checked_id
                   ~instruction_id:description.instruction_id
                   ?span:description.span "HCIR0015"
                   (Printf.sprintf
                      "instruction follows terminator !i%d in block ^b%d"
                      terminator
                      (block_number block.checked_id)))
          | None -> ());
          (match description_targets description with
          | Ok targets -> List.iter (validate_target block description) targets
          | Error message ->
              add
                (error ~block_id:block.checked_id
                   ~instruction_id:description.instruction_id
                   ?span:description.span "HCIR0016" message));
          if description.opcode = Opcode.Ic_end then
            if !end_seen then
              add
                (error ~block_id:block.checked_id
                   ~instruction_id:description.instruction_id
                   ?span:description.span "HCIR0019"
                   "IC_END appears more than once in the function")
            else end_seen := true;
          if Flow.ends_block description.opcode && Option.is_none !terminated
          then terminated := Some (instruction_number description))
        (Sequence.instructions block.checked_instructions))
    blocks;
  List.rev !errors

let final_description block =
  match List.rev (Sequence.instructions block.checked_instructions) with
  | [] -> None
  | instruction :: _ -> Some (Sequence.description instruction)

let derive_successors blocks =
  let errors = ref [] in
  let add item = errors := item :: !errors in
  let rec build result = function
    | [] -> List.rev result
    | [ block ] ->
        let final = final_description block in
        let successors =
          match final with
          | Some description when not (Flow.may_fall_through description.opcode)
            -> (
              match description_targets description with
              | Ok targets -> ordered_unique targets
              | Error _ -> [])
          | Some description ->
              add
                (error ~block_id:block.checked_id
                   ~instruction_id:description.instruction_id
                   ?span:description.span "HCIR0018"
                   (Printf.sprintf
                      "final block ^b%d falls through without IC_END or IC_RET"
                      (block_number block.checked_id)));
              []
          | None ->
              add
                (error ~block_id:block.checked_id "HCIR0018"
                   (Printf.sprintf
                      "empty final block ^b%d falls through without IC_END or \
                       IC_RET"
                      (block_number block.checked_id)));
              []
        in
        List.rev
          ({
             block_id = block.checked_id;
             instructions = block.checked_instructions;
             successors;
           }
          :: result)
    | block :: (next :: _ as remaining) ->
        let final = final_description block in
        let explicit =
          match final with
          | None -> []
          | Some description -> (
              match description_targets description with
              | Ok targets -> targets
              | Error _ -> [])
        in
        let successors =
          match final with
          | None -> [ next.checked_id ]
          | Some description when Flow.may_fall_through description.opcode ->
              ordered_unique (explicit @ [ next.checked_id ])
          | Some _ -> ordered_unique explicit
        in
        build
          ({
             block_id = block.checked_id;
             instructions = block.checked_instructions;
             successors;
           }
          :: result)
          remaining
  in
  let result = build [] blocks in
  (result, List.rev !errors)

let create ~entry descriptions =
  let block_errors = duplicate_block_errors descriptions in
  let checked, child_errors = validate_children descriptions in
  let identity_errors = duplicate_identity_errors checked in
  let index = block_index checked in
  let entry_errors =
    if Block_map.mem entry index then []
    else
      [
        error ~block_id:entry "HCIR0014"
          (Printf.sprintf "entry block ^b%d is not defined" (block_number entry));
      ]
  in
  let flow_errors = control_flow_errors checked index in
  let built, final_errors = derive_successors checked in
  let errors =
    block_errors @ child_errors @ identity_errors @ entry_errors @ flow_errors
    @ final_errors
  in
  match errors with
  | _ :: _ -> Error errors
  | [] ->
      let built_index =
        List.fold_left
          (fun result block -> Block_map.add block.block_id block result)
          Block_map.empty built
      in
      let entry_block = Block_map.find entry built_index in
      Ok { entry = entry_block; blocks = built; index = built_index }

let entry graph = graph.entry
let blocks graph = graph.blocks
let find_block graph id = Block_map.find_opt id graph.index
let block_id block = block.block_id
let instructions block = block.instructions
let successors block = block.successors

let human graph =
  let buffer = Buffer.create 512 in
  Printf.bprintf buffer "holyc-ir-graph-v1 reference=%s\n"
    Sequence.reference_commit;
  Printf.bprintf buffer "entry=^b%d\n" (block_number graph.entry.block_id);
  List.iter
    (fun block ->
      Printf.bprintf buffer "block ^b%d\n" (block_number block.block_id);
      Buffer.add_string buffer (Sequence.human_body block.instructions);
      Buffer.add_string buffer "successors=[";
      List.iteri
        (fun index successor ->
          if index > 0 then Buffer.add_char buffer ',';
          Printf.bprintf buffer "^b%d" (block_number successor))
        block.successors;
      Buffer.add_string buffer "]\n")
    graph.blocks;
  Buffer.contents buffer
