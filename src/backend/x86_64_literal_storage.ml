module Sequence = Ir.Instruction_sequence
module Graph = Ir.Block_graph
module Runtime = Ir.Runtime_call_context
module Function = Ir.Function_body
module Type = Sema.Type
module Instruction_map = Map.Make (Sequence.Instruction_id)

type error = { code : string; message : string; span : Common.Span.t option }
type region = { data_offset : int; table_offset : int; byte_count : int }

type owned_graph = {
  owner : Runtime.owner;
  graph : Graph.t;
  regions : region Instruction_map.t;
}

type t = {
  graphs : owned_graph list;
  literal_bytes : int;
  metadata_bytes : int;
  image : string;
}

let hard_max_literal_bytes = 16_777_216
let error ?span code message = Error [ { code; message; span } ]

let validate_limit ~max_literal_bytes =
  if max_literal_bytes <= 0 || max_literal_bytes > hard_max_literal_bytes then
    error "HCBACK0001"
      (Printf.sprintf "native max_literal_bytes must be between 1 and %d"
         hard_max_literal_bytes)
  else Ok ()

let same_owner left right =
  match (left, right) with
  | Runtime.Entry, Runtime.Entry -> true
  | Runtime.Function left, Runtime.Function right -> left == right
  | _ -> false

let is_literal_type type_ =
  Type.pointer_depth type_ = 1
  &&
  match Type.base type_ with
  | Type.Primitive (Type.Internal_storage, Sema.Primitive_type.U8) -> true
  | _ -> false

let create ~max_literal_bytes ~max_arena_bytes ~arena_prefix_bytes
    ~runtime_calls ~initialization ~entry ~functions =
  let ( let* ) = Result.bind in
  let* () = validate_limit ~max_literal_bytes in
  let* () =
    if
      max_arena_bytes <= 0
      || max_arena_bytes > 33_554_432
      || arena_prefix_bytes < 0
      || arena_prefix_bytes > max_arena_bytes
    then
      error "HCBACK0004"
        "native literal arena prefix exceeds the host byte bound"
    else if
      not
        (Runtime.matches runtime_calls ~entry
           ~initialization:(Some initialization)
           ~functions:
             (List.map
                (fun (definition : Ir.Integer_interpreter.function_definition)
                   -> definition.body)
                functions))
    then error "HCBACK0003" "native literal storage has another callable bundle"
    else Ok ()
  in
  let logical = ref 0 in
  let metadata = ref 0 in
  let used = ref arena_prefix_bytes in
  let chunks = ref [] in
  let collect owner graph =
    let* pushed_arguments =
      List.fold_left
        (fun result block ->
          let* arguments = result in
          List.fold_left
            (fun result instruction ->
              let* arguments = result in
              let description = Sequence.description instruction in
              if description.opcode <> Ir.Opcode.Ic_call_start then Ok arguments
              else
                match
                  Runtime.find_start runtime_calls ~owner
                    description.instruction_id
                with
                | None -> Ok arguments
                | Some call ->
                    List.fold_left
                      (fun result argument ->
                        let* arguments = result in
                        let producer = Runtime.argument_producer argument in
                        if Instruction_map.mem producer arguments then
                          error ?span:description.span "HCBACK0003"
                            "native call repeats a pushed argument producer"
                        else
                          Ok (Instruction_map.add producer argument arguments))
                      (Ok arguments) (Runtime.arguments call))
            (Ok arguments)
            (Graph.instructions block |> Sequence.instructions))
        (Ok Instruction_map.empty) (Graph.blocks graph)
    in
    let valid_flags description result type_ =
      if description.Sequence.flags = 0L then true
      else if description.flags <> 0x2000L then false
      else
        match
          Instruction_map.find_opt description.instruction_id pushed_arguments
        with
        | Some argument ->
            Sequence.Value_id.equal result.Sequence.value_id
              (Runtime.argument_value argument)
            && Type.equal type_ (Runtime.argument_source_type argument)
            && Option.is_none (Runtime.argument_prepared_default argument)
        | None -> false
    in
    let* regions =
      List.fold_left
        (fun result block ->
          let* regions = result in
          List.fold_left
            (fun result instruction ->
              let* regions = result in
              let description = Sequence.description instruction in
              if description.opcode <> Ir.Opcode.Ic_str_const then Ok regions
              else
                match
                  ( description.operands,
                    description.result,
                    description.target_type,
                    description.payload )
                with
                | [], Some result, Some type_, Some (Sequence.Bytes bytes)
                  when is_literal_type type_
                       && valid_flags description result type_ ->
                    let length = String.length bytes in
                    let available =
                      min max_literal_bytes Sys.max_string_length - !logical
                    in
                    if length >= available then
                      error ?span:description.span "HCBACK0004"
                        "owned native strings exceed max_literal_bytes"
                    else
                      let count = length + 1 in
                      let remaining =
                        min max_arena_bytes Sys.max_string_length - !used
                      in
                      if remaining < 32 || count > (remaining - 32) / 33 then
                        error ?span:description.span "HCBACK0004"
                          "native literal bytes and reference tables exceed \
                           the private arena bound"
                      else if
                        Instruction_map.mem description.instruction_id regions
                      then
                        error ?span:description.span "HCBACK0003"
                          "native literal graph repeats an instruction identity"
                      else
                        let table_bytes = (count + 1) * 32 in
                        let region =
                          {
                            data_offset = !used;
                            table_offset = !used + count;
                            byte_count = count;
                          }
                        in
                        chunks :=
                          (region.data_offset - arena_prefix_bytes, bytes)
                          :: !chunks;
                        logical := !logical + count;
                        metadata := !metadata + table_bytes;
                        used := !used + count + table_bytes;
                        Ok
                          (Instruction_map.add description.instruction_id region
                             regions)
                | _ ->
                    error ?span:description.span "HCBACK0003"
                      "native IC_STR_CONST requires its original internal U8 \
                       pointer and byte payload")
            (Ok regions)
            (Graph.instructions block |> Sequence.instructions))
        (Ok Instruction_map.empty) (Graph.blocks graph)
    in
    Ok { owner; graph; regions }
  in
  let* entry_graph = collect Runtime.Entry (Ir.X87_stack.graph entry) in
  let* reversed =
    List.fold_left
      (fun result (definition : Ir.Integer_interpreter.function_definition) ->
        let* graphs = result in
        let* graph =
          collect (Runtime.Function definition.body)
            (Function.body definition.body)
        in
        Ok (graph :: graphs))
      (Ok [ entry_graph ]) functions
  in
  let bytes = Bytes.make (!used - arena_prefix_bytes) '\000' in
  List.iter
    (fun (offset, payload) ->
      Bytes.blit_string payload 0 bytes offset (String.length payload))
    !chunks;
  Ok
    {
      graphs = List.rev reversed;
      literal_bytes = !logical;
      metadata_bytes = !metadata;
      image = Bytes.to_string bytes;
    }

let find storage ~owner ~graph instruction_id =
  Option.bind
    (List.find_opt
       (fun candidate ->
         same_owner candidate.owner owner && candidate.graph == graph)
       storage.graphs)
    (fun owned -> Instruction_map.find_opt instruction_id owned.regions)

let data_offset region = region.data_offset
let table_offset region = region.table_offset
let byte_count region = region.byte_count
let is_empty storage = storage.literal_bytes = 0
let literal_bytes storage = storage.literal_bytes
let metadata_bytes storage = storage.metadata_bytes
let image storage = Bytes.to_string (Bytes.of_string storage.image)
