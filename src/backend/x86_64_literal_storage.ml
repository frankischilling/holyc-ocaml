module Sequence = Ir.Instruction_sequence
module Graph = Ir.Block_graph
module Runtime = Ir.Runtime_call_context
module Function = Ir.Function_body
module Type = Sema.Type
module Instruction_map = Map.Make (Sequence.Instruction_id)
module Graph_map = Map.Make (Int)

type error = { code : string; message : string; span : Common.Span.t option }
type region = { data_offset : int; table_offset : int; byte_count : int }

type owned_graph = {
  owner : Runtime.owner;
  graph : Graph.t;
  runtime_calls : Runtime.t;
  regions : region Instruction_map.t;
}

type source = {
  owner : Runtime.owner;
  graph : Graph.t;
  runtime_calls : Runtime.t;
}

type t = {
  graphs : owned_graph Graph_map.t;
  literal_bytes : int;
  metadata_bytes : int;
  arena_prefix_bytes : int;
  arena_bytes : int;
  chunks : (int * string) list;
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

let empty =
  {
    graphs = Graph_map.empty;
    literal_bytes = 0;
    metadata_bytes = 0;
    arena_prefix_bytes = 0;
    arena_bytes = 0;
    chunks = [];
  }

let source ~runtime_calls ~owner ~graph =
  if Runtime.matches_graph runtime_calls ~owner graph then
    Ok { runtime_calls; owner; graph }
  else
    error "HCBACK0003" "native literal source is not its original sealed graph"

let append storage ~max_literal_bytes ~max_arena_bytes ~arena_prefix_bytes
    ~sources =
  let ( let* ) = Result.bind in
  let* () = validate_limit ~max_literal_bytes in
  let* () =
    if
      max_arena_bytes <= 0
      || max_arena_bytes > 33_554_432
      || arena_prefix_bytes < 0
      || arena_prefix_bytes > max_arena_bytes
      || arena_prefix_bytes < storage.arena_bytes
    then
      error "HCBACK0004"
        "native literal arena prefix exceeds the host byte bound"
    else Ok ()
  in
  let logical = ref storage.literal_bytes in
  let metadata = ref storage.metadata_bytes in
  let used = ref arena_prefix_bytes in
  let chunks = ref storage.chunks in
  let collect (source : source) =
    let { owner; graph; runtime_calls } = source in
    let* () =
      if Runtime.matches_graph runtime_calls ~owner graph then Ok ()
      else error "HCBACK0003" "native literal source changed after sealing"
    in
    match Graph_map.find_opt (Graph.storage_identity graph) storage.graphs with
    | Some owned
      when owned.runtime_calls == runtime_calls
           && owned.graph == graph
           && same_owner owned.owner owner -> Ok owned
    | Some _ ->
        error "HCBACK0003" "native literal graph has another original context"
    | None ->
        let* pushed_arguments =
          List.fold_left
            (fun result block ->
              let* arguments = result in
              List.fold_left
                (fun result instruction ->
                  let* arguments = result in
                  let description = Sequence.description instruction in
                  if description.opcode <> Ir.Opcode.Ic_call_start then
                    Ok arguments
                  else
                    let original_arguments =
                      match
                        Runtime.find_start runtime_calls ~owner
                          description.instruction_id
                      with
                      | Some call -> Runtime.arguments call
                      | None ->
                          Option.fold ~none:[]
                            ~some:(fun callback ->
                              callback.Runtime.callback_arguments)
                            (Runtime.find_callback_start runtime_calls ~owner
                               description.instruction_id)
                    in
                    List.fold_left
                      (fun result argument ->
                        let* arguments = result in
                        let producer = Runtime.argument_producer argument in
                        if Instruction_map.mem producer arguments then
                          error ?span:description.span "HCBACK0003"
                            "native call repeats a pushed argument producer"
                        else
                          Ok (Instruction_map.add producer argument arguments))
                      (Ok arguments) original_arguments)
                (Ok arguments)
                (Graph.instructions block |> Sequence.instructions))
            (Ok Instruction_map.empty) (Graph.blocks graph)
        in
        let valid_flags description result type_ =
          if description.Sequence.flags = 0L then true
          else if description.flags <> 0x2000L then false
          else
            match
              Instruction_map.find_opt description.instruction_id
                pushed_arguments
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
                  if description.opcode <> Ir.Opcode.Ic_str_const then
                    Ok regions
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
                          if remaining < 32 || count > (remaining - 32) / 33
                          then
                            error ?span:description.span "HCBACK0004"
                              "native literal bytes and reference tables \
                               exceed the private arena bound"
                          else if
                            Instruction_map.mem description.instruction_id
                              regions
                          then
                            error ?span:description.span "HCBACK0003"
                              "native literal graph repeats an instruction \
                               identity"
                          else
                            let table_bytes = (count + 1) * 32 in
                            let region =
                              {
                                data_offset = !used;
                                table_offset = !used + count;
                                byte_count = count;
                              }
                            in
                            chunks := (region.data_offset, bytes) :: !chunks;
                            logical := !logical + count;
                            metadata := !metadata + table_bytes;
                            used := !used + count + table_bytes;
                            Ok
                              (Instruction_map.add description.instruction_id
                                 region regions)
                    | _ ->
                        error ?span:description.span "HCBACK0003"
                          "native IC_STR_CONST requires its original internal \
                           U8 pointer and byte payload")
                (Ok regions)
                (Graph.instructions block |> Sequence.instructions))
            (Ok Instruction_map.empty) (Graph.blocks graph)
        in
        Ok { owner; graph; runtime_calls; regions }
  in
  let* graphs =
    List.fold_left
      (fun result source ->
        let* graphs = result in
        match
          Graph_map.find_opt (Graph.storage_identity source.graph) graphs
        with
        | Some (owned : owned_graph)
          when same_owner owned.owner source.owner
               && owned.graph == source.graph
               && owned.runtime_calls == source.runtime_calls
               && Runtime.matches_graph source.runtime_calls ~owner:source.owner
                    source.graph -> Ok graphs
        | Some _ ->
            error "HCBACK0003"
              "native literal graph has another original context"
        | None ->
            let* graph = collect source in
            Ok
              (Graph_map.add (Graph.storage_identity source.graph) graph graphs))
      (Ok storage.graphs) sources
  in
  Ok
    {
      graphs;
      literal_bytes = !logical;
      metadata_bytes = !metadata;
      arena_prefix_bytes = storage.arena_prefix_bytes;
      arena_bytes = !used;
      chunks = !chunks;
    }

let create ~max_literal_bytes ~max_arena_bytes ~arena_prefix_bytes
    ~runtime_calls ~initialization ~entry ~functions =
  let ( let* ) = Result.bind in
  if
    not
      (Runtime.matches runtime_calls ~entry
         ~initialization:(Some initialization)
         ~functions:
           (List.map
              (fun (definition : Ir.Integer_interpreter.function_definition) ->
                definition.body)
              functions))
  then
    error "HCBACK0003"
      "native literal storage requires its original callable graph"
  else
    let* entry_source =
      source ~runtime_calls ~owner:Runtime.Entry
        ~graph:(Ir.X87_stack.graph entry)
    in
    let* sources =
      List.fold_left
        (fun result (definition : Ir.Integer_interpreter.function_definition) ->
          let* sources = result in
          let* source =
            source ~runtime_calls ~owner:(Runtime.Function definition.body)
              ~graph:(Function.body definition.body)
          in
          Ok (source :: sources))
        (Ok [ entry_source ]) functions
    in
    append
      { empty with arena_prefix_bytes }
      ~max_literal_bytes ~max_arena_bytes ~arena_prefix_bytes
      ~sources:(List.rev sources)

let find storage ~owner ~graph instruction_id =
  Option.bind
    (Graph_map.find_opt (Graph.storage_identity graph) storage.graphs)
    (fun owned ->
      if same_owner owned.owner owner && owned.graph == graph then
        Instruction_map.find_opt instruction_id owned.regions
      else None)

let data_offset region = region.data_offset
let table_offset region = region.table_offset
let byte_count region = region.byte_count
let is_empty storage = storage.literal_bytes = 0
let literal_bytes storage = storage.literal_bytes
let metadata_bytes storage = storage.metadata_bytes
let arena_bytes storage = storage.arena_bytes

let initializations_since storage ~arena_prefix_bytes =
  let rec collect kept chunks =
    match chunks with
    | ((offset, _) as chunk) :: rest when offset >= arena_prefix_bytes ->
        collect (chunk :: kept) rest
    | _ -> List.rev kept
  in
  collect [] storage.chunks

let image storage =
  let bytes =
    Bytes.make (storage.arena_bytes - storage.arena_prefix_bytes) '\000'
  in
  List.iter
    (fun (offset, payload) ->
      Bytes.blit_string payload 0 bytes
        (offset - storage.arena_prefix_bytes)
        (String.length payload))
    storage.chunks;
  Bytes.to_string bytes
