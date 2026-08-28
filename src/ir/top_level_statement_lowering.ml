module Sequence = Instruction_sequence
module Graph = Block_graph
module Top_level = Top_level_body
module Statement = Expression_statement_lowering
module Semantic_result = Sema.Function_call_expression_result
module Tree = Sema.Top_level_expression_tree
module Binding = Sema.Top_level_outer_expression_binding

type error = {
  code : string;
  message : string;
  stream_id : int;
  item_position : int;
  block_id : int option;
  instruction_id : int option;
  span : Common.Span.t option;
}

type t = {
  body_ : Top_level.t;
  end_instruction_id_ : Sequence.Instruction_id.t;
  next_instruction_id_ : Sequence.Instruction_id.t;
  next_value_id_ : Sequence.Value_id.t;
}

type lowering_result = Lowered of t | Unsupported_statement

let reference_commit = Opcode.reference_commit

let make_error ?block_id ?instruction_id ?span ~stream_id ~item_position code
    message =
  {
    code;
    message;
    stream_id = Top_level.Stream_id.to_int stream_id;
    item_position;
    block_id;
    instruction_id;
    span;
  }

let span_of_origin ~stream_id ~item_position description = function
  | Sema.Symbol.Source_location location -> Ok location.span
  | Sema.Symbol.Pinned_source _ | Sema.Symbol.Synthesized _ ->
      Error
        (make_error ~stream_id ~item_position "HCIRL0004"
           (Printf.sprintf "%s does not have a source location" description))

let sequence_error ~stream_id ~item_position ~block_id (error : Sequence.error)
    =
  make_error ~stream_id ~item_position
    ~block_id:(Sequence.Block_id.to_int block_id)
    ?instruction_id:error.instruction_id ?span:error.span error.code
    error.message

let graph_error ~stream_id ~item_position (error : Graph.error) =
  make_error ~stream_id ~item_position ?block_id:error.block_id
    ?instruction_id:error.instruction_id ?span:error.span error.code
    error.message

let body_error ~stream_id ~item_position (error : Top_level.error) =
  make_error ~stream_id ~item_position ?block_id:error.block_id
    ?instruction_id:error.instruction_id ?span:error.span error.code
    error.message

let next_instruction_id ~stream_id ~item_position ~span instruction_id =
  let current = Sequence.Instruction_id.to_int instruction_id in
  if current = Int.max_int then
    Error
      (make_error ~stream_id ~item_position ~span "HCIRL0005"
         "cannot allocate the top-level stream-end instruction because the \
          host integer range is exhausted")
  else
    match Sequence.Instruction_id.of_int (current + 1) with
    | Ok next -> Ok next
    | Error error ->
        Error
          (make_error ~stream_id ~item_position ?span:error.span error.code
             error.message)

let descriptions sequence =
  sequence |> Sequence.instructions |> List.map Sequence.description

let statement_metadata ~stream_id statement =
  let tree_statement = Semantic_result.top_level_statement_source statement in
  let source = Tree.statement_source tree_statement in
  let item_position = Binding.statement_item_index source in
  match
    span_of_origin ~stream_id ~item_position "top-level statement"
      (Binding.statement_origin source)
  with
  | Error error -> Error [ error ]
  | Ok span -> (
      match Semantic_result.top_level_statement_roots statement with
      | [ root ] -> Ok (item_position, span, root)
      | roots ->
          Error
            [
              make_error ~stream_id ~item_position ~span "HCIRL0004"
                (Printf.sprintf
                   "top-level direct-call body requires exactly one expression \
                    root, but the statement has %d"
                   (List.length roots));
            ])

let build_body ~stream_id ~block_id:block_id_ ~compiler_options ~item_position
    ~span statement =
  let end_instruction_id_ = Statement.next_instruction_id statement in
  match
    next_instruction_id ~stream_id ~item_position ~span end_instruction_id_
  with
  | Error error -> Error [ error ]
  | Ok next_instruction_id_ -> (
      let end_description : Sequence.description =
        {
          instruction_id = end_instruction_id_;
          opcode = Opcode.Ic_end;
          operands = [];
          result = None;
          target_type = None;
          payload = None;
          flags = 0L;
          span = None;
        }
      in
      let instructions =
        descriptions (Statement.sequence statement) @ [ end_description ]
      in
      match Sequence.create instructions with
      | Error errors ->
          Error
            (List.map
               (sequence_error ~stream_id ~item_position ~block_id:block_id_)
               errors)
      | Ok sequence -> (
          let block =
            Graph.{ block_id = block_id_; instructions = descriptions sequence }
          in
          match Graph.create ~entry:block_id_ [ block ] with
          | Error errors ->
              Error (List.map (graph_error ~stream_id ~item_position) errors)
          | Ok body -> (
              match
                Top_level.create
                  {
                    stream_id;
                    item_position;
                    compiler_options;
                    span = Some span;
                    body;
                  }
              with
              | Error errors ->
                  Error (List.map (body_error ~stream_id ~item_position) errors)
              | Ok body_ ->
                  Ok
                    (Lowered
                       {
                         body_;
                         end_instruction_id_;
                         next_instruction_id_;
                         next_value_id_ = Statement.next_value_id statement;
                       }))))

let lower_direct_call ~stream_id ~block_id ~instruction_id ~value_id
    ~compiler_options ~target statement =
  match statement_metadata ~stream_id statement with
  | Error _ as error -> error
  | Ok (item_position, span, root) -> (
      match
        Statement.lower_top_level_direct_call_statement ~instruction_id
          ~value_id ~target root
      with
      | Error errors ->
          Error
            (List.map
               (sequence_error ~stream_id ~item_position ~block_id)
               errors)
      | Ok Statement.Unsupported_expression -> Ok Unsupported_statement
      | Ok (Statement.Lowered lowered) ->
          build_body ~stream_id ~block_id ~compiler_options ~item_position ~span
            lowered)

let body lowered = lowered.body_
let end_instruction_id lowered = lowered.end_instruction_id_
let next_instruction_id lowered = lowered.next_instruction_id_
let next_value_id lowered = lowered.next_value_id_

let human lowered =
  Printf.sprintf
    "holyc-ir-top-level-statement-v1 reference=%s\n\
     end-instruction=!i%d next-instruction=%d next-value=%d\n\
     %s"
    reference_commit
    (Sequence.Instruction_id.to_int lowered.end_instruction_id_)
    (Sequence.Instruction_id.to_int lowered.next_instruction_id_)
    (Sequence.Value_id.to_int lowered.next_value_id_)
    (Top_level.human lowered.body_)
