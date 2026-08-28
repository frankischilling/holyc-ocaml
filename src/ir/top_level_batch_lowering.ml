module Sequence = Instruction_sequence
module Top_level = Top_level_body
module Statement = Top_level_statement_lowering

type error = {
  code : string;
  message : string;
  statement_index : int option;
  stream_id : int option;
  item_position : int option;
  block_id : int option;
  instruction_id : int option;
  span : Common.Span.t option;
}

type t = {
  bodies_ : Top_level.t list;
  next_stream_id_ : Top_level.Stream_id.t;
  next_block_id_ : Sequence.Block_id.t;
  next_instruction_id_ : Sequence.Instruction_id.t;
  next_value_id_ : Sequence.Value_id.t;
}

type lowering_result = Lowered of t | Unsupported_batch

let reference_commit = Opcode.reference_commit

let make_error ?statement_index ?stream_id ?item_position ?block_id
    ?instruction_id ?span code message =
  {
    code;
    message;
    statement_index;
    stream_id;
    item_position;
    block_id;
    instruction_id;
    span;
  }

let child_error statement_index (error : Statement.error) =
  make_error ~statement_index ~stream_id:error.stream_id
    ~item_position:error.item_position ?block_id:error.block_id
    ?instruction_id:error.instruction_id ?span:error.span error.code
    error.message

let count_error statement_count target_count option_count =
  make_error "HCIRL0004"
    (Printf.sprintf
       "top-level direct-call batch has %d statements, %d targets, and %d \
        compiler-option snapshots"
       statement_count target_count option_count)

let next_stream_id ~statement_index stream_id =
  let current = Top_level.Stream_id.to_int stream_id in
  if current = Int.max_int then
    Error
      (make_error ~statement_index ~stream_id:current "HCIRL0005"
         "cannot allocate another top-level stream identity because the host \
          integer range is exhausted")
  else
    match Top_level.Stream_id.of_int (current + 1) with
    | Ok next -> Ok next
    | Error error ->
        Error
          (make_error ~statement_index ~stream_id:current ?span:error.span
             error.code error.message)

let next_block_id ~statement_index block_id =
  let current = Sequence.Block_id.to_int block_id in
  if current = Int.max_int then
    Error
      (make_error ~statement_index ~block_id:current "HCIRL0005"
         "cannot allocate another top-level entry-block identity because the \
          host integer range is exhausted")
  else
    match Sequence.Block_id.of_int (current + 1) with
    | Ok next -> Ok next
    | Error error ->
        Error
          (make_error ~statement_index ~block_id:current ?span:error.span
             error.code error.message)

let advance_owner_ids ~statement_index stream_id block_id =
  match
    ( next_stream_id ~statement_index stream_id,
      next_block_id ~statement_index block_id )
  with
  | Ok next_stream_id, Ok next_block_id -> Ok (next_stream_id, next_block_id)
  | Error stream_error, Ok _ -> Error [ stream_error ]
  | Ok _, Error block_error -> Error [ block_error ]
  | Error stream_error, Error block_error -> Error [ stream_error; block_error ]

let lower_direct_calls ~stream_id ~block_id ~instruction_id ~value_id
    ~compiler_options ~targets statements =
  let statement_count = List.length statements in
  let target_count = List.length targets in
  let option_count = List.length compiler_options in
  if statement_count <> target_count || statement_count <> option_count then
    Error [ count_error statement_count target_count option_count ]
  else
    let rec lower reversed_bodies statement_index stream_id block_id
        instruction_id value_id options targets statements =
      match (options, targets, statements) with
      | [], [], [] ->
          Ok
            (Lowered
               {
                 bodies_ = List.rev reversed_bodies;
                 next_stream_id_ = stream_id;
                 next_block_id_ = block_id;
                 next_instruction_id_ = instruction_id;
                 next_value_id_ = value_id;
               })
      | ( compiler_options :: remaining_options,
          target :: remaining_targets,
          statement :: remaining_statements ) -> (
          match
            Statement.lower_direct_call ~stream_id ~block_id ~instruction_id
              ~value_id ~compiler_options ~target statement
          with
          | Error errors ->
              Error (List.map (child_error statement_index) errors)
          | Ok Statement.Unsupported_statement -> Ok Unsupported_batch
          | Ok (Statement.Lowered lowered) -> (
              match advance_owner_ids ~statement_index stream_id block_id with
              | Error _ as error -> error
              | Ok (next_stream_id, next_block_id) ->
                  lower
                    (Statement.body lowered :: reversed_bodies)
                    (statement_index + 1) next_stream_id next_block_id
                    (Statement.next_instruction_id lowered)
                    (Statement.next_value_id lowered)
                    remaining_options remaining_targets remaining_statements))
      | _ -> assert false
    in
    lower [] 0 stream_id block_id instruction_id value_id compiler_options
      targets statements

let bodies lowered = lowered.bodies_
let next_stream_id lowered = lowered.next_stream_id_
let next_block_id lowered = lowered.next_block_id_
let next_instruction_id lowered = lowered.next_instruction_id_
let next_value_id lowered = lowered.next_value_id_

let human lowered =
  let buffer = Buffer.create 2048 in
  Printf.bprintf buffer
    "holyc-ir-top-level-batch-v1 reference=%s\n\
     bodies=%d next-stream=%d next-block=%d next-instruction=%d next-value=%d\n"
    reference_commit
    (List.length lowered.bodies_)
    (Top_level.Stream_id.to_int lowered.next_stream_id_)
    (Sequence.Block_id.to_int lowered.next_block_id_)
    (Sequence.Instruction_id.to_int lowered.next_instruction_id_)
    (Sequence.Value_id.to_int lowered.next_value_id_);
  List.iteri
    (fun index body ->
      Printf.bprintf buffer "body[%d]\n" index;
      Buffer.add_string buffer (Top_level.human body))
    lowered.bodies_;
  Buffer.contents buffer
