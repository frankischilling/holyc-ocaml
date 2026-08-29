module Sequence = Instruction_sequence
module Top_level = Top_level_body
module Statement = Top_level_statement_lowering
module Semantic_result = Sema.Function_call_expression_result
module Tree = Sema.Top_level_expression_tree
module Target = Sema.Top_level_function_call_target_classification
module Resolution = Sema.Function_call_resolution
module Outer_binding = Sema.Top_level_outer_expression_binding
module Publication = Sema.Module_expression_binding
module Result_id_map = Map.Make (Semantic_result.Id)

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

type mixed_item =
  | Direct_call of
      Target.t * Semantic_result.top_level_statement_result
  | Expression of Semantic_result.top_level_statement_result

type mixed_candidate = {
  statement_index : int;
  result : Semantic_result.expression_result;
  source : Sema.Function_call_resolution.argument_expression;
  is_direct_call : bool;
}

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

let direct_call_count_error statement_count target_count option_count =
  make_error "HCIRL0004"
    (Printf.sprintf
       "top-level direct-call batch has %d statements, %d targets, and %d \
        compiler-option snapshots"
       statement_count target_count option_count)

let expression_count_error statement_count option_count =
  make_error "HCIRL0004"
    (Printf.sprintf
       "top-level expression batch has %d statements and %d compiler-option \
        snapshots"
       statement_count option_count)

let mixed_count_error statement_count option_count =
  make_error "HCIRL0004"
    (Printf.sprintf
       "top-level mixed batch has %d statements and %d compiler-option snapshots"
       statement_count option_count)

let target_result_id target =
  target |> Target.source |> Semantic_result.top_level_direct_result_id

let target_source target =
  target |> Target.source |> Semantic_result.top_level_direct_source
  |> Tree.call_result_expression

let unmatched_target_error target =
  make_error "HCIRL0004"
    (Printf.sprintf
       "top-level mixed batch target result %d does not match a standalone \
        statement root"
       (target |> target_result_id |> Semantic_result.Id.to_int))

let ambiguous_target_error target =
  make_error "HCIRL0004"
    (Printf.sprintf
       "top-level mixed batch target result %d matches more than one standalone \
        statement root"
       (target |> target_result_id |> Semantic_result.Id.to_int))

let duplicate_target_error statement_index =
  make_error ~statement_index "HCIRL0004"
    "top-level mixed batch has more than one classified target for the same \
     standalone statement root"

let missing_target_error statement_index =
  make_error ~statement_index "HCIRL0004"
    "top-level mixed batch is missing the classified target for a standalone \
     direct-call statement"

let call_is_direct_root source call =
  Tree.call_result_expression call == source
  && Resolution.call_callee_form (Tree.call_source call)
     = Resolution.Identifier_callee
  &&
  match
    call |> Tree.call_callee |> Outer_binding.occurrence_resolution
  with
  | Outer_binding.Module_binding publication ->
      Publication.publication_kind publication = Publication.Function
  | Outer_binding.Outer_binding _ -> false

let statement_has_direct_root statement source =
  statement |> Semantic_result.top_level_statement_source
  |> Tree.statement_calls |> List.exists (call_is_direct_root source)

let standalone_candidate statement_index statement =
  match Semantic_result.top_level_statement_roots statement with
  | [ root ] ->
      let source = Semantic_result.top_level_root_source root in
      (match Tree.root_role source with
      | Tree.Expression_statement _ ->
          Some
            {
              statement_index;
              result = Semantic_result.top_level_root_value root;
              source = Tree.root_expression source;
              is_direct_call =
                statement_has_direct_root statement (Tree.root_expression source);
            }
      | Tree.Implicit_output_fixed _
      | Tree.Implicit_output_argument _
      | Tree.Condition _
      | Tree.Switch_selector _
      | Tree.Switch_case_value _
      | Tree.Local_array_dimension _
      | Tree.Local_initializer _
      | Tree.Return_value _ -> None)
  | [] | _ :: _ :: _ -> None

let mixed_candidates statements =
  let candidates_by_statement = Array.make (List.length statements) None in
  let candidates =
    statements
    |> List.mapi (fun statement_index statement ->
        standalone_candidate statement_index statement)
    |> List.fold_left
         (fun candidates -> function
           | None -> candidates
           | Some candidate ->
               candidates_by_statement.(candidate.statement_index) <-
                 Some candidate;
               Result_id_map.update
                 (Semantic_result.result_id candidate.result)
                 (function
                   | None -> Some [ candidate ]
                   | Some matching -> Some (candidate :: matching))
                 candidates)
         Result_id_map.empty
  in
  (candidates, candidates_by_statement)

let matching_candidates candidates target =
  match Result_id_map.find_opt (target_result_id target) candidates with
  | None -> []
  | Some matching ->
      let source = target_source target in
      List.filter (fun candidate -> candidate.source == source) matching

let assign_mixed_targets candidates assignments targets =
  let rec assign = function
    | [] -> Ok ()
    | target :: remaining -> (
        match matching_candidates candidates target with
        | [] -> Error [ unmatched_target_error target ]
        | [ candidate ] -> (
            match assignments.(candidate.statement_index) with
            | Some _ ->
                Error [ duplicate_target_error candidate.statement_index ]
            | None ->
                assignments.(candidate.statement_index) <- Some target;
                assign remaining)
        | _ -> Error [ ambiguous_target_error target ])
  in
  assign targets

let mixed_items ~targets statements =
  let candidates, candidates_by_statement = mixed_candidates statements in
  let assignments = Array.make (List.length statements) None in
  match assign_mixed_targets candidates assignments targets with
  | Error _ as error -> error
  | Ok () ->
      let rec build reversed statement_index = function
        | [] -> Ok (List.rev reversed)
        | statement :: remaining -> (
            match
              ( assignments.(statement_index),
                candidates_by_statement.(statement_index) )
            with
            | Some target, _ ->
                build
                  (Direct_call (target, statement) :: reversed)
                  (statement_index + 1) remaining
            | None, Some candidate when candidate.is_direct_call ->
                Error [ missing_target_error statement_index ]
            | None, _ ->
                build (Expression statement :: reversed) (statement_index + 1)
                  remaining)
      in
      build [] 0 statements

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

let lower_items ~stream_id ~block_id ~instruction_id ~value_id ~compiler_options
    lower_statement items =
  let rec lower reversed_bodies statement_index stream_id block_id
      instruction_id value_id options items =
    match (options, items) with
    | [], [] ->
        Ok
          (Lowered
             {
               bodies_ = List.rev reversed_bodies;
               next_stream_id_ = stream_id;
               next_block_id_ = block_id;
               next_instruction_id_ = instruction_id;
               next_value_id_ = value_id;
             })
    | compiler_options :: remaining_options, item :: remaining_items -> (
        match
          lower_statement ~stream_id ~block_id ~instruction_id ~value_id
            ~compiler_options item
        with
        | Error errors -> Error (List.map (child_error statement_index) errors)
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
                  remaining_options remaining_items))
    | _ -> assert false
  in
  lower [] 0 stream_id block_id instruction_id value_id compiler_options items

let lower_direct_calls ~stream_id ~block_id ~instruction_id ~value_id
    ~compiler_options ~targets statements =
  let statement_count = List.length statements in
  let target_count = List.length targets in
  let option_count = List.length compiler_options in
  if statement_count <> target_count || statement_count <> option_count then
    Error [ direct_call_count_error statement_count target_count option_count ]
  else
    lower_items ~stream_id ~block_id ~instruction_id ~value_id ~compiler_options
      (fun ~stream_id ~block_id ~instruction_id ~value_id ~compiler_options
           (target, statement) ->
        Statement.lower_direct_call ~stream_id ~block_id ~instruction_id
          ~value_id ~compiler_options ~target statement)
      (List.combine targets statements)

let lower_expressions ~stream_id ~block_id ~instruction_id ~value_id
    ~compiler_options statements =
  let statement_count = List.length statements in
  let option_count = List.length compiler_options in
  if statement_count <> option_count then
    Error [ expression_count_error statement_count option_count ]
  else
    lower_items ~stream_id ~block_id ~instruction_id ~value_id ~compiler_options
      Statement.lower_expression statements

let lower_mixed ~stream_id ~block_id ~instruction_id ~value_id
    ~compiler_options ~targets statements =
  let statement_count = List.length statements in
  let option_count = List.length compiler_options in
  if statement_count <> option_count then
    Error [ mixed_count_error statement_count option_count ]
  else
    match mixed_items ~targets statements with
    | Error _ as error -> error
    | Ok items ->
        lower_items ~stream_id ~block_id ~instruction_id ~value_id
          ~compiler_options
          (fun ~stream_id ~block_id ~instruction_id ~value_id
               ~compiler_options -> function
            | Direct_call (target, statement) ->
                Statement.lower_direct_call ~stream_id ~block_id
                  ~instruction_id ~value_id ~compiler_options ~target statement
            | Expression statement ->
                Statement.lower_expression ~stream_id ~block_id ~instruction_id
                  ~value_id ~compiler_options statement)
          items

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
