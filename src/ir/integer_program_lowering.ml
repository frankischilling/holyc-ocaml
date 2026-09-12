module Sequence = Instruction_sequence
module Typed = Sema.Function_call_expression_result
module Source = Sema.Function_call_resolution

type statement =
  | Empty of Common.Span.t
  | Expression of Typed.expression_result
  | Function_output of Sema.Implicit_output_argument_binding.bound_output
  | Top_level_output of
      Sema.Top_level_implicit_output_argument_binding.bound_output
  | Initialize of Typed.initializer_result
  | Initialize_global of Typed.top_level_root_result
  | Initialize_fragment of Initializer_fragment_destination.t
  | Initialize_static of Integer_globals.static_slot
  | Initialize_static_leaf of
      Integer_globals.static_slot * Typed.initializer_result
  | Publish_array of Global_initialization.prepared_root
  | Return of Typed.return_result
  | Block of statement list
  | If of Typed.expression_result * statement * statement option
  | While of Typed.expression_result * statement
  | Do_while of statement * Typed.expression_result
  | For of statement * Typed.expression_result * statement option * statement
  | Break of Common.Span.t

type t = {
  expression_source_ : (Integer_globals.t * Typed.expression_result) option;
  graph_ : X87_stack.t;
  initializer_regions_ : Global_initialization.region_description list;
  static_initializer_regions_ :
    Global_initialization.static_region_description list;
  runtime_calls_ : Runtime_call_context.description list;
  publications_ : Global_initialization.publication_description list;
  publication_evidence_ : Global_initialization.publication_evidence option;
}

let graph result = result.graph_

let owns_expression result ~globals ~value =
  match result.expression_source_ with
  | Some (original_globals, original_value) ->
      original_globals == globals && original_value == value
  | None -> false

let initializer_regions result = result.initializer_regions_
let static_initializer_regions result = result.static_initializer_regions_
let runtime_calls result = result.runtime_calls_
let publications result = result.publications_
let publication_evidence result = result.publication_evidence_

exception Invalid of Common.Diagnostic.t list

let fail span code message =
  raise
    (Invalid
       [
         Common.Diagnostic.make ~code ~severity:Common.Diagnostic.Error ~message
           ~primary:span ();
       ])

let span_of_result fallback result =
  match Typed.result_origin result with
  | Sema.Symbol.Source_location location -> location.span
  | _ -> fallback

let lower_complete ?frame ?globals ?records ?(top_calls = [])
    ?(function_calls = []) ~span statements =
  try
    let instruction_count = ref 0
    and value_count = ref 0
    and block_count = ref 0 in
    let initial_regions = ref [] in
    let static_regions = ref [] in
    let runtime_calls = ref [] in
    let publications = ref [] in
    let checked_id = function
      | Ok value -> value
      | Error (e : Sequence.error) -> fail span e.code e.message
    in
    let allocate make count =
      if !count = Int.max_int then
        fail span "HCIRL0005" "integer program identity space is exhausted";
      let result = make !count |> checked_id in
      incr count;
      result
    in
    let block () = allocate Sequence.Block_id.of_int block_count in
    let entry = block () in
    let leave = Option.map (fun _ -> block ()) frame in
    let current = ref (Some (entry, [])) and blocks = ref [] in
    let start block_id =
      match !current with
      | None -> current := Some (block_id, [])
      | Some _ ->
          fail span "HCRUN0004"
            "cannot start a block before terminating its predecessor"
    in
    let ensure_open () = if !current = None then start (block ()) in
    let append description =
      ensure_open ();
      match !current with
      | Some (id, rev) -> current := Some (id, description :: rev)
      | None -> assert false
    in
    let finish () =
      match !current with
      | Some (block_id, rev) ->
          blocks :=
            { Block_graph.block_id; instructions = List.rev rev } :: !blocks;
          current := None
      | None -> fail span "HCRUN0004" "cannot terminate an absent block"
    in
    let instruction ?(operands = []) ?payload ?(flags = 0L) ~at opcode =
      let instruction_id =
        allocate Sequence.Instruction_id.of_int instruction_count
      in
      append
        {
          Sequence.instruction_id;
          opcode;
          operands;
          result = None;
          target_type = None;
          payload;
          flags;
          span = Some at;
        }
    in
    let jump ~at target =
      instruction ~at ~payload:(Sequence.Block target) Opcode.Ic_jmp;
      finish ()
    in
    let rec validate_expression expression =
      let source = Typed.result_source expression in
      (match Source.argument_expression_kind source with
      | Source.Binary_expression binary ->
          let comparison = function
            | Opcode.Ic_less
            | Opcode.Ic_greater
            | Opcode.Ic_less_equ
            | Opcode.Ic_greater_equ
            | Opcode.Ic_equ_equ
            | Opcode.Ic_not_equ -> true
            | _ -> false
          in
          let adjacent source =
            match Source.argument_expression_kind source with
            | Source.Binary_expression child ->
                comparison (Source.binary_operator child)
            | _ -> false
          in
          if
            comparison (Source.binary_operator binary)
            && adjacent (Source.binary_left binary)
          then
            fail
              (span_of_result span expression)
              "HCRUN0003"
              "conditional comparison chains require shared values across \
               branches, which are not implemented"
      | _ -> ());
      Option.iter validate_expression (Typed.result_operand expression);
      Option.iter
        (fun (left, right) ->
          validate_expression left;
          validate_expression right)
        (Typed.result_binary_operands expression);
      Option.iter
        (fun (base, index) ->
          validate_expression base;
          validate_expression index)
        (Typed.result_index_operands expression)
    in
    let append_fragment sequence next_instruction next_value result_value =
      sequence |> Sequence.instructions
      |> List.iter (fun instruction ->
          append (Sequence.description instruction));
      instruction_count := Sequence.Instruction_id.to_int next_instruction;
      value_count := Sequence.Value_id.to_int next_value;
      result_value
    in
    let append_expression result =
      append_fragment
        (Expression_lowering.sequence result)
        (Expression_lowering.next_instruction_id result)
        (Expression_lowering.next_value_id result)
        (Expression_lowering.result_value result)
    in
    let lower_errors errors =
      raise
        (Invalid
           (List.map
              (fun (e : Sequence.error) ->
                Common.Diagnostic.make ~code:e.code
                  ~severity:Common.Diagnostic.Error ~message:e.message
                  ~primary:(Option.value e.span ~default:span)
                  ())
              errors))
    in
    let rec direct_call_in frame ~instruction_id ~value_id value =
      let lowered =
        match
          List.find_opt
            (fun target ->
              let call =
                Sema.Top_level_function_call_target_classification.source target
              in
              Typed.Id.equal
                (Typed.top_level_direct_result_id call)
                (Typed.result_id value))
            (if Option.is_some frame then [] else top_calls)
        with
        | Some target ->
            Direct_call_lowering.lower_top_level ?frame ?globals
              ~lower_call:(direct_call_in frame) ~instruction_id ~value_id
              ~target value
        | None -> (
            match
              List.find_opt
                (fun target ->
                  match Typed.result_call_resolution value with
                  | Some (Source.Direct_call call) ->
                      call
                      == (target
                        |> Sema.Function_call_target_classification.source
                        |> Typed.direct_source
                        |> Sema.Function_call_conversion_policy.direct_source)
                  | _ -> false)
                function_calls
            with
            | Some target ->
                Direct_call_lowering.lower ?frame ?globals
                  ~lower_call:(direct_call_in frame) ~instruction_id ~value_id
                  ~target value
            | None -> Ok Direct_call_lowering.Unsupported_call)
      in
      Result.map
        (function
          | Direct_call_lowering.Unsupported_call -> None
          | Direct_call_lowering.Lowered result ->
              runtime_calls :=
                Direct_call_lowering.runtime_call result :: !runtime_calls;
              Some (Direct_call_lowering.sequence result))
        lowered
    in
    let direct_call = direct_call_in frame in
    let output_statement ~at lower =
      let records =
        match records with
        | Some records -> records
        | None ->
            fail at "HCRUN0004"
              "implicit output requires checked declaration records"
      in
      let instruction_id =
        Sequence.Instruction_id.of_int !instruction_count |> checked_id
      in
      let value_id = Sequence.Value_id.of_int !value_count |> checked_id in
      match lower ~records ~instruction_id ~value_id with
      | Error errors -> lower_errors errors
      | Ok Direct_call_lowering.Unsupported_call ->
          fail at "HCRUN0003" "implicit output is outside checked call lowering"
      | Ok (Direct_call_lowering.Lowered result) ->
          let operand =
            append_fragment
              (Direct_call_lowering.sequence result)
              (Direct_call_lowering.next_instruction_id result)
              (Direct_call_lowering.next_value_id result)
              (Direct_call_lowering.result_value result)
          in
          let discard =
            Sequence.Instruction_id.of_int !instruction_count |> checked_id
          in
          instruction ~at ~operands:[ operand ] ~flags:0x200L Opcode.Ic_end_exp;
          let call = Direct_call_lowering.runtime_call result in
          runtime_calls :=
            { call with Runtime_call_context.discard = Some discard }
            :: !runtime_calls
    in
    let expression value =
      let instruction_id =
        Sequence.Instruction_id.of_int !instruction_count |> checked_id
      in
      let value_id = Sequence.Value_id.of_int !value_count |> checked_id in
      match
        Expression_lowering.lower_typed_result ?frame ?globals
          ~lower_call:direct_call ~instruction_id ~value_id value
      with
      | Error errors -> lower_errors errors
      | Ok Expression_lowering.Unsupported_expression ->
          fail
            (span_of_result span value)
            "HCRUN0003" "expression is outside integer program lowering"
      | Ok (Expression_lowering.Lowered result) -> append_expression result
    in
    let rec condition value ~yes ~no =
      validate_expression value;
      let at = span_of_result span value in
      let ordinary () =
        let operand = expression value in
        instruction ~at ~operands:[ operand ] ~payload:(Sequence.Block no)
          Opcode.Ic_br_zero;
        finish ();
        start (block ());
        jump ~at yes
      in
      if
        Typed.result_intrinsic_conversion value <> Typed.No_intrinsic_conversion
      then ordinary ()
      else
        match Source.argument_expression_kind (Typed.result_source value) with
        | Source.Parenthesized_expression _ -> (
            match Typed.result_operand value with
            | Some operand -> condition operand ~yes ~no
            | None -> ordinary ())
        | Source.Prefix_expression prefix -> (
            match
              (Source.prefix_operator prefix, Typed.result_operand value)
            with
            | Source.Logical_not, Some operand ->
                condition operand ~yes:no ~no:yes
            | Source.Unary_plus, Some operand -> condition operand ~yes ~no
            | _ -> ordinary ())
        | Source.Binary_expression binary -> (
            match
              (Source.binary_operator binary, Typed.result_binary_operands value)
            with
            | Opcode.Ic_and_and, Some (left, right) ->
                let rhs = block () in
                condition left ~yes:rhs ~no;
                start rhs;
                condition right ~yes ~no
            | Opcode.Ic_or_or, Some (left, right) ->
                let rhs = block () in
                condition left ~yes ~no:rhs;
                start rhs;
                condition right ~yes ~no
            | _ -> ordinary ())
        | _ -> ordinary ()
    in
    let rec statement break_target = function
      | Empty _ -> ()
      | Function_output output ->
          let origin =
            output |> Sema.Implicit_output_argument_binding.bound_source
            |> Sema.Implicit_output_target_resolution.output_source
            |> Typed.implicit_output_source |> Source.implicit_output_origin
          in
          let at =
            match origin with
            | Sema.Symbol.Source_location location -> location.span
            | _ -> span
          in
          output_statement ~at (fun ~records ~instruction_id ~value_id ->
              Direct_call_lowering.lower_implicit_output ?frame ?globals
                ~lower_call:direct_call ~records ~instruction_id ~value_id
                output)
      | Top_level_output output ->
          let origin =
            output
            |> Sema.Top_level_implicit_output_argument_binding.bound_source
            |> Sema.Top_level_implicit_output_target_resolution
               .output_marker_origin
          in
          let at =
            match origin with
            | Sema.Symbol.Source_location location -> location.span
            | _ -> span
          in
          output_statement ~at (fun ~records ~instruction_id ~value_id ->
              Direct_call_lowering.lower_top_level_implicit_output ?frame
                ?globals ~lower_call:direct_call ~records ~instruction_id
                ~value_id output)
      | Initialize_global root -> (
          let at = span_of_result span (Typed.top_level_root_value root) in
          match (globals, frame) with
          | Some globals, None -> (
              let first =
                Sequence.Instruction_id.of_int !instruction_count |> checked_id
              in
              match
                Expression_lowering.lower_global_initializer ~globals
                  ~lower_call:direct_call ~instruction_id:first
                  ~value_id:(Sequence.Value_id.of_int !value_count |> checked_id)
                  root
              with
              | Error errors -> lower_errors errors
              | Ok Expression_lowering.Unsupported_expression ->
                  fail at "HCRUN0003"
                    "global initializer is outside integer program lowering"
              | Ok (Expression_lowering.Lowered result) ->
                  let operand = append_expression result in
                  let last =
                    Sequence.Instruction_id.of_int !instruction_count
                    |> checked_id
                  in
                  instruction ~at ~operands:[ operand ] ~flags:0x200L
                    Opcode.Ic_end_exp;
                  initial_regions :=
                    { Global_initialization.root; first; last }
                    :: !initial_regions)
          | _ ->
              fail at "HCRUN0004"
                "global initializer requires program storage and a module entry"
          )
      | Initialize_fragment destination -> (
          let module Destination = Initializer_fragment_destination in
          let at = Destination.span destination in
          match (globals, frame) with
          | Some globals, None when globals == Destination.globals destination
            -> (
              let first =
                Sequence.Instruction_id.of_int !instruction_count |> checked_id
              in
              match
                Expression_lowering.lower_fragment_initializer
                  ~lower_call:direct_call ~instruction_id:first
                  ~value_id:(Sequence.Value_id.of_int !value_count |> checked_id)
                  destination
              with
              | Error errors -> lower_errors errors
              | Ok Expression_lowering.Unsupported_expression ->
                  fail at "HCRUN0003"
                    "initializer fragment is outside integer program lowering"
              | Ok (Expression_lowering.Lowered result) ->
                  let operand = append_expression result in
                  let last =
                    Sequence.Instruction_id.of_int !instruction_count
                    |> checked_id
                  in
                  instruction ~at ~operands:[ operand ] ~flags:0x200L
                    Opcode.Ic_end_exp;
                  initial_regions :=
                    {
                      Global_initialization.root = Destination.root destination;
                      first;
                      last;
                    }
                    :: !initial_regions)
          | _ ->
              fail at "HCRUN0004"
                "initializer fragment requires its exact retained module \
                 storage")
      | Publish_array prepared_root -> (
          match (globals, frame) with
          | Some _, None ->
              let before =
                Sequence.Instruction_id.of_int !instruction_count |> checked_id
              in
              publications :=
                { Global_initialization.prepared_root; before } :: !publications
          | _ ->
              fail span "HCRUN0004"
                "array publication requires a module entry and exact storage")
      | Initialize_static slot -> (
          match Integer_globals.static_initializer slot with
          | Some root ->
              statement break_target (Initialize_static_leaf (slot, root))
          | None ->
              fail span "HCRUN0004"
                "static initializer has no scalar declaration root")
      | Initialize_static_leaf (slot, static_root) -> (
          match (globals, frame) with
          | Some globals, None -> (
              let at =
                span_of_result span (Typed.initializer_value static_root)
              in
              let first =
                Sequence.Instruction_id.of_int !instruction_count |> checked_id
              in
              match
                Expression_lowering.lower_static_initializer ~globals
                  ~root:static_root
                  ~lower_call:
                    (direct_call_in (Some (Integer_globals.static_frame slot)))
                  ~instruction_id:first
                  ~value_id:(Sequence.Value_id.of_int !value_count |> checked_id)
                  slot
              with
              | Error errors -> lower_errors errors
              | Ok Expression_lowering.Unsupported_expression ->
                  fail at "HCRUN0003"
                    "static initializer is outside integer program lowering"
              | Ok (Expression_lowering.Lowered result) ->
                  let operand = append_expression result in
                  let last =
                    Sequence.Instruction_id.of_int !instruction_count
                    |> checked_id
                  in
                  instruction ~at ~operands:[ operand ] ~flags:0x200L
                    Opcode.Ic_end_exp;
                  static_regions :=
                    {
                      Global_initialization.static_root;
                      static_slot = slot;
                      first;
                      last;
                    }
                    :: !static_regions)
          | _ ->
              fail span "HCRUN0004"
                "static initializer requires its exact storage root and module \
                 entry")
      | Initialize initial -> (
          let value = Typed.initializer_value initial in
          let at = span_of_result span value in
          match frame with
          | None ->
              fail at "HCRUN0001" "local initializer has no function frame"
          | Some frame -> (
              match
                Expression_lowering.lower_initializer ~frame ?globals
                  ~lower_call:direct_call
                  ~instruction_id:
                    (Sequence.Instruction_id.of_int !instruction_count
                    |> checked_id)
                  ~value_id:(Sequence.Value_id.of_int !value_count |> checked_id)
                  initial
              with
              | Error errors -> lower_errors errors
              | Ok Expression_lowering.Unsupported_expression ->
                  fail at "HCRUN0003"
                    "initializer is outside integer function lowering"
              | Ok (Expression_lowering.Lowered result) ->
                  let operand = append_expression result in
                  instruction ~at ~operands:[ operand ] ~flags:0x200L
                    Opcode.Ic_end_exp))
      | Return returned -> (
          match leave with
          | None -> fail span "HCRUN0001" "return has no named function body"
          | Some leave -> (
              match
                Return_lowering.lower_function_return ?frame ?globals
                  ~lower_call:direct_call
                  ~instruction_id:
                    (Sequence.Instruction_id.of_int !instruction_count
                    |> checked_id)
                  ~value_id:(Sequence.Value_id.of_int !value_count |> checked_id)
                  ~leave returned
              with
              | Error errors -> lower_errors errors
              | Ok Return_lowering.Unsupported_expression ->
                  fail span "HCRUN0003"
                    "return expression is outside integer function lowering"
              | Ok (Return_lowering.Lowered result) ->
                  ignore
                    (append_fragment
                       (Return_lowering.sequence result)
                       (Return_lowering.next_instruction_id result)
                       (Return_lowering.next_value_id result)
                       ());
                  finish ()))
      | Expression value ->
          let operand = expression value in
          instruction
            ~at:(span_of_result span value)
            ~operands:[ operand ] ~flags:0x200L Opcode.Ic_end_exp
      | Block body -> List.iter (statement break_target) body
      | Break at -> (
          match break_target with
          | Some target -> jump ~at target
          | None -> fail at "HCRUN0002" "break has no enclosing loop target")
      | If (value, then_branch, else_branch) ->
          let at = span_of_result span value in
          let yes = block () in
          let no = block () in
          let done_ = block () in
          condition value ~yes ~no;
          start yes;
          statement break_target then_branch;
          jump ~at done_;
          start no;
          Option.iter (statement break_target) else_branch;
          jump ~at done_;
          start done_
      | While (value, body) ->
          let at = span_of_result span value in
          let test = block () in
          let yes = block () in
          let done_ = block () in
          jump ~at test;
          start test;
          condition value ~yes ~no:done_;
          start yes;
          statement (Some done_) body;
          jump ~at test;
          start done_
      | Do_while (body, value) ->
          let at = span_of_result span value in
          let body_id = block () in
          let done_ = block () in
          jump ~at body_id;
          start body_id;
          statement (Some done_) body;
          condition value ~yes:body_id ~no:done_;
          start done_
      | For (initial, value, update, body) ->
          statement None initial;
          let at = span_of_result span value in
          let test = block () in
          let yes = block () in
          let done_ = block () in
          jump ~at test;
          start test;
          condition value ~yes ~no:done_;
          start yes;
          statement (Some done_) body;
          Option.iter (statement None) update;
          jump ~at test;
          start done_
    in
    List.iter (statement None) statements;
    (match leave with
    | None -> instruction ~at:span Opcode.Ic_end
    | Some leave ->
        jump ~at:span leave;
        start leave;
        instruction ~at:span Opcode.Ic_ret);
    finish ();
    let graph =
      match Block_graph.create ~entry (List.rev !blocks) with
      | Ok graph -> graph
      | Error errors ->
          raise
            (Invalid
               (List.map
                  (fun (e : Block_graph.error) ->
                    Common.Diagnostic.make ~code:e.code
                      ~severity:Common.Diagnostic.Error ~message:e.message
                      ~primary:(Option.value e.span ~default:span)
                      ())
                  errors))
    in
    X87_stack.verify graph
    |> Result.map (fun graph ->
        let publications_ = List.rev !publications in
        let publication_evidence_ =
          match (globals, publications_) with
          | Some globals, _ :: _ ->
              Some
                (Initializer_publication.create ~globals ~entry:graph
                   publications_)
          | _ -> None
        in
        {
          graph_ = graph;
          expression_source_ =
            (match (frame, globals, statements) with
            | None, Some globals, [ Expression value ] -> Some (globals, value)
            | _ -> None);
          initializer_regions_ = List.rev !initial_regions;
          static_initializer_regions_ = List.rev !static_regions;
          runtime_calls_ = List.rev !runtime_calls;
          publications_;
          publication_evidence_;
        })
    |> Result.map_error
         (List.map (fun (e : X87_stack.error) ->
              Common.Diagnostic.make ~code:e.code
                ~severity:Common.Diagnostic.Error ~message:e.message
                ~primary:(Option.value e.span ~default:span)
                ()))
  with Invalid diagnostics -> Error diagnostics

let lower_with_storage_initializers ?frame ?globals ?top_calls ?function_calls
    ~span statements =
  match
    lower_complete ?frame ?globals ?top_calls ?function_calls ~span statements
  with
  | Error _ as error -> error
  | Ok result ->
      if result.publications_ <> [] then
        Error
          [
            Common.Diagnostic.make ~code:"HCRUN0004"
              ~severity:Common.Diagnostic.Error
              ~message:
                "prepared array publications require complete initialization \
                 lowering"
              ~primary:span ();
          ]
      else if
        List.exists
          (fun (call : Runtime_call_context.description) ->
            Option.is_some call.discard)
          result.runtime_calls_
      then
        Error
          [
            Common.Diagnostic.make ~code:"HCRUN0004"
              ~severity:Common.Diagnostic.Error ~primary:span
              ~message:
                "implicit output requires complete lowering and its checked \
                 call context"
              ();
          ]
      else
        Ok
          ( result.graph_,
            result.initializer_regions_,
            result.static_initializer_regions_ )

let lower_with_initializers ?frame ?globals ?top_calls ?function_calls ~span
    statements =
  match
    lower_with_storage_initializers ?frame ?globals ?top_calls ?function_calls
      ~span statements
  with
  | Ok (graph, regions, []) -> Ok (graph, regions)
  | Error _ as error -> error
  | Ok (_, _, _ :: _) ->
      Error
        [
          Common.Diagnostic.make ~code:"HCRUN0004"
            ~severity:Common.Diagnostic.Error ~primary:span
            ~message:
              "static initializer regions require complete storage \
               initialization lowering"
            ();
        ]

let lower ?frame ?globals ?top_calls ?function_calls ~span statements =
  match
    lower_with_initializers ?frame ?globals ?top_calls ?function_calls ~span
      statements
  with
  | Ok (graph, []) -> Ok graph
  | Ok (_, _ :: _) ->
      Error
        [
          Common.Diagnostic.make ~code:"HCRUN0004"
            ~severity:Common.Diagnostic.Error
            ~message:
              "global initializer regions require the compiled-program API"
            ~primary:span ();
        ]
  | Error errors -> Error errors
