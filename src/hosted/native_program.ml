open Driver
module Ast = Frontend.Ast
module Image = Backend.X86_64_program
module Native = Runtime.Native_program_execution

type 'a checked = { value : 'a; diagnostics : Common.Diagnostic.t list }

type result = {
  image : Image.t;
  execution : Image.execution;
  platform : Native.platform;
}

type report = {
  outcome_ : (result checked, Common.Diagnostic.t list) Stdlib.result;
  image_ : Image.t option;
  native_outcome_ : Image.outcome option;
  platform_ : Native.platform;
  executed_steps_ : int option;
  preparation_steps_ : int;
  switch_work_ : int;
  default_bytes_ : int;
}

let ( let* ) = Result.bind

let diagnostic ~span code message =
  Common.Diagnostic.make ~code ~severity:Common.Diagnostic.Error ~message
    ~primary:span ()

let image_errors ~fallback errors =
  List.map
    (fun (error : Image.error) ->
      diagnostic
        ~span:(Option.value error.span ~default:fallback)
        error.code error.message)
    errors

let validate_limits ~span ~max_ir_instructions ~max_code_bytes ~max_stack_bytes
    ~max_blocks ~max_global_bytes =
  let errors = image_errors ~fallback:span in
  let* () =
    Image.validate_limits ~max_ir_instructions ~max_code_bytes
    |> Result.map_error errors
  in
  let* () =
    Image.validate_stack_limit ~max_stack_bytes |> Result.map_error errors
  in
  let* () = Image.validate_block_limit ~max_blocks |> Result.map_error errors in
  Image.validate_global_limit ~max_global_bytes |> Result.map_error errors

let source_error span message = diagnostic ~span "HCRUN0001" message

let item_span = function
  | Ast.Aggregate_forward_declaration item -> item.location.span
  | Ast.Aggregate_definition item -> item.location.span
  | Ast.Global_variable item -> item.location.span
  | Ast.Global_declaration item -> item.location.span
  | Ast.Function_prototype item -> item.location.span
  | Ast.Function_definition item -> item.location.span
  | Ast.Top_level_statement statement -> (Ast.statement_location statement).span

type gate_node =
  | Gate_item of Ast.item
  | Gate_statement of bool * Ast.statement
  | Gate_expression of bool * Ast.expression

let prepend_statements in_function statements rest =
  List.rev_append
    (List.rev_map
       (fun statement -> Gate_statement (in_function, statement))
       statements)
    rest

let scalar_integer primitive =
  let info = Common.Primitive_type.info primitive in
  info.category = Common.Primitive_type.Integer && info.byte_size > 0

let scalar_word_type = function
  | Ast.Primitive_type_specifier primitive -> scalar_integer primitive.primitive
  | Ast.Internal_type_specifier primitive -> scalar_integer primitive.primitive
  | Ast.Named_type_specifier _ -> false

let void_return_type = function
  | Ast.Primitive_type_specifier { primitive = U0; _ }
  | Ast.Internal_type_specifier { primitive = U0; _ } -> true
  | _ -> false

let function_source_error (definition : Ast.function_definition) =
  let reject message = Some (source_error definition.location.span message) in
  if
    not
      (scalar_word_type definition.return_type
      || void_return_type definition.return_type)
  then reject "native functions require a scalar integer or U0 return type"
  else if definition.return_pointer_layers <> [] then
    reject "native functions do not admit pointer returns"
  else if definition.modifiers <> [] then
    reject "native functions do not admit explicit declaration modifiers"
  else if Option.is_some definition.variadic then
    reject "native functions require fixed parameters without a variadic tail"
  else if Option.is_none definition.body then
    reject "native functions require their original source definition body"
  else
    List.find_map
      (fun (parameter : Ast.function_parameter) ->
        let reject message =
          Some (source_error parameter.location.span message)
        in
        if not (scalar_word_type parameter.type_specifier) then
          reject
            "native function parameters require nonzero scalar integer types"
        else if
          parameter.pointer_layers <> []
          || Option.is_some parameter.function_pointer
        then reject "native functions do not admit pointer parameters"
        else if parameter.register_qualifiers <> [] then
          reject "native functions do not admit explicit parameter registers"
        else if Option.is_none parameter.name then
          reject "native function definitions require named fixed parameters"
        else None)
      definition.parameters

let local_source_error (declaration : Ast.local_declaration) =
  let reject message =
    Some (source_error declaration.local_declaration_location.span message)
  in
  let is_static = declaration.local_storage = Ast.Static_local in
  if
    if is_static then
      declaration.local_modifiers = []
      || List.exists
           (fun (modifier : Ast.declaration_modifier) ->
             modifier.kind <> Ast.Static || modifier.spelling <> "static")
           declaration.local_modifiers
    else declaration.local_modifiers <> []
  then reject "native locals do not admit declaration modifiers"
  else if not (scalar_word_type declaration.local_type_specifier) then
    reject "native locals require nonzero scalar integer types"
  else
    List.find_map
      (fun (local : Ast.local_declarator) ->
        let reject message =
          Some (source_error local.local_declarator_location.span message)
        in
        if
          local.local_pointer_layers <> []
          || Option.is_some local.local_function_pointer
        then reject "native locals do not admit pointers"
        else if local.local_array_dimensions <> [] then
          reject "native locals do not admit arrays"
        else if local.local_register_qualifiers <> [] then
          reject "native locals do not admit explicit registers"
        else
          match local.local_initializer with
          | None -> None
          | Some _ when is_static ->
              reject "native statics do not admit declaration initializers"
          | Some { local_initializer_value = Ast.Scalar_initializer _; _ } ->
              None
          | Some _ -> reject "native locals require scalar initializers")
      declaration.local_declarators

let global_source_error ~span ~modifiers ~binding ~type_specifier
    ~pointer_layers ~function_pointer ~array_dimensions ~has_initializer:_ =
  let reject message = Some (source_error span message) in
  if modifiers <> [] || Option.is_some binding then
    reject
      "native globals require ordinary declarations without modifiers or \
       aliases"
  else if not (scalar_word_type type_specifier) then
    reject "native globals require nonzero scalar integer types"
  else if pointer_layers <> [] || Option.is_some function_pointer then
    reject "native globals do not admit pointer or callback storage"
  else if array_dimensions <> [] then
    reject "native globals do not admit arrays"
  else None

let ast_errors (ast : Ast.module_) =
  let first_error = ref None in
  let work =
    ref
      (List.rev_append (List.rev_map (fun item -> Gate_item item) ast.items) [])
  in
  let reject error =
    first_error := Some error;
    work := []
  in
  while !work <> [] do
    match !work with
    | [] -> assert false
    | node :: rest -> (
        work := rest;
        match node with
        | Gate_item (Ast.Top_level_statement statement) ->
            work := Gate_statement (false, statement) :: !work
        | Gate_item (Ast.Function_definition definition) -> (
            match function_source_error definition with
            | Some error -> reject error
            | None ->
                Option.iter
                  (fun body -> work := Gate_statement (true, body) :: !work)
                  definition.body)
        | Gate_item (Ast.Global_variable variable) ->
            Option.iter reject
              (global_source_error ~span:variable.location.span
                 ~modifiers:variable.modifiers ~binding:variable.binding
                 ~type_specifier:variable.type_specifier
                 ~pointer_layers:variable.pointer_layers ~function_pointer:None
                 ~array_dimensions:variable.array_dimensions
                 ~has_initializer:false)
        | Gate_item (Ast.Global_declaration declaration) ->
            List.find_map
              (fun (variable : Ast.global_declarator) ->
                global_source_error ~span:variable.location.span
                  ~modifiers:declaration.modifiers ~binding:declaration.binding
                  ~type_specifier:declaration.type_specifier
                  ~pointer_layers:variable.pointer_layers
                  ~function_pointer:variable.function_pointer
                  ~array_dimensions:variable.array_dimensions
                  ~has_initializer:
                    (Option.is_some variable.global_initial_value))
              declaration.declarators
            |> Option.iter reject
        | Gate_item item ->
            reject
              (source_error (item_span item)
                 "native programs admit only scalar globals, source function \
                  definitions and executable statements; other declarations \
                  require a later source gate")
        | Gate_expression (in_function, expression) -> (
            match expression with
            | Ast.Integer_literal _
            | Ast.Float_literal _
            | Ast.Character_literal _
            | Ast.Current_position_expression _
            | Ast.Sizeof_expression _
            | Ast.Offset_expression _
            | Ast.Defined_expression _ -> ()
            | Ast.String_literal literal ->
                reject
                  (source_error literal.literal_location.span
                     "native programs do not admit string-literal storage")
            | Ast.Identifier_expression _ -> ()
            | Ast.Parenthesized_expression grouped ->
                work :=
                  Gate_expression (in_function, grouped.grouped_expression)
                  :: !work
            | Ast.Prefix_expression prefix -> (
                match prefix.prefix_operator_kind with
                | Ast.Unary_plus
                | Ast.Unary_minus
                | Ast.Logical_not
                | Ast.Bitwise_not
                | Ast.Pre_increment
                | Ast.Pre_decrement ->
                    work :=
                      Gate_expression (in_function, prefix.prefix_operand)
                      :: !work
                | Ast.Dereference | Ast.Address_of ->
                    reject
                      (source_error prefix.prefix_location.span
                         "native programs do not admit pointer prefix \
                          expressions"))
            | Ast.Postfix_cast_expression cast ->
                work :=
                  Gate_expression (in_function, cast.cast_operand) :: !work
            | Ast.Binary_expression binary ->
                work :=
                  Gate_expression (in_function, binary.binary_left)
                  :: Gate_expression (in_function, binary.binary_right)
                  :: !work
            | Ast.Postfix_expression postfix ->
                work :=
                  Gate_expression (in_function, postfix.postfix_operand)
                  :: !work
            | Ast.Call_expression call -> (
                match call.call_callee with
                | Ast.Identifier_expression _ ->
                    work :=
                      List.rev_append
                        (List.rev_map
                           (fun argument ->
                             match argument.Ast.call_argument_value with
                             | Ast.Provided_call_argument expression ->
                                 Some
                                   (Gate_expression (in_function, expression))
                             | Ast.Omitted_call_argument -> None)
                           call.call_arguments
                        |> List.filter_map Fun.id)
                        !work
                | _ ->
                    reject
                      (source_error call.call_location.span
                         "native programs require direct calls to checked \
                          source-defined functions"))
            | Ast.Index_expression index ->
                reject
                  (source_error index.index_location.span
                     "native programs do not admit indexed storage")
            | Ast.Member_expression member ->
                reject
                  (source_error member.member_location.span
                     "native programs do not admit member storage"))
        | Gate_statement (in_function, statement) -> (
            match statement with
            | Ast.Empty_statement _ | Ast.Break_statement _ -> ()
            | Ast.Expression_statement statement ->
                work :=
                  Gate_expression
                    (in_function, statement.expression_statement_expression)
                  :: !work
            | Ast.Block_statement block ->
                work :=
                  prepend_statements in_function block.block_statements !work
            | Ast.Sequence_statement sequence ->
                work :=
                  List.rev_append
                    (List.rev_map
                       (fun element ->
                         Gate_statement
                           (in_function, element.Ast.sequence_statement))
                       sequence.sequence_elements)
                    !work
            | Ast.If_statement branch ->
                let rest =
                  match branch.if_else_clause with
                  | None ->
                      Gate_statement (in_function, branch.if_then_branch)
                      :: !work
                  | Some clause ->
                      Gate_statement (in_function, branch.if_then_branch)
                      :: Gate_statement (in_function, clause.Ast.else_branch)
                      :: !work
                in
                work :=
                  Gate_expression (in_function, branch.if_condition) :: rest
            | Ast.While_statement loop ->
                work :=
                  Gate_expression (in_function, loop.while_condition)
                  :: Gate_statement (in_function, loop.while_body)
                  :: !work
            | Ast.Do_while_statement loop ->
                work :=
                  Gate_statement (in_function, loop.do_body)
                  :: Gate_expression (in_function, loop.do_while_condition)
                  :: !work
            | Ast.For_statement loop ->
                let rest =
                  Gate_statement (in_function, loop.for_body) :: !work
                in
                let rest =
                  match loop.for_update with
                  | None -> rest
                  | Some update -> Gate_statement (in_function, update) :: rest
                in
                work :=
                  Gate_statement (in_function, loop.for_initializer)
                  :: Gate_expression (in_function, loop.for_condition)
                  :: rest
            | Ast.Switch_statement switch ->
                if switch.switch_mode <> Ast.Bounded_switch then
                  reject
                    (source_error switch.switch_location.span
                       "native programs do not admit no-bound switches")
                else
                  let body =
                    List.filter_map
                      (function
                        | Ast.Switch_statement_element statement ->
                            Some statement
                        | Ast.Switch_case_element _
                        | Ast.Switch_default_element _ -> None
                        | Ast.Switch_subswitch_element subswitch ->
                            reject
                              (source_error subswitch.subswitch_location.span
                                 "native programs do not admit sub-switch \
                                  regions");
                            None)
                      switch.switch_elements
                  in
                  work :=
                    Gate_expression (in_function, switch.switch_expression)
                    :: prepend_statements in_function body !work
            | Ast.Implicit_output_statement statement ->
                reject
                  (source_error statement.location.span
                     "native programs do not admit implicit runtime output")
            | Ast.Local_declaration_statement declaration when in_function -> (
                match local_source_error declaration with
                | Some error -> reject error
                | None ->
                    let initializers =
                      List.filter_map
                        (fun (local : Ast.local_declarator) ->
                          match local.local_initializer with
                          | Some
                              {
                                local_initializer_value =
                                  Ast.Scalar_initializer expression;
                                _;
                              } -> Some (Gate_expression (true, expression))
                          | _ -> None)
                        declaration.local_declarators
                    in
                    work := List.rev_append (List.rev initializers) !work)
            | Ast.Local_declaration_statement declaration ->
                reject
                  (source_error declaration.local_declaration_location.span
                     "native entry statements do not admit local storage \
                      declarations")
            | Ast.Return_statement returned when in_function -> (
                match returned.return_value with
                | Some expression ->
                    work := Gate_expression (true, expression) :: !work
                | None -> ())
            | Ast.Return_statement returned ->
                reject
                  (source_error returned.return_location.span
                     "native program entry source cannot contain return \
                      statements")
            | (Ast.Goto_statement _ | Ast.Label_statement _) when in_function ->
                ()
            | ( Ast.Assembly_block_statement _
              | Ast.Inline_assembly_statement _
              | Ast.Goto_statement _
              | Ast.Label_statement _
              | Ast.Lock_statement _
              | Ast.No_warn_statement _
              | Ast.Try_catch_statement _ ) as statement ->
                reject
                  (source_error (Ast.statement_location statement).span
                     "statement is outside the closed native program execution \
                      domain")))
  done;
  Option.to_list !first_error

let program_storage_errors compiled span =
  let errors = ref [] in
  let add message = errors := source_error span message :: !errors in
  let initialization = Integer_unit.initialization compiled in
  if
    Ir.Global_initialization.regions initialization <> []
    || Ir.Global_initialization.static_regions initialization <> []
    || Ir.Global_initialization.publications initialization <> []
  then add "native programs require an entry with no runtime initialization";
  if Integer_unit.dimension_preparation_work compiled <> 0 then
    add "native programs require zero dimension preparation work";
  List.rev !errors

let compile_with_preparation ?(max_ir_instructions = 4096)
    ?(max_code_bytes = 65536) ?(max_stack_bytes = Image.hard_max_stack_bytes)
    ?(max_blocks = 4096) ?(max_initializer_steps = 100_000)
    ?(max_switch_work = 100_000) ?(max_default_bytes = 65_536)
    ?(max_global_bytes = 1_048_576) ?status_abi ~preparation_steps ~switch_work
    ~default_bytes session ~config ~source =
  let span = Integer_source.source_span source in
  let* () =
    if max_initializer_steps > 0 && max_default_bytes > 0 && max_switch_work > 0
    then Ok ()
    else
      Error
        [
          diagnostic ~span "HCIRVM0001"
            "max_initializer_steps, max_switch_work and max_default_bytes must \
             be greater than zero";
        ]
  in
  let* () =
    validate_limits ~span ~max_ir_instructions ~max_code_bytes ~max_stack_bytes
      ~max_blocks ~max_global_bytes
  in
  let* ledger =
    Task_declarations.create_source ~max_offset_work:max_initializer_steps
      ~max_switch_work session ~source
    |> Result.map_error (fun message ->
        [ diagnostic ~span "HCRUN0004" message ])
  in
  let* preparation =
    Native_default_preparation.create
      ~compilation_mode:(Frontend.Preprocessor.Config.compilation_mode config)
      ~max_initializer_steps ~max_default_bytes session
    |> Result.map_error (fun message ->
        [ diagnostic ~span "HCIRVM0001" message ])
  in
  let entry_statement_seen = ref false in
  let commands : Frontend.Parser.command_sink =
    {
      checkpoint =
        Some
          (fun event ->
            let* () = Task_declarations.observe_command ledger event in
            (match event with
            | Frontend.Parser.Command_completed receipt ->
                if
                  List.exists
                    (function
                      | Ast.Top_level_statement (Ast.Empty_statement _) -> false
                      | Ast.Top_level_statement _ -> true
                      | _ -> false)
                    receipt.command_ast.items
                then entry_statement_seen := true
            | _ -> ());
            Ok ());
      query = Some (Task_declarations.observe_query ledger);
      call = None;
      implicit_output = Some (Task_declarations.observe_implicit_output ledger);
      reference = Some (Task_declarations.observe_reference ledger);
      declaration =
        Some
          (fun event ->
            let* () =
              match event with
              | Frontend.Parser.Parameter_default_completed receipt
                when !entry_statement_seen ->
                  Error
                    [
                      diagnostic ~span:receipt.default_ast.location.span
                        "HCRUN0006"
                        "native defaults must precede executable top-level \
                         statements; interleaved declaration execution is \
                         unsupported";
                    ]
              | Frontend.Parser.Array_dimension_preparing receipt ->
                  Error
                    [
                      diagnostic ~span:receipt.dimension_opening.span
                        "HCRUN0001" "native source does not admit array storage";
                    ]
              | Frontend.Parser.Global_declared publication -> (
                  match
                    global_source_error
                      ~span:publication.global_name.location.span
                      ~modifiers:publication.global_header.modifiers
                      ~binding:publication.global_header.binding
                      ~type_specifier:publication.global_header.type_specifier
                      ~pointer_layers:publication.global_pointer_layers
                      ~function_pointer:publication.global_function_pointer
                      ~array_dimensions:publication.global_dimensions
                      ~has_initializer:false
                  with
                  | None -> Ok ()
                  | Some error -> Error [ error ])
              | Frontend.Parser.Global_initializer_started receipt
                when !entry_statement_seen ->
                  Error
                    [
                      source_error receipt.initializer_equals.span
                        "native initializers must precede executable top-level \
                         statements";
                    ]
              | Frontend.Parser.Aggregate_declared _ ->
                  Error
                    [
                      diagnostic ~span "HCRUN0001"
                        "native source does not admit aggregate declarations";
                    ]
              | _ -> Ok ()
            in
            let* () = Task_declarations.observe ledger event in
            match event with
            | Frontend.Parser.Parameter_default_completed receipt ->
                Native_default_preparation.prepare preparation ~session ~ledger
                  receipt
            | Frontend.Parser.Global_initializer_leaf_completed receipt ->
                Native_default_preparation.prepare_initializer preparation
                  ~session ~ledger receipt
            | Frontend.Parser.Function_header_completed header ->
                Task_declarations.complete_source_defaults ledger header
            | _ -> Ok ());
      dimension_count = Some (Task_declarations.grammar_dimension_count ledger);
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let parsed =
    Frontend.Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  preparation_steps := Native_default_preparation.work preparation;
  switch_work := Task_declarations.switch_work ledger;
  default_bytes := Native_default_preparation.bytes preparation;
  match parsed.ast with
  | None -> Error parsed.diagnostics
  | Some ast -> (
      match ast_errors ast with
      | _ :: _ as errors -> Error (parsed.diagnostics @ errors)
      | [] -> (
          let* source_command =
            Task_declarations.seal_source ledger ast
            |> Result.map_error (fun errors -> parsed.diagnostics @ errors)
          in
          let* prepared_defaults =
            Task_declarations.native_source_defaults
              ~table:(Session.semantic_symbols session)
              ~ast source_command
            |> Result.map_error (fun errors -> parsed.diagnostics @ errors)
          in
          match
            Integer_unit.compile_source_output ~source_command
              ~native_initializers:
                (Native_default_preparation.initializers preparation)
              ~max_initializer_steps session ~config
              { parsed with diagnostics = [] }
          with
          | Error errors -> Error (parsed.diagnostics @ errors)
          | Ok checked -> (
              let diagnostics = parsed.diagnostics @ checked.diagnostics in
              match program_storage_errors checked.value span with
              | _ :: _ as errors -> Error (diagnostics @ errors)
              | [] ->
                  (match Integer_unit.functions checked.value with
                    | []
                      when Ir.Integer_globals.byte_size
                             (Integer_unit.globals checked.value)
                           = 0 ->
                        Image.compile ?status_abi ~max_stack_bytes ~max_blocks
                          ~max_ir_instructions ~max_code_bytes
                          (Integer_unit.entry checked.value)
                    | functions ->
                        let* parameter_defaults =
                          Native_parameter_defaults.create
                            ~globals:(Integer_unit.globals checked.value)
                            ~runtime_calls:
                              (Integer_unit.runtime_calls checked.value)
                            ~initialization:
                              (Integer_unit.initialization checked.value)
                            ~entry:(Integer_unit.entry checked.value)
                            ~functions ~prepared:prepared_defaults
                            ~completions:
                              (Native_default_preparation.completions
                                 preparation)
                          |> Result.map_error (fun message ->
                              [
                                Backend.X86_64_program.
                                  {
                                    code = "HCBACK0002";
                                    message;
                                    span = Some span;
                                  };
                              ])
                        in
                        let* global_initializers =
                          Native_global_initializers.create ~span
                            ~completions:
                              (Native_default_preparation
                               .initializer_completions preparation)
                            ~preparation:
                              (Integer_unit.initializer_preparation
                                 checked.value)
                            ~runtime_calls:
                              (Integer_unit.runtime_calls checked.value)
                            ~initialization:
                              (Integer_unit.initialization checked.value)
                            ~entry:(Integer_unit.entry checked.value)
                            ~functions
                          |> Result.map_error (fun message ->
                              [
                                Backend.X86_64_program.
                                  {
                                    code = "HCBACK0003";
                                    message;
                                    span = Some span;
                                  };
                              ])
                        in
                        Image.compile_callable ~parameter_defaults
                          ~global_initializers ?status_abi ~max_stack_bytes
                          ~max_blocks ~max_ir_instructions ~max_code_bytes
                          ~max_global_bytes
                          ~runtime_calls:
                            (Integer_unit.runtime_calls checked.value)
                          ~initialization:
                            (Integer_unit.initialization checked.value)
                          ~entry:(Integer_unit.entry checked.value)
                          ~functions ())
                  |> Result.map (fun image -> { value = image; diagnostics })
                  |> Result.map_error (fun errors ->
                      diagnostics @ image_errors ~fallback:span errors))))

let compile ?max_ir_instructions ?max_code_bytes ?max_stack_bytes ?max_blocks
    ?max_initializer_steps ?max_switch_work ?max_default_bytes ?max_global_bytes
    ?status_abi session ~config ~source =
  compile_with_preparation ?max_ir_instructions ?max_code_bytes ?max_stack_bytes
    ?max_blocks ?max_initializer_steps ?max_switch_work ?max_default_bytes
    ?max_global_bytes ?status_abi ~preparation_steps:(ref 0)
    ~switch_work:(ref 0) ~default_bytes:(ref 0) session ~config ~source

let fault_diagnostic ~fallback (fault : Image.fault) =
  let code, message =
    match fault.kind with
    | Image.Division_by_zero ->
        let opcode =
          match fault.operation with
          | Some Image.Divide -> "IC_DIV"
          | Some Image.Remainder -> "IC_MOD"
          | None -> "integer division"
        in
        ("HCIRVM0009", opcode ^ " divisor is zero")
    | Image.Signed_division_overflow ->
        let opcode =
          match fault.operation with
          | Some Image.Divide -> "IC_DIV"
          | Some Image.Remainder -> "IC_MOD"
          | None -> "integer division"
        in
        ("HCIRVM0010", opcode ^ " signed quotient overflows I64")
    | Image.Step_limit_exceeded ->
        ("HCIRVM0007", "the bounded integer execution step limit was exhausted")
    | Image.Call_depth_exceeded ->
        ("HCIRVM0015", "the runtime call depth limit was exhausted")
    | Image.Frame_limit_exceeded ->
        ( "HCIRVM0011",
          "the simultaneous parameter and local frame limit was exhausted" )
    | Image.Native_stack_limit_exceeded ->
        ( "HCNATIVE0006",
          "the simultaneous native stack byte limit was exhausted" )
    | Image.Uninitialized_read ->
        ("HCIRVM0012", "native execution read an uninitialized scalar object")
  in
  Common.Diagnostic.make ~code ~severity:Common.Diagnostic.Error ~message
    ~primary:(Option.value fault.span ~default:fallback)
    ~notes:
      ([
         "stage=execution";
         Printf.sprintf "executed_steps=%d" fault.executed_steps;
         Printf.sprintf "block_id=%d" fault.block_id;
         Printf.sprintf "instruction_id=%d" fault.instruction_id;
       ]
      @ Option.to_list
          (Option.map (Printf.sprintf "function_id=%d") fault.function_id)
      @ Option.to_list
          (Option.map (Printf.sprintf "function_name=%s") fault.function_name))
    ()

let host_diagnostic ~span platform message =
  diagnostic ~span
    (if platform = Native.Unsupported then "HCNATIVE0001" else "HCNATIVE0002")
    message

let evaluate ?max_ir_instructions ?max_code_bytes ?max_stack_bytes ?max_blocks
    ?(max_initializer_steps = 100_000) ?(max_default_bytes = 65_536)
    ?(max_switch_work = 100_000) ?(max_global_bytes = 1_048_576)
    ?(max_frame_bytes = 1_048_576) ?(max_call_depth = 128)
    ?(max_active_stack_bytes = Native.hard_max_active_stack_bytes) ?status_abi
    session ~config ~source ~max_steps =
  let span = Integer_source.source_span source in
  let platform = Native.platform () in
  if
    max_steps <= 0 || max_initializer_steps <= 0 || max_default_bytes <= 0
    || max_switch_work <= 0 || max_frame_bytes <= 0 || max_call_depth <= 0
    || max_active_stack_bytes <= 0
    || max_active_stack_bytes > Native.hard_max_active_stack_bytes
  then
    {
      outcome_ =
        Error
          [
            diagnostic ~span "HCIRVM0001"
              (Printf.sprintf
                 "max_steps, max_initializer_steps, max_switch_work, \
                  max_default_bytes, max_frame_bytes and max_call_depth must \
                  be greater than zero; max_active_stack_bytes must be between \
                  1 and %d"
                 Native.hard_max_active_stack_bytes);
          ];
      image_ = None;
      native_outcome_ = None;
      platform_ = platform;
      executed_steps_ = None;
      preparation_steps_ = 0;
      switch_work_ = 0;
      default_bytes_ = 0;
    }
  else
    let preparation_steps = ref 0 in
    let switch_work = ref 0 in
    let default_bytes = ref 0 in
    match
      compile_with_preparation ?max_ir_instructions ?max_code_bytes
        ?max_stack_bytes ?max_blocks ~max_initializer_steps ~max_switch_work
        ~max_default_bytes ~max_global_bytes ?status_abi ~preparation_steps
        ~switch_work ~default_bytes session ~config ~source
    with
    | Error diagnostics ->
        {
          outcome_ = Error diagnostics;
          image_ = None;
          native_outcome_ = None;
          platform_ = platform;
          executed_steps_ = None;
          preparation_steps_ = !preparation_steps;
          switch_work_ = !switch_work;
          default_bytes_ = !default_bytes;
        }
    | Ok checked -> (
        match
          Native.execute ~max_steps ~max_frame_bytes ~max_call_depth
            ~max_active_stack_bytes ~max_global_bytes checked.value
        with
        | Error message ->
            {
              outcome_ =
                Error
                  (checked.diagnostics
                  @ [ host_diagnostic ~span platform message ]);
              image_ = Some checked.value;
              native_outcome_ = None;
              platform_ = platform;
              executed_steps_ = None;
              preparation_steps_ = !preparation_steps;
              switch_work_ = !switch_work;
              default_bytes_ = !default_bytes;
            }
        | Ok (Image.Completed execution as native_outcome) ->
            {
              outcome_ =
                Ok
                  {
                    value = { image = checked.value; execution; platform };
                    diagnostics = checked.diagnostics;
                  };
              image_ = Some checked.value;
              native_outcome_ = Some native_outcome;
              platform_ = platform;
              executed_steps_ = Some execution.executed_steps;
              preparation_steps_ = !preparation_steps;
              switch_work_ = !switch_work;
              default_bytes_ = !default_bytes;
            }
        | Ok (Image.Fault fault as native_outcome) ->
            {
              outcome_ =
                Error
                  (checked.diagnostics
                  @ [ fault_diagnostic ~fallback:span fault ]);
              image_ = Some checked.value;
              native_outcome_ = Some native_outcome;
              platform_ = platform;
              executed_steps_ = Some fault.executed_steps;
              preparation_steps_ = !preparation_steps;
              switch_work_ = !switch_work;
              default_bytes_ = !default_bytes;
            })

let outcome report = report.outcome_
let image report = report.image_
let native_outcome report = report.native_outcome_
let platform report = report.platform_
let executed_steps report = report.executed_steps_
let preparation_steps report = report.preparation_steps_
let switch_work report = report.switch_work_
let default_bytes report = report.default_bytes_
