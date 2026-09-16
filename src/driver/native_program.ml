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
    ~max_blocks =
  let errors = image_errors ~fallback:span in
  let* () =
    Image.validate_limits ~max_ir_instructions ~max_code_bytes
    |> Result.map_error errors
  in
  let* () =
    Image.validate_stack_limit ~max_stack_bytes |> Result.map_error errors
  in
  Image.validate_block_limit ~max_blocks |> Result.map_error errors

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
  | Gate_statement of Ast.statement
  | Gate_expression of Ast.expression

let prepend_statements statements rest =
  List.rev_append
    (List.rev_map (fun statement -> Gate_statement statement) statements)
    rest

let storage_binary (operator : Frontend.Operator.binary_operator) =
  List.mem operator.ic_name
    [
      "IC_ASSIGN";
      "IC_SHL_EQU";
      "IC_SHR_EQU";
      "IC_MUL_EQU";
      "IC_DIV_EQU";
      "IC_MOD_EQU";
      "IC_AND_EQU";
      "IC_OR_EQU";
      "IC_XOR_EQU";
      "IC_ADD_EQU";
      "IC_SUB_EQU";
    ]

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
            work := Gate_statement statement :: !work
        | Gate_item item ->
            reject
              (source_error (item_span item)
                 "native programs do not admit declarations or function \
                  definitions")
        | Gate_expression expression -> (
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
            | Ast.Identifier_expression identifier ->
                reject
                  (source_error identifier.location.span
                     "native programs require closed expressions without \
                      identifier storage")
            | Ast.Parenthesized_expression grouped ->
                work := Gate_expression grouped.grouped_expression :: !work
            | Ast.Prefix_expression prefix -> (
                match prefix.prefix_operator_kind with
                | Ast.Unary_plus
                | Ast.Unary_minus
                | Ast.Logical_not
                | Ast.Bitwise_not ->
                    work := Gate_expression prefix.prefix_operand :: !work
                | Ast.Dereference
                | Ast.Address_of
                | Ast.Pre_increment
                | Ast.Pre_decrement ->
                    reject
                      (source_error prefix.prefix_location.span
                         "native programs do not admit pointer or storage \
                          prefix expressions"))
            | Ast.Postfix_cast_expression cast ->
                work := Gate_expression cast.cast_operand :: !work
            | Ast.Binary_expression binary ->
                if storage_binary binary.binary_operator_spec then
                  reject
                    (source_error binary.binary_location.span
                       "native programs do not admit assignment or compound \
                        storage expressions")
                else
                  work :=
                    Gate_expression binary.binary_left
                    :: Gate_expression binary.binary_right :: !work
            | Ast.Postfix_expression postfix ->
                reject
                  (source_error postfix.postfix_location.span
                     "native programs do not admit storage update expressions")
            | Ast.Call_expression call ->
                reject
                  (source_error call.call_location.span
                     "native programs do not admit function calls")
            | Ast.Index_expression index ->
                reject
                  (source_error index.index_location.span
                     "native programs do not admit indexed storage")
            | Ast.Member_expression member ->
                reject
                  (source_error member.member_location.span
                     "native programs do not admit member storage"))
        | Gate_statement statement -> (
            match statement with
            | Ast.Empty_statement _ | Ast.Break_statement _ -> ()
            | Ast.Expression_statement statement ->
                work :=
                  Gate_expression statement.expression_statement_expression
                  :: !work
            | Ast.Block_statement block ->
                work := prepend_statements block.block_statements !work
            | Ast.Sequence_statement sequence ->
                work :=
                  List.rev_append
                    (List.rev_map
                       (fun element ->
                         Gate_statement element.Ast.sequence_statement)
                       sequence.sequence_elements)
                    !work
            | Ast.If_statement branch ->
                let rest =
                  match branch.if_else_clause with
                  | None -> Gate_statement branch.if_then_branch :: !work
                  | Some clause ->
                      Gate_statement branch.if_then_branch
                      :: Gate_statement clause.Ast.else_branch :: !work
                in
                work := Gate_expression branch.if_condition :: rest
            | Ast.While_statement loop ->
                work :=
                  Gate_expression loop.while_condition
                  :: Gate_statement loop.while_body :: !work
            | Ast.Do_while_statement loop ->
                work :=
                  Gate_statement loop.do_body
                  :: Gate_expression loop.do_while_condition :: !work
            | Ast.For_statement loop ->
                let rest = Gate_statement loop.for_body :: !work in
                let rest =
                  match loop.for_update with
                  | None -> rest
                  | Some update -> Gate_statement update :: rest
                in
                work :=
                  Gate_statement loop.for_initializer
                  :: Gate_expression loop.for_condition :: rest
            | Ast.Implicit_output_statement statement ->
                reject
                  (source_error statement.location.span
                     "native programs do not admit implicit runtime output")
            | Ast.Local_declaration_statement declaration ->
                reject
                  (source_error declaration.local_declaration_location.span
                     "native programs do not admit local storage declarations")
            | Ast.Return_statement returned ->
                reject
                  (source_error returned.return_location.span
                     "native program entry source cannot contain return \
                      statements")
            | ( Ast.Assembly_block_statement _
              | Ast.Inline_assembly_statement _
              | Ast.Goto_statement _
              | Ast.Label_statement _
              | Ast.Lock_statement _
              | Ast.No_warn_statement _
              | Ast.Switch_statement _
              | Ast.Try_catch_statement _ ) as statement ->
                reject
                  (source_error (Ast.statement_location statement).span
                     "statement is outside the closed native program execution \
                      domain")))
  done;
  Option.to_list !first_error

let closed_program_errors compiled span =
  let errors = ref [] in
  let add message = errors := source_error span message :: !errors in
  if Integer_unit.functions compiled <> [] then
    add "native programs require an entry with no named functions";
  let globals = Integer_unit.globals compiled in
  if Ir.Integer_globals.byte_size globals <> 0 then
    add "native programs require an entry with no global or static storage";
  if Ir.Integer_globals.has_initializers globals then
    add
      "native programs require an entry with no global initializer preparation";
  if Integer_unit.has_entry_calls compiled then
    add "native programs require an entry with no runtime calls";
  let initialization = Integer_unit.initialization compiled in
  if
    Ir.Global_initialization.regions initialization <> []
    || Ir.Global_initialization.static_regions initialization <> []
    || Ir.Global_initialization.publications initialization <> []
  then add "native programs require an entry with no runtime initialization";
  if Ir.Global_initialization.prepared_steps initialization <> 0 then
    add "native programs require zero prepared initializer steps";
  if
    Integer_initializers.executed_steps
      (Integer_unit.initializer_preparation compiled)
    <> 0
  then add "native programs require zero initializer preparation work";
  if Integer_unit.dimension_preparation_work compiled <> 0 then
    add "native programs require zero dimension preparation work";
  List.rev !errors

let compile ?(max_ir_instructions = 4096) ?(max_code_bytes = 65536)
    ?(max_stack_bytes = Image.hard_max_stack_bytes) ?(max_blocks = 4096)
    ?status_abi session ~config ~source =
  let span = Integer_source.source_span source in
  let* () =
    validate_limits ~span ~max_ir_instructions ~max_code_bytes ~max_stack_bytes
      ~max_blocks
  in
  let parsed =
    Frontend.Parser.parse ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  match parsed.ast with
  | None -> Error parsed.diagnostics
  | Some ast -> (
      match ast_errors ast with
      | _ :: _ as errors -> Error (parsed.diagnostics @ errors)
      | [] -> (
          match Integer_unit.compile_ast session ~config ast with
          | Error errors -> Error (parsed.diagnostics @ errors)
          | Ok checked -> (
              let diagnostics = parsed.diagnostics @ checked.diagnostics in
              match closed_program_errors checked.value span with
              | _ :: _ as errors -> Error (diagnostics @ errors)
              | [] ->
                  Image.compile ?status_abi ~max_stack_bytes ~max_blocks
                    ~max_ir_instructions ~max_code_bytes
                    (Integer_unit.entry checked.value)
                  |> Result.map (fun image -> { value = image; diagnostics })
                  |> Result.map_error (fun errors ->
                      diagnostics @ image_errors ~fallback:span errors))))

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
  in
  Common.Diagnostic.make ~code ~severity:Common.Diagnostic.Error ~message
    ~primary:(Option.value fault.span ~default:fallback)
    ~notes:
      [
        "stage=execution";
        Printf.sprintf "executed_steps=%d" fault.executed_steps;
        Printf.sprintf "block_id=%d" fault.block_id;
        Printf.sprintf "instruction_id=%d" fault.instruction_id;
      ]
    ()

let host_diagnostic ~span platform message =
  diagnostic ~span
    (if platform = Native.Unsupported then "HCNATIVE0001" else "HCNATIVE0002")
    message

let evaluate ?max_ir_instructions ?max_code_bytes ?max_stack_bytes ?max_blocks
    ?status_abi session ~config ~source ~max_steps =
  let span = Integer_source.source_span source in
  let platform = Native.platform () in
  if max_steps <= 0 then
    {
      outcome_ =
        Error
          [
            diagnostic ~span "HCIRVM0001" "max_steps must be greater than zero";
          ];
      image_ = None;
      native_outcome_ = None;
      platform_ = platform;
      executed_steps_ = None;
    }
  else
    match
      compile ?max_ir_instructions ?max_code_bytes ?max_stack_bytes ?max_blocks
        ?status_abi session ~config ~source
    with
    | Error diagnostics ->
        {
          outcome_ = Error diagnostics;
          image_ = None;
          native_outcome_ = None;
          platform_ = platform;
          executed_steps_ = None;
        }
    | Ok checked -> (
        match Native.execute ~max_steps checked.value with
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
            })

let outcome report = report.outcome_
let image report = report.image_
let native_outcome report = report.native_outcome_
let platform report = report.platform_
let executed_steps report = report.executed_steps_
