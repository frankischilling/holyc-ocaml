module VM = Ir.Integer_interpreter
module Ast = Frontend.Ast
module Parser = Frontend.Parser

type completion = { execution : Ir.Default_fragment_program.execution }

type initializer_completion = {
  preparation : Integer_initializers.native_preparation;
}

type static_completion = {
  static_preparation : Integer_initializers.native_static_preparation;
}

type t = {
  compilation_mode : Frontend.Preprocessor.compilation_mode;
  table : Sema.Symbol_table.t;
  state : VM.task_state;
  max_default_bytes : int;
  mutable saved_bytes : int;
  mutable ledger : Task_declarations.t option;
  mutable completed_rev : completion list;
  mutable initializer_attempts : Parser.completed_initializer_leaf list;
  mutable statics_rev : static_completion list;
  mutable initializers_rev : initializer_completion list;
}

let ( let* ) = Result.bind
let work value = VM.task_initializer_steps value.state
let bytes value = value.saved_bytes
let completions value = List.rev value.completed_rev
let execution completion = completion.execution
let initializer_completions value = List.rev value.initializers_rev
let initializer_preparation completion = completion.preparation

let initializers value =
  List.map initializer_preparation (initializer_completions value)

let create ~compilation_mode ~max_initializer_steps
    ?(max_default_bytes = 65_536) session =
  if max_initializer_steps <= 0 || max_default_bytes <= 0 then
    Error
      "max_initializer_steps and max_default_bytes must be greater than zero"
  else
    let table = Session.semantic_symbols session in
    let* state = VM.create_task_state ~max_initializer_steps ~table () in
    Ok
      {
        compilation_mode;
        table;
        state;
        max_default_bytes;
        saved_bytes = 0;
        ledger = None;
        completed_rev = [];
        initializer_attempts = [];
        initializers_rev = [];
        statics_rev = [];
      }

let scalar_integer primitive =
  let info = Common.Primitive_type.info primitive in
  info.category = Common.Primitive_type.Integer && info.byte_size > 0

let scalar_word_type = function
  | Ast.Primitive_type_specifier primitive -> scalar_integer primitive.primitive
  | Ast.Internal_type_specifier primitive -> scalar_integer primitive.primitive
  | _ -> false

let prepare value ~session ~ledger receipt =
  let span = receipt.Parser.default_ast.location.span in
  let fail code message =
    Error [ Integer_source.diagnostic ~span code message ]
  in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
  in
  let* () =
    if
      Session.semantic_symbols session != value.table
      || Option.fold ~none:false
           ~some:(fun prior -> prior != ledger)
           value.ledger
      || Parser.context_mode
           receipt.default_function.function_header.declaration_command
             .command_context
         <> value.compilation_mode
    then
      fail "HCRUN0004"
        "native default preparation has another source owner or mode"
    else if
      (not (scalar_word_type receipt.default_type_specifier))
      || receipt.default_pointer_layers <> []
      || Option.is_some receipt.default_function_pointer
      || receipt.default_register_qualifiers <> []
    then
      fail "HCRUN0001"
        "native defaults require unqualified nonzero scalar integer parameters"
    else Ok ()
  in
  let* expression =
    match receipt.default_ast.value with
    | Ast.Expression_default expression -> Ok expression
    | Ast.Lastclass_default _ ->
        fail "HCRUN0006"
          "native lastclass defaults require owned type-name storage"
  in
  let* () =
    if Expression_facts.contains_string_literal expression then
      fail "HCRUN0006" "native string defaults require owned string preparation"
    else if value.max_default_bytes - value.saved_bytes < 8 then
      fail "HCIRVM0011" "native saved-default payload exceeds max_default_bytes"
    else Ok ()
  in
  let* authority =
    Task_declarations.begin_native_source_default ledger ~runtime:value.state
      receipt
  in
  value.ledger <- Some ledger;
  let fragment = Sema.Default_fragment.authorized_fragment authority in
  let create_context =
    match value.compilation_mode with
    | Frontend.Preprocessor.Jit -> Initializer_fragment_typing.create_context
    | Frontend.Preprocessor.Aot ->
        Initializer_fragment_typing.create_aot_context
  in
  let* context =
    create_context ~table:value.table
      ~parent:(Task_declarations.initializer_scope ledger)
    |> diagnose
  in
  let* typed =
    Initializer_fragment_typing.prepare_default context fragment |> diagnose
  in
  let* destination =
    Ir.Default_fragment_destination.create_native_source typed |> diagnose
  in
  let before = work value in
  let* classification, steps =
    Integer_initializers.prepare_default
      ~on_progress:(fun steps ->
        VM.record_task_preparation value.state ~before ~steps)
      ~max_steps:(VM.task_initializer_limit value.state - before)
      ~top_calls:[] destination
  in
  let* bits =
    match classification with
    | Integer_initializers.Prepared_constant bits -> Ok bits
    | Scheduled ->
        fail "HCRUN0006" "native defaults require checked constant preparation"
  in
  let* execution =
    Ir.Default_fragment_program.prepare ~authority ~destination
      ~code:(Ir.Default_fragment_program.Prepared bits) ~steps
    |> diagnose
  in
  let* () = Task_declarations.finish_native_source_default ledger execution in
  value.saved_bytes <- value.saved_bytes + 8;
  value.completed_rev <- { execution } :: value.completed_rev;
  Ok ()

let prepare_initializer value ~session ~ledger receipt =
  let span = receipt.Parser.leaf_initializer.initializer_equals.span in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
  in
  let* () =
    if
      Session.semantic_symbols session != value.table
      || Option.fold ~none:false
           ~some:(fun prior -> prior != ledger)
           value.ledger
      || Parser.context_mode
           receipt.leaf_initializer.initializer_owner.global_header
             .declaration_command
             .command_context
         <> value.compilation_mode
      || List.exists (( == ) receipt) value.initializer_attempts
    then
      diagnose
        (Error
           "HCRUN0004: native initializer has another owner or was already \
            attempted")
    else Ok ()
  in
  let* authority =
    Task_declarations.native_initializer_fragment ledger ~runtime:value.state
      receipt
  in
  let fragment = Sema.Initializer_fragment.authorized_fragment authority in
  let expression =
    fragment |> Sema.Initializer_fragment.leaf
    |> Sema.Initializer_source.leaf_expression_ast
  in
  let* () =
    if Expression_facts.contains_string_literal expression then
      diagnose
        (Error
           "HCRUN0006: native initializers do not admit string-backed values")
    else Ok ()
  in
  value.ledger <- Some ledger;
  value.initializer_attempts <- receipt :: value.initializer_attempts;
  let create_context =
    match value.compilation_mode with
    | Frontend.Preprocessor.Jit -> Initializer_fragment_typing.create_context
    | Frontend.Preprocessor.Aot ->
        Initializer_fragment_typing.create_aot_context
  in
  let* context =
    create_context ~table:value.table
      ~parent:(Task_declarations.initializer_scope ledger)
    |> diagnose
  in
  let* typed =
    Initializer_fragment_typing.prepare context fragment |> diagnose
  in
  let before = work value in
  let* prepared =
    Integer_initializers.prepare_native ~authority ~typed
      ~on_progress:(fun steps ->
        VM.record_task_preparation value.state ~before ~steps)
      ~max_steps:(VM.task_initializer_limit value.state - before)
  in
  value.initializers_rev <- { preparation = prepared } :: value.initializers_rev;
  Ok ()

let static_completions value = List.rev value.statics_rev
let static_preparation completion = completion.static_preparation

let static_initializers value =
  List.map static_preparation (static_completions value)

let prepare_static value ~session ~ledger receipt =
  let span =
    receipt.Parser.static_initializer.local_initializer_location.span
  in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
  in
  let* () =
    if
      Session.semantic_symbols session != value.table
      || Option.fold ~none:false
           ~some:(fun prior -> prior != ledger)
           value.ledger
      || Parser.context_mode
           receipt.static_allocation.allocation_function.function_header
             .declaration_command
             .command_context
         <> value.compilation_mode
    then
      diagnose
        (Error
           "HCRUN0004: native static initializer has another source owner or \
            mode")
    else Ok ()
  in
  let* fragment =
    Task_declarations.native_static_initializer_fragment ledger
      ~runtime:value.state receipt
  in
  let* () =
    if
      Expression_facts.contains_string_literal
        (Sema.Static_initializer_fragment.expression fragment)
    then
      diagnose
        (Error
           "HCRUN0006: native static initializers do not admit string-backed \
            values")
    else Ok ()
  in
  value.ledger <- Some ledger;
  let create_context =
    match value.compilation_mode with
    | Frontend.Preprocessor.Jit -> Initializer_fragment_typing.create_context
    | Frontend.Preprocessor.Aot ->
        Initializer_fragment_typing.create_aot_context
  in
  let* context =
    create_context ~table:value.table
      ~parent:(Task_declarations.initializer_scope ledger)
    |> diagnose
  in
  let* typed =
    Initializer_fragment_typing.prepare_static context fragment |> diagnose
  in
  let before = work value in
  let* static_preparation =
    Integer_initializers.prepare_native_static ~fragment ~typed
      ~on_progress:(fun steps ->
        VM.record_task_preparation value.state ~before ~steps)
      ~max_steps:(VM.task_initializer_limit value.state - before)
  in
  value.statics_rev <- { static_preparation } :: value.statics_rev;
  Ok ()
