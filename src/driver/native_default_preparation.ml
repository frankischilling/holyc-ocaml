module VM = Ir.Integer_interpreter
module Ast = Frontend.Ast
module Parser = Frontend.Parser

type completion = { execution : Ir.Default_fragment_program.execution }

type t = {
  compilation_mode : Frontend.Preprocessor.compilation_mode;
  table : Sema.Symbol_table.t;
  state : VM.task_state;
  max_default_bytes : int;
  mutable saved_bytes : int;
  mutable ledger : Task_declarations.t option;
  mutable completed_rev : completion list;
}

let ( let* ) = Result.bind
let work value = VM.task_initializer_steps value.state
let bytes value = value.saved_bytes
let completions value = List.rev value.completed_rev
let execution completion = completion.execution

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
