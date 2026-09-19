type t = {
  globals : Ir.Integer_globals.t;
  initialization : Ir.Global_initialization.t;
  entry : Ir.X87_stack.t;
  runtime_calls : Ir.Runtime_call_context.t;
  functions : Ir.Function_body.t list;
}

let bodies functions =
  List.map
    (fun (definition : Ir.Integer_interpreter.function_definition) ->
      definition.body)
    functions

let matches_storage proof ~initialization ~entry =
  proof.initialization == initialization
  && proof.entry == entry
  && Ir.Global_initialization.globals initialization == proof.globals

let matches proof ~runtime_calls ~initialization ~entry ~functions =
  matches_storage proof ~initialization ~entry
  && proof.runtime_calls == runtime_calls
  && List.length proof.functions = List.length functions
  && List.for_all2 ( == ) proof.functions (bodies functions)

let create ~span ~completions ~preparation ~runtime_calls ~initialization ~entry
    ~functions =
  let globals = Integer_initializers.globals preparation in
  let functions = bodies functions in
  let evidence = Integer_initializers.native_evidence preparation in
  let completed =
    List.map Native_default_preparation.initializer_preparation completions
  in
  if
    (not (Integer_initializers.native_complete ~span preparation))
    || List.length evidence <> List.length completed
    || not (List.for_all2 ( == ) evidence completed)
  then
    Error "native global initializers lack their complete original preparation"
  else if
    (not (Ir.Global_initialization.matches initialization ~globals ~entry))
    || (not
          (Ir.Runtime_call_context.matches runtime_calls ~entry
             ~initialization:(Some initialization) ~functions))
    || Ir.Global_initialization.regions initialization <> []
    || Ir.Global_initialization.static_regions initialization <> []
    || Ir.Global_initialization.publications initialization <> []
    || Option.is_some
         (Ir.Global_initialization.publication_evidence initialization)
    || Ir.Integer_globals.statics globals <> []
    || Ir.Integer_globals.is_task_command globals
    || Ir.Global_initialization.prepared_steps initialization
       <> Integer_initializers.executed_steps preparation
  then
    Error
      "native global preparation has another initialization or callable bundle"
  else Ok { globals; initialization; entry; runtime_calls; functions }
