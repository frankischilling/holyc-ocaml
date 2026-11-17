type t = {
  globals : Ir.Integer_globals.t;
  initialization : Ir.Global_initialization.t;
  entry : Ir.X87_stack.t;
  runtime_calls : Ir.Runtime_call_context.t;
  functions : Ir.Function_body.t list;
  load_roots : Sema.Function_call_expression_result.top_level_root_result list;
  static_load_roots :
    (Ir.Integer_globals.static_slot
    * Sema.Function_call_expression_result.initializer_result)
    list;
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

let is_load_root proof slot root =
  List.exists (( == ) slot) (Ir.Integer_globals.slots proof.globals)
  && List.exists (( == ) root) (Ir.Integer_globals.slot_initializers slot)
  && List.exists (( == ) root) proof.load_roots

let is_load_slot proof slot =
  List.exists (is_load_root proof slot)
    (Ir.Integer_globals.slot_initializers slot)

let is_static_load_root proof slot root =
  List.exists (( == ) slot) (Ir.Integer_globals.statics proof.globals)
  && List.exists (( == ) root) (Ir.Integer_globals.static_initializers slot)
  && List.exists
       (fun (original, source) -> original == slot && source == root)
       proof.static_load_roots

let is_static_load_slot proof slot =
  List.exists
    (is_static_load_root proof slot)
    (Ir.Integer_globals.static_initializers slot)

let owns_static_load_address proof ~globals ~slot ~instruction =
  let module Initialization = Ir.Global_initialization in
  let module Id = Ir.Instruction_sequence.Instruction_id in
  globals == proof.globals
  && List.exists (( == ) slot) (Ir.Integer_globals.statics globals)
  && List.exists
       (fun region ->
         let original = Initialization.static_slot region in
         let description = Initialization.describe_static region in
         is_static_load_root proof original (Initialization.static_root region)
         && Ir.Integer_globals.static_frame original
            == Ir.Integer_globals.static_frame slot
         && Id.compare description.first instruction <= 0
         && Id.compare instruction description.last <= 0)
       (Initialization.static_regions proof.initialization)

let publications_precede_entry ~globals ~initialization ~entry =
  let module Initialization = Ir.Global_initialization in
  let publications = Initialization.publications initialization in
  let descriptions =
    List.map Initialization.describe_publication publications
  in
  let original =
    match Initialization.publication_evidence initialization with
    | None -> publications = []
    | Some evidence ->
        Ir.Initializer_publication.matches evidence ~globals ~entry descriptions
  in
  original
  &&
  match
    Ir.X87_stack.graph entry |> Ir.Block_graph.entry
    |> Ir.Block_graph.instructions |> Ir.Instruction_sequence.instructions
  with
  | [] -> false
  | first :: _ ->
      let first = (Ir.Instruction_sequence.description first).instruction_id in
      List.for_all
        (fun publication ->
          Ir.Instruction_sequence.Instruction_id.equal first
            (Initialization.publication_before publication))
        publications

let create ~span ~static_completions ~completions ~preparation ~runtime_calls
    ~initialization ~entry ~functions =
  let globals = Integer_initializers.globals preparation in
  let functions = bodies functions in
  let evidence = Integer_initializers.native_evidence preparation in
  let completed =
    List.map Native_default_preparation.initializer_preparation completions
  in
  let static_evidence =
    Integer_initializers.native_static_evidence preparation
  in
  let completed_statics =
    List.map Native_default_preparation.static_preparation static_completions
  in
  let load_roots = Integer_initializers.native_load_roots preparation in
  let load_evidence =
    List.filter Integer_initializers.native_is_load evidence
  in
  let load_regions = Ir.Global_initialization.regions initialization in
  let static_load_roots =
    Integer_initializers.native_static_load_roots preparation
  in
  let static_load_evidence =
    List.filter Integer_initializers.native_static_is_load static_evidence
  in
  let static_regions = Ir.Global_initialization.static_regions initialization in
  if
    (not (Integer_initializers.native_statics_complete ~span preparation))
    || List.length static_evidence <> List.length completed_statics
    || (not (List.for_all2 ( == ) static_evidence completed_statics))
    || (not (Integer_initializers.native_complete ~span preparation))
    || List.length evidence <> List.length completed
    || not (List.for_all2 ( == ) evidence completed)
  then
    Error "native global initializers lack their complete original preparation"
  else if
    (not (Ir.Global_initialization.matches initialization ~globals ~entry))
    || (not
          (Ir.Runtime_call_context.matches runtime_calls ~entry
             ~initialization:(Some initialization) ~functions))
    || List.length load_roots <> List.length load_evidence
    || List.length load_regions <> List.length load_roots
    || (not
          (List.for_all2
             (fun region root ->
               Ir.Global_initialization.root region == root
               && Ir.Global_initialization.phase region
                  = Ir.Global_initialization.Load_initializer)
             load_regions load_roots))
    || List.length static_load_roots <> List.length static_load_evidence
    || List.length static_regions <> List.length static_load_roots
    || (not
          (List.for_all2
             (fun region (slot, root) ->
               Ir.Global_initialization.static_slot region == slot
               && Ir.Global_initialization.static_root region == root
               && Ir.Global_initialization.static_phase region
                  = Ir.Global_initialization.Load_initializer)
             static_regions static_load_roots))
    || (not (publications_precede_entry ~globals ~initialization ~entry))
    || Ir.Integer_globals.is_task_command globals
    || Ir.Global_initialization.prepared_steps initialization
       <> Integer_initializers.executed_steps preparation
  then
    Error
      "native global preparation has another initialization or callable bundle"
  else
    Ok
      {
        globals;
        initialization;
        entry;
        runtime_calls;
        functions;
        load_roots;
        static_load_roots;
      }
