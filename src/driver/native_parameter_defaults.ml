module Headers = Sema.Function_type_resolution
module Function = Ir.Function_body
module Program = Ir.Integer_interpreter
module Prepared = Ir.Prepared_parameter_default
module Prepared_callback = Ir.Prepared_callback_default
module Frame = Sema.Function_frame_layout
module Integer_globals = Ir.Integer_globals
module Runtime_call_context = Ir.Runtime_call_context
module Global_initialization = Ir.Global_initialization
module X87_stack = Ir.X87_stack
module Integer_interpreter = Ir.Integer_interpreter
module Default_fragment_destination = Ir.Default_fragment_destination
module Type = Sema.Type

type requirement = {
  header : Headers.resolved_function;
  parameter : Headers.parameter;
  prepared : Prepared.t;
}

type callback_requirement = {
  pointer : Headers.function_pointer;
  callback_parameter : Headers.parameter;
  callback_prepared : Prepared_callback.t;
}

type t = {
  globals : Integer_globals.t;
  runtime_calls : Runtime_call_context.t;
  initialization : Global_initialization.t;
  entry : X87_stack.t;
  functions : Function.t list;
  requirements : requirement list;
  callback_requirements : callback_requirement list;
}

let same_functions expected actual =
  List.length expected = List.length actual
  && List.for_all2
       (fun body (definition : Integer_interpreter.function_definition) ->
         body == definition.body)
       expected actual

let matches proof ~globals ~runtime_calls ~initialization ~entry ~functions =
  proof.globals == globals
  && proof.runtime_calls == runtime_calls
  && proof.initialization == initialization
  && proof.entry == entry
  && same_functions proof.functions functions

let admits proof ~prepared ~header ~parameter =
  List.exists
    (fun requirement ->
      requirement.prepared == prepared
      && requirement.header == header
      && requirement.parameter == parameter
      && Prepared.matches prepared ~header ~parameter)
    proof.requirements

let admits_callback proof ~prepared ~pointer ~parameter =
  List.exists
    (fun requirement ->
      requirement.callback_prepared == prepared
      && requirement.pointer == pointer
      && requirement.callback_parameter == parameter
      && Prepared_callback.matches prepared ~pointer ~parameter)
    proof.callback_requirements

let scalar_word type_ = Option.is_some (Ir.Integer_scalar_storage.of_type type_)

let parameter_type parameter =
  parameter |> Headers.parameter_type_reference
  |> Sema.Type_reference.resolved_type

let supported_parameter ?(allow_data = false) parameter =
  let stack_register =
    match Headers.parameter_register_selection parameter with
    | Sema.Register_request.Unspecified | Sema.Register_request.Disabled -> true
    | Sema.Register_request.Allocatable | Sema.Register_request.Explicit _ ->
        false
  in
  stack_register
  &&
  match Headers.parameter_declarator_kind parameter with
  | Headers.Object ->
      let type_ = parameter_type parameter in
      scalar_word type_
      || (Type.pointer_depth type_ = 0
         &&
         match Type.base type_ with
         | Type.Aggregate _ -> true
         | _ -> false)
      || allow_data
         && Type.pointer_depth type_ = 1
         && Option.is_some
              (Option.bind
                 (Result.to_option (Type.dereference type_))
                 Ir.Integer_scalar_storage.of_type)
  | Headers.Function_pointer pointer ->
      List.length (Headers.function_pointer_indirection_origins pointer) = 1

let execution_type_matches fragment prepared_type destination =
  let _, _, pointer = Sema.Default_fragment.parameter_parts fragment in
  match pointer with
  | None -> (
      match Default_fragment_destination.aggregate_value_type destination with
      | None ->
          Type.equal
            (Default_fragment_destination.type_ destination)
            prepared_type
      | Some _ ->
          Option.fold ~none:false
            ~some:(fun reference ->
              Type.equal
                (Sema.Type_reference.resolved_type reference)
                prepared_type)
            (Result.to_option (Sema.Default_fragment.parameter_type fragment))
          && scalar_word (Default_fragment_destination.type_ destination))
  | Some pointer when List.length pointer.Frontend.Ast.indirection_layers = 1 ->
      let storage =
        Type.make_primitive ~form:Internal_storage ~primitive:I64
          ~pointer_depth:1
        |> Result.get_ok
      and word =
        Type.make_primitive ~form:Internal_storage ~primitive:I64
          ~pointer_depth:0
        |> Result.get_ok
      in
      Type.equal prepared_type storage
      && Type.equal (Default_fragment_destination.type_ destination) word
  | Some _ -> false

let same_requirement left_header left_parameter right_header right_parameter =
  left_header == right_header && left_parameter == right_parameter

let add_requirements ?(allow_data = false) globals prepared requirements header
    =
  let parameters =
    header |> Headers.function_signature |> Headers.signature_parameters
  in
  List.fold_left
    (fun result parameter ->
      let ( let* ) = Result.bind in
      let* requirements = result in
      match Headers.parameter_default parameter with
      | None -> Ok requirements
      | Some (Headers.Lastclass_default _) ->
          Error "native parameter defaults do not admit lastclass"
      | Some (Headers.Expression_default { contains_string_literal = true; _ })
        when not allow_data ->
          Error "native parameter defaults do not admit string-backed values"
      | Some (Headers.Expression_default _) ->
          if not (supported_parameter ~allow_data parameter) then
            Error
              "native parameter defaults require scalar integer objects or \
               original one-star callback-word parameters"
          else
            let* value =
              match
                Integer_globals.prepared_parameter_default globals ~header
                  ~parameter
              with
              | Some value -> Ok value
              | None ->
                  Error
                    "native parameter default lacks its original prepared value"
            in
            if not (List.exists (( == ) value) prepared) then
              Error
                "native parameter default is outside the supplied source proof"
            else if
              List.exists
                (fun requirement ->
                  same_requirement requirement.header requirement.parameter
                    header parameter)
                requirements
            then Ok requirements
            else Ok ({ header; parameter; prepared = value } :: requirements))
    (Ok requirements) parameters

let execution_matches prepared execution =
  let authority = Program.default_constant_authority execution in
  let fragment = Sema.Default_fragment.authorized_fragment authority in
  let destination = Program.default_constant_destination execution in
  (match Sema.Default_fragment.source fragment with
    | Named (publication, receipt) ->
        receipt == Prepared.receipt prepared
        && publication == Prepared.publication prepared
    | Callback _ -> false)
  && Sema.Default_fragment.references fragment = []
  && Default_fragment_destination.fragment destination == fragment
  && execution_type_matches fragment (Prepared.type_ prepared) destination
  && Program.default_constant_is_consumed execution
  && Option.fold ~none:false
       ~some:(Int64.equal (Program.default_constant_bits execution))
       (Prepared.word_bits prepared)

let callback_execution_matches prepared execution =
  let authority = Program.default_constant_authority execution in
  let fragment = Sema.Default_fragment.authorized_fragment authority in
  let destination = Program.default_constant_destination execution in
  (match Sema.Default_fragment.source fragment with
    | Callback (namespace, receipt) ->
        namespace == Prepared_callback.namespace prepared
        && receipt == Prepared_callback.receipt prepared
    | Named _ -> false)
  && Sema.Default_fragment.references fragment = []
  && Default_fragment_destination.fragment destination == fragment
  && execution_type_matches fragment
       (Prepared_callback.type_ prepared)
       destination
  && Program.default_constant_is_consumed execution
  && Option.fold ~none:false
       ~some:(Int64.equal (Program.default_constant_bits execution))
       (Prepared_callback.word_bits prepared)

let callback_pointers globals functions =
  let pointers = ref [] in
  let rec add_pointer pointer =
    if not (List.exists (( == ) pointer) !pointers) then (
      pointers := pointer :: !pointers;
      pointer |> Headers.function_pointer_signature |> add_signature)
  and add_signature signature =
    signature |> Headers.signature_parameters
    |> List.iter (fun parameter ->
        match Headers.parameter_declarator_kind parameter with
        | Headers.Object -> ()
        | Headers.Function_pointer pointer -> add_pointer pointer)
  in
  Integer_globals.storage_slots globals
  |> List.iter (fun slot ->
      Option.iter add_pointer (Integer_globals.storage_callback_pointer slot));
  List.iter
    (fun (definition : Integer_interpreter.function_definition) ->
      definition.frame |> Frame.function_locations
      |> List.iter (fun location ->
          Option.iter add_pointer (Frame.location_callback_pointer location));
      Option.iter
        (fun declaration ->
          declaration |> Sema.Function_resolution.resolved_declaration_header
          |> Headers.function_signature |> add_signature;
          declaration |> Sema.Function_resolution.resolved_declaration_site
          |> Sema.Function_resolution.declaration_site_function
          |> Headers.function_signature |> add_signature)
        (Function.definition_declaration definition.body))
    functions;
  !pointers

let add_callback_requirements ?(allow_data = false) globals prepared
    requirements pointer =
  let ( let* ) = Result.bind in
  List.fold_left
    (fun result parameter ->
      let* requirements = result in
      match Headers.parameter_default parameter with
      | None -> Ok requirements
      | Some (Headers.Lastclass_default _) ->
          Error "native callback defaults do not admit lastclass"
      | Some (Headers.Expression_default { contains_string_literal = true; _ })
        when not allow_data ->
          Error "native callback defaults do not admit string-backed values"
      | Some (Headers.Expression_default _) ->
          if not (supported_parameter ~allow_data parameter) then
            Error
              "native callback defaults require scalar integer objects or \
               original one-star callback-word parameters"
          else
            let* value =
              match
                Integer_globals.prepared_callback_default globals ~pointer
                  ~parameter
              with
              | Some value -> Ok value
              | None ->
                  Error "native callback default lacks its original saved value"
            in
            if not (List.exists (( == ) value) prepared) then
              Error
                "native callback default is outside its supplied source proof"
            else if
              List.exists
                (fun requirement ->
                  requirement.pointer == pointer
                  && requirement.callback_parameter == parameter
                  && requirement.callback_prepared == value)
                requirements
            then Ok requirements
            else
              Ok
                ({
                   pointer;
                   callback_parameter = parameter;
                   callback_prepared = value;
                 }
                :: requirements))
    (Ok requirements)
    (pointer |> Headers.function_pointer_signature
   |> Headers.signature_parameters)

let requires_callback_proof ~globals ~functions =
  callback_pointers globals functions
  |> List.exists (fun pointer ->
      pointer |> Headers.function_pointer_signature
      |> Headers.signature_parameters
      |> List.exists (fun parameter ->
          Option.is_some (Headers.parameter_default parameter)))

let create ~globals ~runtime_calls ~initialization ~entry ~functions ~prepared
    ~prepared_callbacks ~completions =
  let ( let* ) = Result.bind in
  let executions = List.map Native_default_preparation.execution completions in
  let bodies =
    List.map
      (fun (definition : Integer_interpreter.function_definition) ->
        definition.body)
      functions
  in
  let* () =
    if
      Global_initialization.matches initialization ~globals ~entry
      && Runtime_call_context.matches runtime_calls ~entry
           ~initialization:(Some initialization) ~functions:bodies
    then Ok ()
    else Error "native parameter-default proof has another compiled bundle"
  in
  let* () =
    let rec unique seen = function
      | [] -> Ok ()
      | value :: rest ->
          if
            List.exists
              (fun prior ->
                prior == value
                || Prepared.receipt prior == Prepared.receipt value)
              seen
          then Error "native parameter-default proof repeats prepared evidence"
          else unique (value :: seen) rest
    in
    unique [] prepared
  in
  let* () =
    let rec unique seen = function
      | [] -> Ok ()
      | value :: rest ->
          if
            List.exists
              (fun prior ->
                prior == value
                || Prepared_callback.receipt prior
                   == Prepared_callback.receipt value)
              seen
          then Error "native callback-default proof repeats prepared evidence"
          else unique (value :: seen) rest
    in
    unique [] prepared_callbacks
  in
  let* () =
    let rec unique named callbacks = function
      | [] -> Ok ()
      | execution :: rest -> (
          let source =
            execution |> Program.default_constant_authority
            |> Sema.Default_fragment.authorized_fragment
            |> Sema.Default_fragment.source
          in
          match source with
          | Callback (_, receipt) ->
              if List.exists (( == ) receipt) callbacks then
                Error "native callback-default proof repeats execution evidence"
              else unique named (receipt :: callbacks) rest
          | Named (_, receipt) ->
              if List.exists (( == ) receipt) named then
                Error
                  "native parameter-default proof repeats execution evidence"
              else unique (receipt :: named) callbacks rest)
    in
    unique [] [] executions
  in
  let* requirements =
    List.fold_left
      (fun result (definition : Integer_interpreter.function_definition) ->
        let* requirements = result in
        let body = definition.body in
        let* declaration =
          match Function.definition_declaration body with
          | Some declaration -> Ok declaration
          | None ->
              Error
                "native parameter-default proof requires original definitions"
        in
        let selected =
          Sema.Function_resolution.resolved_declaration_header declaration
        in
        let source =
          declaration |> Sema.Function_resolution.resolved_declaration_site
          |> Sema.Function_resolution.declaration_site_function
        in
        let* requirements =
          add_requirements globals prepared requirements selected
        in
        add_requirements globals prepared requirements source)
      (Ok []) functions
  in
  let used = List.map (fun requirement -> requirement.prepared) requirements in
  let* callback_requirements =
    List.fold_left
      (fun result pointer ->
        let* requirements = result in
        add_callback_requirements globals prepared_callbacks requirements
          pointer)
      (Ok [])
      (callback_pointers globals functions)
  in
  let callback_used =
    List.map
      (fun requirement -> requirement.callback_prepared)
      callback_requirements
  in
  let* () =
    if
      List.for_all (fun value -> List.exists (( == ) value) used) prepared
      && List.for_all
           (fun value -> List.exists (( == ) value) callback_used)
           prepared_callbacks
      && List.for_all
           (fun value -> List.exists (execution_matches value) executions)
           prepared
      && List.for_all
           (fun value ->
             List.exists (callback_execution_matches value) executions)
           prepared_callbacks
      && List.for_all
           (fun execution ->
             List.exists
               (fun value -> execution_matches value execution)
               prepared
             || List.exists
                  (fun value -> callback_execution_matches value execution)
                  prepared_callbacks)
           executions
      && List.length prepared + List.length prepared_callbacks
         = List.length executions
    then Ok ()
    else
      Error
        "native parameter-default proof has missing, extra or foreign \
         preparation evidence"
  in
  Ok
    {
      globals;
      runtime_calls;
      initialization;
      entry;
      functions = bodies;
      requirements;
      callback_requirements;
    }

let create_task ~globals ~runtime_calls ~initialization ~entry ~functions
    ~sources ~available ~available_callback =
  let ( let* ) = Result.bind in
  let bodies =
    List.map
      (fun (definition : Integer_interpreter.function_definition) ->
        definition.body)
      functions
  in
  let* () =
    if
      Integer_globals.is_task_command globals
      && Global_initialization.matches initialization ~globals ~entry
      && Runtime_call_context.matches runtime_calls ~entry
           ~initialization:(Some initialization) ~functions:bodies
    then Ok ()
    else Error "native saved-default proof has another original task bundle"
  in
  let add_header source_globals requirements header =
    let parameters =
      header |> Headers.function_signature |> Headers.signature_parameters
    in
    let prepared =
      List.filter_map
        (fun parameter ->
          Integer_globals.prepared_parameter_default source_globals ~header
            ~parameter)
        parameters
    in
    let* requirements =
      add_requirements ~allow_data:true source_globals prepared requirements
        header
    in
    let* () =
      List.fold_left
        (fun result parameter ->
          let* () = result in
          match
            Integer_globals.prepared_parameter_default source_globals ~header
              ~parameter
          with
          | None -> Ok ()
          | Some value ->
              available ~globals:source_globals ~header ~parameter value)
        (Ok ()) parameters
    in
    Ok requirements
  in
  let* requirements =
    List.fold_left
      (fun result (source_globals, definition, _) ->
        let* requirements = result in
        let* declaration =
          match
            Function.definition_declaration definition.Integer_interpreter.body
          with
          | Some declaration -> Ok declaration
          | None ->
              Error
                "native saved defaults require original function definitions"
        in
        let* requirements =
          add_header source_globals requirements
            (Sema.Function_resolution.resolved_declaration_header declaration)
        in
        add_header source_globals requirements
          (declaration |> Sema.Function_resolution.resolved_declaration_site
         |> Sema.Function_resolution.declaration_site_function))
      (Ok []) sources
  in
  let graphs =
    (globals, runtime_calls, Runtime_call_context.Entry, X87_stack.graph entry)
    :: List.map
         (fun (source_globals, definition, calls) ->
           ( source_globals,
             calls,
             Runtime_call_context.Function definition.Integer_interpreter.body,
             Function.body definition.body ))
         sources
  in
  let* requirements =
    List.fold_left
      (fun result (source_globals, calls, owner, graph) ->
        let* requirements = result in
        if not (Runtime_call_context.matches_graph calls ~owner graph) then
          Error "native saved defaults require the original sealed call graph"
        else
          List.fold_left
            (fun result block ->
              List.fold_left
                (fun result instruction ->
                  let* requirements = result in
                  let id =
                    (Ir.Instruction_sequence.description instruction)
                      .instruction_id
                  in
                  match Runtime_call_context.find_start calls ~owner id with
                  | None -> Ok requirements
                  | Some call ->
                      add_header source_globals requirements
                        (Runtime_call_context.header call))
                result
                (Ir.Instruction_sequence.instructions
                   (Ir.Block_graph.instructions block)))
            (Ok requirements)
            (Ir.Block_graph.blocks graph))
      (Ok requirements) graphs
  in
  let add_pointer source_globals requirements pointer =
    let parameters =
      pointer |> Headers.function_pointer_signature
      |> Headers.signature_parameters
    in
    let prepared =
      List.filter_map
        (fun parameter ->
          Integer_globals.prepared_callback_default source_globals ~pointer
            ~parameter)
        parameters
    in
    let* requirements =
      add_callback_requirements ~allow_data:true source_globals prepared
        requirements pointer
    in
    let* () =
      List.fold_left
        (fun result parameter ->
          let* () = result in
          match
            Integer_globals.prepared_callback_default source_globals ~pointer
              ~parameter
          with
          | None -> Ok ()
          | Some prepared ->
              available_callback ~globals:source_globals ~pointer ~parameter
                prepared)
        (Ok ()) parameters
    in
    Ok requirements
  in
  let* callback_requirements =
    List.fold_left
      (fun result (source_globals, definitions) ->
        List.fold_left
          (fun result pointer ->
            let* requirements = result in
            add_pointer source_globals requirements pointer)
          result
          (callback_pointers source_globals definitions))
      (Ok [])
      ((globals, functions)
      :: List.map
           (fun (source_globals, definition, _) ->
             (source_globals, [ definition ]))
           sources)
  in
  let* callback_requirements =
    List.fold_left
      (fun result (source_globals, calls, owner, _) ->
        let* requirements = result in
        let* callbacks =
          match Runtime_call_context.original_callback_calls calls ~owner with
          | Some callbacks -> Ok callbacks
          | None ->
              Error "native callback defaults require their original call graph"
        in
        List.fold_left
          (fun result (callback : Runtime_call_context.callback_call) ->
            let* requirements = result in
            add_pointer source_globals requirements callback.callback_pointer)
          (Ok requirements) callbacks)
      (Ok callback_requirements) graphs
  in
  Ok
    {
      globals;
      runtime_calls;
      initialization;
      entry;
      functions = bodies;
      requirements;
      callback_requirements;
    }
