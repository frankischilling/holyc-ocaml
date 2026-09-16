module Headers = Sema.Function_type_resolution
module Function = Ir.Function_body
module Program = Ir.Default_fragment_program
module Prepared = Ir.Prepared_parameter_default
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

type t = {
  globals : Integer_globals.t;
  runtime_calls : Runtime_call_context.t;
  initialization : Global_initialization.t;
  entry : X87_stack.t;
  functions : Function.t list;
  requirements : requirement list;
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

let scalar_word type_ =
  Type.pointer_depth type_ = 0
  &&
  match Type.base type_ with
  | Type.Primitive (_, (Sema.Primitive_type.I64 | Sema.Primitive_type.U64)) ->
      true
  | _ -> false

let parameter_type parameter =
  parameter |> Headers.parameter_type_reference
  |> Sema.Type_reference.resolved_type

let same_requirement left_header left_parameter right_header right_parameter =
  left_header == right_header && left_parameter == right_parameter

let add_requirements globals prepared requirements header =
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
        -> Error "native parameter defaults do not admit string-backed values"
      | Some (Headers.Expression_default _) ->
          let type_ = parameter_type parameter in
          if
            (not (scalar_word type_))
            || Headers.parameter_register_requests parameter <> []
            ||
            match Headers.parameter_declarator_kind parameter with
            | Headers.Object -> false
            | Headers.Function_pointer _ -> true
          then
            Error "native parameter defaults require scalar I64 or U64 objects"
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
  let authority = Program.authority execution in
  let fragment = Sema.Default_fragment.authorized_fragment authority in
  let destination = Program.execution_destination execution in
  Sema.Default_fragment.receipt fragment == Prepared.receipt prepared
  && Sema.Default_fragment.publication fragment == Prepared.publication prepared
  && Sema.Default_fragment.references fragment = []
  && Default_fragment_destination.fragment destination == fragment
  && Type.equal
       (Default_fragment_destination.type_ destination)
       (Prepared.type_ prepared)
  &&
  match Program.code execution with
  | Program.Prepared bits -> Int64.equal bits (Prepared.bits prepared)
  | Program.Scheduled _ -> false

let create ~globals ~runtime_calls ~initialization ~entry ~functions ~prepared
    ~completions =
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
      | execution :: rest ->
          let receipt =
            execution |> Program.authority
            |> Sema.Default_fragment.authorized_fragment
            |> Sema.Default_fragment.receipt
          in
          if List.exists (( == ) receipt) seen then
            Error "native parameter-default proof repeats execution evidence"
          else unique (receipt :: seen) rest
    in
    unique [] executions
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
  let* () =
    if
      List.for_all (fun value -> List.exists (( == ) value) used) prepared
      && List.for_all
           (fun value -> List.exists (execution_matches value) executions)
           prepared
      && List.for_all
           (fun execution ->
             List.exists
               (fun value -> execution_matches value execution)
               prepared)
           executions
      && List.length prepared = List.length executions
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
    }
