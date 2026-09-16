open Holyc_lib
module Declarations = Task_declarations
module Preparation = Holyc_lib__Driver.Native_default_preparation
module Unit = Holyc_lib__Driver.Integer_unit
module VM = Ir_integer_interpreter

type t = {
  unit_ : Unit.compiled;
  preparation_steps : int;
  default_bytes : int;
  prepared_defaults : Holyc_lib__Ir.Prepared_parameter_default.t list;
  completions : Preparation.completion list;
}

let diagnostic ~span code message =
  Diagnostic.make ~code ~severity:Diagnostic.Error ~message ~primary:span ()

let of_message ~span code = function
  | Ok value -> Ok value
  | Error message -> Error [ diagnostic ~span code message ]

let ( let* ) = Result.bind

let compile ?(max_initializer_steps = 100_000) ?(max_default_bytes = 65_536)
    ~mode ~path ~contents () =
  let session = Session.create () in
  let source = Session.add_source session ~path ~contents in
  let span =
    Span.unsafe_make ~source:(Source_file.id source) ~start:0
      ~stop:(Source_file.length source)
  in
  let* config =
    Preprocessor.Config.create ~compilation_mode:mode ()
    |> of_message ~span "HCRUN0004"
  in
  let table = Session.semantic_symbols session in
  let* ledger =
    Declarations.create_source ~max_offset_work:max_initializer_steps session
      ~source
    |> of_message ~span "HCRUN0004"
  in
  let* preparation =
    Preparation.create ~compilation_mode:mode ~max_initializer_steps
      ~max_default_bytes session
    |> of_message ~span "HCIRVM0001"
  in
  let commands : Parser.command_sink =
    {
      checkpoint = Some (Declarations.observe_command ledger);
      query = Some (Declarations.observe_query ledger);
      reference = Some (Declarations.observe_reference ledger);
      implicit_output = Some (Declarations.observe_implicit_output ledger);
      call = None;
      declaration =
        Some
          (fun event ->
            let* () = Declarations.observe ledger event in
            match event with
            | Parser.Parameter_default_completed receipt ->
                Preparation.prepare preparation ~session ~ledger receipt
            | Parser.Function_header_completed header ->
                Declarations.complete_source_defaults ledger header
            | _ -> Ok ());
      dimension_count = Some (Declarations.grammar_dimension_count ledger);
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let parsed =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  let* () =
    if Parser.has_errors parsed then Error parsed.diagnostics else Ok ()
  in
  let* ast =
    match parsed.ast with
    | Some ast -> Ok ast
    | None -> Error parsed.diagnostics
  in
  let* source_command =
    Declarations.seal_source ledger ast
    |> Result.map_error (fun errors -> parsed.diagnostics @ errors)
  in
  let* prepared_defaults =
    Declarations.native_source_defaults ~table ~ast source_command
    |> Result.map_error (fun errors -> parsed.diagnostics @ errors)
  in
  let* checked =
    Unit.compile_source_output ~source_command ~max_initializer_steps session
      ~config
      { parsed with diagnostics = [] }
    |> Result.map_error (fun errors -> parsed.diagnostics @ errors)
  in
  Ok
    {
      unit_ = checked.value;
      preparation_steps = Preparation.work preparation;
      default_bytes = Preparation.bytes preparation;
      prepared_defaults;
      completions = Preparation.completions preparation;
    }

let execute ?(max_frame_bytes = 1_048_576) ?(max_call_depth = 128) ~max_steps
    fixture =
  VM.execute_program
    ~runtime_calls:(Unit.runtime_calls fixture.unit_)
    ~globals:(Unit.globals fixture.unit_)
    ~initialization:(Unit.initialization fixture.unit_)
    ~max_steps ~max_frame_bytes ~max_call_depth
    ~functions:(Unit.functions fixture.unit_)
    (Unit.entry fixture.unit_)
