open Holyc_lib
module C = Semantic_declaration_collection
module Record = Semantic_compiler_record
module Source = Holyc_lib__Sema.Source_type_reference

module Type_specifiers = Hashtbl.Make (struct
  type t = Ast.type_specifier

  let equal = ( == )
  let hash = Hashtbl.hash
end)

type aggregate = {
  source : Parser.aggregate_publication;
  publication : C.publication;
  progress : Record.aggregate_progress;
  initial : Record.t;
  mutable current : Record.t;
}

type function_ = {
  source : Parser.function_publication;
  publication : C.publication;
}

type query = {
  root : Parser.query_root;
  function_publication : C.publication;
  read : Record.sizeof_read;
}

let checked = function
  | Ok value -> value
  | Error reason -> Alcotest.fail reason

let reject label result =
  Alcotest.(check bool) label true (Result.is_error result)

let parse session mode source commands =
  let result =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~symbols:(Session.symbols session)
      ~definitions:(Session.definitions session)
      ~config:(Preprocessor.Config.create ~compilation_mode:mode () |> checked)
      source
  in
  match result.ast with
  | Some _ when not (Parser.has_errors result) -> ()
  | _ ->
      Alcotest.fail
        (result.diagnostics
        |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
        |> String.concat "; ")

let local_query_authority mode () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let foreign_namespace = C.create_namespace ~table () |> checked in
  let foreign_table = Session.semantic_symbols (Session.create ()) in
  let aggregates : aggregate list ref = ref [] in
  let functions : function_ list ref = ref [] in
  let selected = Type_specifiers.create 16 in
  let queries = ref [] and completed = ref 0 in
  let timing_reads = ref [] and timing_type = ref None in
  let capture_type source =
    let type_specifier, selection = Source.source_type source |> checked in
    match selection with
    | None -> ()
    | Some selection ->
        let aggregate =
          List.find
            (fun (state : aggregate) ->
              state.source.aggregate_entry == selection.entry
              && state.source.aggregate_environment == selection.environment)
            !aggregates
        in
        let proof =
          Source.select_aggregate ~table ~namespace ~source
            aggregate.publication
          |> checked
        in
        Type_specifiers.replace selected type_specifier proof
  in
  let declaration event =
    (match event with
    | Parser.Aggregate_declared source ->
        let publication = C.publish_aggregate namespace source |> checked in
        let progress =
          Record.begin_aggregate ~table ~namespace publication |> checked
        in
        let initial = Record.aggregate_metadata progress |> checked in
        aggregates :=
          { source; publication; progress; initial; current = initial }
          :: !aggregates
    | Parser.Aggregate_advanced phase ->
        let state =
          List.find
            (fun (state : aggregate) -> state.source == phase.phase_aggregate)
            !aggregates
        in
        Record.advance_aggregate
          ~dimensions:(fun _ -> None)
          state.progress phase
        |> checked;
        state.current <- Record.aggregate_metadata state.progress |> checked
    | Parser.Aggregate_completed receipt ->
        let state =
          List.find
            (fun (state : aggregate) ->
              state.source == receipt.aggregate_publication)
            !aggregates
        in
        state.current <-
          Record.complete_aggregate ~progress:state.progress ~table ~namespace
            state.publication receipt
          |> checked
    | Parser.Function_declared source ->
        let publication = C.publish_function namespace source |> checked in
        functions := { source; publication } :: !functions
    | Parser.Function_parameter_declared source ->
        capture_type (Source.Function_parameter source)
    | Parser.Function_local_allocated source ->
        capture_type (Source.Function_local source)
    | _ -> ());
    Ok ()
  in
  let current_record proof =
    let symbol = Source.selected_base_symbol proof in
    List.find_opt
      (fun (state : aggregate) ->
        Option.fold ~none:false ~some:(( == ) symbol)
          (C.publication_aggregate_identity state.publication)
        && Option.fold ~none:false ~some:(( == ) state.publication)
             (C.current_aggregate_publication namespace state.publication))
      !aggregates
  in
  let local_type (root : Parser.query_root) =
    match root.query_local with
    | Some { local_source = Parser.Local_variable local; _ } ->
        (local.local_type_specifier, local.local_pointer_layers)
    | Some { local_source = Parser.Local_parameter parameter; _ } ->
        (parameter.type_specifier, parameter.pointer_layers)
    | Some { local_source = Parser.Variadic_count _; _ }
    | Some { local_source = Parser.Variadic_vector _; _ }
    | None -> Alcotest.fail "expected an original named local sizeof root"
  in
  let read_local root function_publication ~aggregate =
    Record.read_local_sizeof ~table ~namespace ~function_publication ~root
      ~aggregate ~dimensions:[]
  in
  let query event =
    (match event with
    | Parser.Query_root root ->
        let function_ =
          List.find
            (fun (value : function_) ->
              value.source.function_header.declaration_command
              == root.query_command)
            !functions
        in
        let type_specifier, pointer_layers = local_type root in
        let proof = Type_specifiers.find_opt selected type_specifier in
        let aggregate =
          if pointer_layers <> [] then None
          else
            Option.bind proof (fun proof ->
                Option.map
                  (fun (state : aggregate) -> (proof, state.current))
                  (current_record proof))
        in
        let read =
          read_local root function_.publication ~aggregate |> checked
        in
        let name =
          match root.query_node with
          | Parser.Sizeof_target name -> name.spelling
          | _ -> Alcotest.fail "expected a sizeof root"
        in
        let expected =
          if name = "ptr" then 8L
          else if name = "early" then (
            (match !timing_type with
            | None -> timing_type := Some type_specifier
            | Some original ->
                Alcotest.(check bool)
                  "both timing queries select the same original local type" true
                  (original == type_specifier));
            match List.length !timing_reads with
            | 0 -> 0L
            | 1 -> 2L
            | _ -> Alcotest.fail "unexpected third query for the forward local")
          else 2L
        in
        Alcotest.(check int64)
          ("live sizeof(" ^ name ^ ")")
          expected
          (Record.sizeof_value read ~pointer:false);
        if name = "early" then timing_reads := read :: !timing_reads;
        if name = "a" then (
          let proof = Option.get proof in
          let state = Option.get (current_record proof) in
          let other =
            List.find
              (fun (state : aggregate) ->
                state.source.aggregate_name.spelling = "B")
              !aggregates
          in
          reject "equal-sized other class does not own the selected type"
            (read_local root function_.publication
               ~aggregate:(Some (proof, other.current)));
          reject "uncompleted stale class record cannot replace current"
            (read_local root function_.publication
               ~aggregate:(Some (proof, state.initial)));
          let other_query =
            List.find
              (fun query ->
                match query.root.query_node with
                | Parser.Sizeof_target name -> name.spelling = "arg"
                | _ -> false)
              !queries
          in
          let other_type, _ = local_type other_query.root in
          let other_proof = Type_specifiers.find selected other_type in
          Alcotest.(check bool)
            "foreign proof has another type occurrence" true
            (other_type != type_specifier);
          Alcotest.(check bool)
            "foreign proof retains the same class identity" true
            (Source.selected_base_symbol other_proof
            == Source.selected_base_symbol proof);
          reject "another original type occurrence cannot lend its proof"
            (read_local root function_.publication
               ~aggregate:(Some (other_proof, state.current)));
          reject "foreign namespace cannot authenticate a local query"
            (Record.read_local_sizeof ~table ~namespace:foreign_namespace
               ~function_publication:function_.publication ~root ~aggregate
               ~dimensions:[]);
          reject "foreign table cannot authenticate a local query"
            (Record.read_local_sizeof ~table:foreign_table ~namespace
               ~function_publication:function_.publication ~root ~aggregate
               ~dimensions:[]);
          let other_function =
            List.find
              (fun (other : function_) ->
                other.publication != function_.publication)
              !functions
          in
          reject "other function cannot authenticate this local query"
            (read_local root other_function.publication ~aggregate));
        queries :=
          { root; function_publication = function_.publication; read }
          :: !queries
    | Parser.Query_completed receipt ->
        let query =
          List.find (fun query -> query.root == receipt.query_root) !queries
        in
        Record.complete_sizeof ~table ~receipt query.read |> checked;
        incr completed
    | _ -> ());
    Ok ()
  in
  let commands : Parser.command_sink =
    {
      lexical_lookup = None;
      checkpoint = None;
      reference = None;
      call = None;
      implicit_output = None;
      declaration = Some declaration;
      query = Some query;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let source =
    Session.add_source session ~path:"local-sizeof-authority.hc"
      ~contents:
        "class A{U16 value;};class B{U16 value;};U0 Other(){};U0 F(A arg){A \
         a;B b;U16 n;A *ptr;I64 \
         sizes=sizeof(b)+sizeof(arg)+sizeof(a)+sizeof(n)+sizeof(ptr);}; U0 \
         Timing(){extern class C;C early;sizeof(early);class C{U16 \
         value;};sizeof(early);}"
  in
  parse session mode source commands;
  Alcotest.(check int) "seven original sizeof queries" 7 !completed;
  (match List.rev !timing_reads with
  | [ before; after ] ->
      Alcotest.(check int64)
        "saved query before completion still observes forward size" 0L
        (Record.sizeof_value before ~pointer:false);
      Alcotest.(check int64)
        "saved query after completion observes completed class size" 2L
        (Record.sizeof_value after ~pointer:false)
  | _ -> Alcotest.fail "expected two original sizeof queries for early");
  List.iter
    (fun query ->
      Alcotest.(check bool)
        "query root expired after parser return" false
        (Parser.query_root_is_current query.root))
    !queries;
  let cls =
    List.find
      (fun query ->
        match query.root.query_node with
        | Parser.Sizeof_target name -> name.spelling = "a"
        | _ -> false)
      !queries
  in
  let proof =
    let spec, _ = local_type cls.root in
    Type_specifiers.find selected spec
  in
  let state = Option.get (current_record proof) in
  reject "expired root cannot reuse the current aggregate snapshot"
    (read_local cls.root cls.function_publication
       ~aggregate:(Some (proof, state.current)))

let () =
  Alcotest.run "Local sizeof source authority"
    [
      ( "query timing and provenance",
        [
          Alcotest.test_case "JIT original query and foreign evidence" `Quick
            (local_query_authority Preprocessor.Jit);
          Alcotest.test_case "AOT original query and foreign evidence" `Quick
            (local_query_authority Preprocessor.Aot);
        ] );
    ]
