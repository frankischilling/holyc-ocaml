open Holyc_lib
module C = Semantic_declaration_collection
module Record = Holyc_lib__Sema.Compiler_record
module Init = Holyc_lib__Sema.Initializer_type
module Types = Holyc_lib__Sema.Source_type_reference
module Type = Semantic_type

module Entries = Hashtbl.Make (struct
  type t = Symbol_visibility.entry

  let equal = ( == )
  let hash = Hashtbl.hash
end)

type state = {
  publication : C.publication;
  progress : Record.aggregate_progress;
  mutable record : Record.t;
}

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let reject label result =
  Alcotest.(check bool) label true (Result.is_error result)

let with_records ?(on_declared = fun _ _ _ -> ()) contents on_completed =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let entries = Entries.create 8 in
  let resolve type_ =
    match Type.base type_ with
    | Type.Aggregate symbol ->
        Entries.fold
          (fun _ state found ->
            if
              Option.fold ~none:false ~some:(( == ) symbol)
                (C.publication_aggregate_identity state.publication)
              && Option.fold ~none:false ~some:(( == ) state.publication)
                   (C.current_aggregate_publication namespace state.publication)
            then Some state.record
            else found)
          entries None
    | _ -> None
  in
  let declaration = function
    | Parser.Aggregate_declared source ->
        let publication = C.publish_aggregate namespace source |> checked in
        let progress =
          Record.begin_aggregate ~initializer_records:resolve ~table ~namespace
            publication
          |> checked
        in
        let record = Record.aggregate_metadata progress |> checked in
        Entries.add entries source.aggregate_entry
          { publication; progress; record };
        on_declared resolve (Entries.find entries source.aggregate_entry) source;
        Ok ()
    | Parser.Aggregate_advanced receipt ->
        let state =
          Entries.find entries receipt.phase_aggregate.aggregate_entry
        in
        let member =
          match receipt.phase_step with
          | Parser.Aggregate_member_prepared member ->
              Option.map
                (fun (selection : Parser.named_aggregate_selection) ->
                  let selected = Entries.find entries selection.entry in
                  let proof =
                    Types.select_aggregate ~table ~namespace
                      ~source:(Types.Aggregate_member receipt)
                      selected.publication
                    |> checked
                  in
                  Record.select_aggregate_member ~table ~namespace
                    ~selected_aggregate:proof receipt (Some selected.record)
                  |> checked)
                member.member_selection
          | _ -> None
        in
        Record.advance_aggregate
          ~dimensions:(fun _ -> None)
          ~members:(fun requested ->
            match member with
            | Some member when requested == receipt -> Ok member
            | _ -> Error "no original member selection")
          state.progress receipt
        |> checked;
        state.record <- Record.aggregate_metadata state.progress |> checked;
        Ok ()
    | Parser.Aggregate_completed receipt ->
        let state =
          Entries.find entries receipt.aggregate_publication.aggregate_entry
        in
        state.record <-
          Record.complete_aggregate ~table ~namespace ~progress:state.progress
            state.publication receipt
          |> checked;
        on_completed resolve state receipt;
        Ok ()
    | _ -> Ok ()
  in
  let commands : Parser.command_sink =
    {
      lexical_lookup = None;
      checkpoint = None;
      reference = None;
      call = None;
      implicit_output = None;
      declaration = Some declaration;
      query = None;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let source =
    Session.add_source session ~path:"initializer-scalar-phase.hc" ~contents
  in
  let parsed =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session)
      ~config:(Preprocessor.Config.create ~compilation_mode:Jit () |> checked)
      source
  in
  if Parser.has_errors parsed then
    Alcotest.fail
      (parsed.diagnostics
      |> List.map (fun (error : Diagnostic.t) ->
          error.code ^ ": " ^ error.message)
      |> String.concat "; ")

let first_member ?resolve state =
  let symbol =
    C.publication_aggregate_identity state.publication |> Option.get
  in
  let type_ = Type.make_aggregate ~symbol ~pointer_depth:0 |> checked in
  let node = Init.of_record ?resolve ~type_ state.record |> checked in
  match Init.view node |> checked with
  | Init.Aggregate (first :: _) -> Init.element_type first
  | _ -> Alcotest.fail "expected the original first direct member"

let reached_width_and_replacement () =
  let phases = ref [] and first_settled = ref None in
  with_records
    "extern class Item;class Box{Item item;I64 pad;};class Item{U16 \
     value;};class Item{U8 value;};" (fun resolve state receipt ->
      match receipt.aggregate_publication.aggregate_name.spelling with
      | "Box" ->
          let node = first_member ~resolve state in
          reject "ordinary traversal still requires completion" (Init.view node);
          let first = Init.begin_scalar_evaluation node |> checked in
          let second = Init.begin_scalar_evaluation node |> checked in
          Alcotest.(check int)
            "cached forward width before evaluation" 0
            (Init.scalar_byte_size (Init.scalar_evaluation_before first));
          phases := [ first; second ]
      | "Item" when Record.aggregate_initializer_is_complete state.record -> (
          match (!first_settled, !phases) with
          | None, first :: _ ->
              let scalar = Init.finish_scalar_evaluation first |> checked in
              Alcotest.(check int)
                "reached width after evaluation" 2
                (Init.scalar_byte_size scalar);
              first_settled := Some scalar;
              reject "settlement cannot repeat"
                (Init.finish_scalar_evaluation first)
          | Some _, _ -> ()
          | _ -> Alcotest.fail "missing original scalar phases")
      | _ -> ());
  let second = List.nth !phases 1 in
  let scalar = Init.finish_scalar_evaluation second |> checked in
  Alcotest.(check int)
    "same-name replacement cannot retarget cached class" 2
    (Init.scalar_byte_size scalar)

let late_backing_keeps_cached_class () =
  List.iter
    (fun backing ->
      let phase = ref None in
      with_records
        (Printf.sprintf
           "extern class Item;class Box{Item item;I64 pad;};%s class Item{I64 \
            value;};"
           backing) (fun resolve state receipt ->
          if receipt.aggregate_publication.aggregate_name.spelling = "Box" then
            phase :=
              Some
                (Init.begin_scalar_evaluation (first_member ~resolve state)
                |> checked));
      let scalar =
        Init.finish_scalar_evaluation (Option.get !phase) |> checked
      in
      Alcotest.(check bool)
        "late backing cannot retarget the cached raw representation" true
        (Type.equal
           (Init.scalar_value_type scalar)
           (Init.scalar_value_type
              (Init.scalar_evaluation_before (Option.get !phase))));
      Alcotest.(check int)
        "late backing does not replace the cached nominal width" 8
        (Init.scalar_byte_size scalar);
      let bytes =
        Holyc_lib__Ir.Integer_initializer_layout.word_bytes scalar (-1L)
        |> checked
      in
      Alcotest.(check string)
        "integer Call words are copied without raw-type truncation"
        (String.make 8 '\255') bytes)
    [ "U16"; "F64" ]

let foreign_read_and_domain () =
  let borrowed = ref None in
  with_records "class Item{U16 value;};" (fun _ state receipt ->
      if receipt.aggregate_publication.aggregate_name.spelling = "Item" then
        borrowed := Some state.record);
  let phase = ref None and foreign_phase = ref None in
  with_records "extern class Item;class Box{Item item;I64 pad;};"
    (fun resolve state receipt ->
      if receipt.aggregate_publication.aggregate_name.spelling = "Box" then (
        let active_read = ref false in
        let read type_ = if !active_read then !borrowed else resolve type_ in
        let node = first_member ~resolve:read state in
        let original = Init.begin_scalar_evaluation node |> checked in
        let foreign = Init.begin_scalar_evaluation node |> checked in
        phase := Some original;
        foreign_phase := Some foreign;
        active_read := true));
  reject "foreign equal-looking class cannot supply reached width"
    (Init.finish_scalar_evaluation (Option.get !foreign_phase));
  reject "a failed settlement cannot retry"
    (Init.finish_scalar_evaluation (Option.get !foreign_phase));
  let original = Option.get !phase in
  let rejected =
    Domain.spawn (fun () ->
        Result.is_error (Init.finish_scalar_evaluation original))
    |> Domain.join
  in
  Alcotest.(check bool)
    "another domain cannot settle the original phase" true rejected

let fixed_scalar () =
  let type_ =
    Type.make_primitive ~form:Type.Internal_storage
      ~primitive:Primitive_type.U16 ~pointer_depth:0
    |> checked
  in
  let node = Init.of_type type_ |> checked in
  let phase = Init.begin_scalar_evaluation node |> checked in
  let before = Init.scalar_evaluation_before phase in
  let rejected =
    Domain.spawn (fun () ->
        Result.is_error (Init.finish_scalar_evaluation phase))
    |> Domain.join
  in
  Alcotest.(check bool)
    "foreign domain cannot consume a fixed scalar phase" true rejected;
  let after = Init.finish_scalar_evaluation phase |> checked in
  Alcotest.(check bool)
    "fixed cached scalar retains its exact type and width" true
    (Init.scalar_equal before after)

let parser_timing () =
  let module Source = Holyc_lib__Sema.Initializer_source in
  let session = Session.create () in
  let pending = ref None and preparing = ref None and phases = ref [] in
  let order = ref [] in
  let append event = order := event :: !order in
  let declaration = function
    | Parser.Global_initializer_started start ->
        pending := Some (Source.begin_parser start |> checked);
        Ok ()
    | Parser.Global_initializer_leaf_preparing receipt ->
        Alcotest.(check bool)
          "preparation is the active original callback" true
          (Parser.initializer_leaf_preparation_is_current receipt);
        Source.observe_parser_preparation (Option.get !pending) receipt
        |> checked;
        reject "preparation cannot repeat during its own callback"
          (Source.observe_parser_preparation (Option.get !pending) receipt);
        preparing := Some receipt;
        phases := receipt :: !phases;
        append ("before " ^ receipt.preparation_first.raw);
        Ok ()
    | Parser.Global_initializer_leaf_completed receipt ->
        let original = Option.get !preparing in
        Alcotest.(check bool)
          "completed leaf retains the exact earlier preparation" true
          (original == receipt.leaf_preparation);
        Alcotest.(check bool)
          "preparation expires before expression completion" false
          (Parser.initializer_leaf_preparation_is_current original);
        let leaf =
          Source.observe_parser_leaf (Option.get !pending) receipt |> checked
        in
        Alcotest.(check bool)
          "source leaf retains its original preparation" true
          (Option.fold ~none:false ~some:(( == ) original)
             (Source.leaf_parser_preparation leaf));
        append "after";
        Ok ()
    | Parser.Global_completed (_, completed) as event
      when Option.is_some completed.global_initial_value ->
        ignore (Source.complete_parser (Option.get !pending) event |> checked);
        pending := None;
        Ok ()
    | _ -> Ok ()
  in
  let commands : Parser.command_sink =
    {
      lexical_lookup = None;
      checkpoint = None;
      reference =
        Some
          (fun _ ->
            append "reference";
            Ok ());
      call = None;
      implicit_output = None;
      declaration = Some declaration;
      query = None;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let source =
    Session.add_source session ~path:"initializer-preparation-timing.hc"
      ~contents:"I64 N;I64 A=N+1;I64 B=42;"
  in
  let parsed =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session)
      ~config:(Preprocessor.Config.create ~compilation_mode:Jit () |> checked)
      source
  in
  Alcotest.(check bool) "timing source parses" false (Parser.has_errors parsed);
  Alcotest.(check (list string))
    "preparation precedes expression references"
    [ "before N"; "reference"; "after"; "before 42"; "after" ]
    (List.rev !order);
  List.iter
    (fun receipt ->
      Alcotest.(check bool)
        "remembered preparation cannot be reused" false
        (Parser.initializer_leaf_preparation_is_current receipt))
    !phases

let failed_expression_recovery () =
  let session = Session.create () in
  let phases = ref [] in
  let commands : Parser.command_sink =
    {
      lexical_lookup = None;
      checkpoint = None;
      reference = None;
      call = None;
      implicit_output = None;
      declaration =
        Some
          (function
          | Parser.Global_initializer_leaf_preparing receipt ->
              phases := receipt :: !phases;
              Ok ()
          | _ -> Ok ());
      query = None;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let source =
    Session.add_source session ~path:"initializer-preparation-recovery.hc"
      ~contents:"I64 A=1+;"
  in
  let parsed =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session)
      ~config:(Preprocessor.Config.create ~compilation_mode:Jit () |> checked)
      source
  in
  Alcotest.(check bool)
    "malformed expression retains its diagnostics" true
    (Parser.has_errors parsed);
  let next_source =
    Session.add_source session ~path:"initializer-after-failure.hc"
      ~contents:"I64 B=42;"
  in
  let next =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session)
      ~config:(Preprocessor.Config.create ~compilation_mode:Jit () |> checked)
      next_source
  in
  Alcotest.(check bool)
    "next source parses after the failed expression" false
    (Parser.has_errors next);
  Alcotest.(check bool)
    "next source reaches its original initializer" true
    (List.exists
       (fun (receipt : Parser.initializer_leaf_preparation) ->
         receipt.preparation_first.raw = "42")
       !phases);
  List.iter
    (fun receipt ->
      Alcotest.(check bool)
        "failed preparation has no later callback authority" false
        (Parser.initializer_leaf_preparation_is_current receipt))
    !phases

let static_parser_phases () =
  let session = Session.create () in
  let saved = ref [] and pending = ref None and order = ref [] in
  let append item = order := item :: !order in
  let declaration = function
    | Parser.Static_initializer_expression_preparing receipt ->
        Alcotest.(check bool)
          "original static preparation is current" true
          (Parser.static_leaf_preparation_is_current receipt);
        Alcotest.(check bool)
          "cloned static preparation has no callback authority" false
          (Parser.static_leaf_preparation_is_current
             (Obj.obj (Obj.dup (Obj.repr receipt))));
        saved := receipt :: !saved;
        pending := Some receipt;
        append ("before " ^ receipt.static_expression_first.raw);
        Ok ()
    | Parser.Static_initializer_preparing receipt ->
        let original = Option.get !pending in
        Alcotest.(check bool)
          "completed static leaf keeps the same preparation" true
          (original == receipt.static_expression_preparation);
        Alcotest.(check bool)
          "static preparation expires before completion" false
          (Parser.static_leaf_preparation_is_current original);
        append "after";
        Ok ()
    | _ -> Ok ()
  in
  let commands : Parser.command_sink =
    {
      lexical_lookup = None;
      checkpoint = None;
      reference =
        Some
          (fun _ ->
            append "reference";
            Ok ());
      call = None;
      implicit_output = None;
      declaration = Some declaration;
      query = None;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let parse name contents =
    let source = Session.add_source session ~path:name ~contents in
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session)
      ~config:(Preprocessor.Config.create ~compilation_mode:Jit () |> checked)
      source
  in
  let good =
    parse "static-phase-order.hc"
      "I64 N;I64 F(){static I64 A=N+1;static I64 B=42;return A;}"
  in
  Alcotest.(check bool)
    "static preparation source parses" false (Parser.has_errors good);
  Alcotest.(check (list string))
    "static preparation precedes expression references"
    [ "before N"; "reference"; "after"; "before 42"; "after"; "reference" ]
    (List.rev !order);
  let bad = parse "static-phase-failure.hc" "I64 G(){static I64 C=1+;}" in
  Alcotest.(check bool)
    "failed static expression retains its error" true (Parser.has_errors bad);
  let next =
    parse "static-phase-next.hc" "I64 H(){static I64 D=42;return D;}"
  in
  Alcotest.(check bool)
    "next source parses after a failed static expression" false
    (Parser.has_errors next);
  List.iter
    (fun receipt ->
      Alcotest.(check bool)
        "saved static phases have no later authority" false
        (Parser.static_leaf_preparation_is_current receipt))
    !saved

let static_delimiter_phases () =
  let session = Session.create () in
  let saved = ref [] and previous_leaf = ref None and order = ref [] in
  let append value = order := value :: !order in
  let declaration = function
    | Parser.Static_initializer_delimiter_completed receipt ->
        let previous = List.nth_opt !saved 0 in
        Alcotest.(check bool)
          "delimiter callback is current" true
          (Parser.static_initializer_delimiter_is_current receipt);
        Alcotest.(check bool)
          "cloned delimiter has no callback authority" false
          (Parser.static_initializer_delimiter_is_current
             (Obj.obj (Obj.dup (Obj.repr receipt))));
        Alcotest.(check bool)
          "exact delimiter predecessor" true
          (Option.equal ( == ) receipt.static_delimiter_predecessor previous);
        Alcotest.(check bool)
          "exact successful leaf predecessor" true
          (Option.equal ( == ) receipt.static_delimiter_leaf_predecessor
             !previous_leaf);
        Alcotest.(check int)
          "monotone delimiter index" (List.length !saved)
          receipt.static_delimiter_index;
        append
          (match receipt.static_delimiter_value with
          | Parser.Initializer_open _ -> "open"
          | Initializer_comma _ -> "comma"
          | Initializer_close _ -> "close");
        saved := receipt :: !saved;
        Ok ()
    | Parser.Static_initializer_expression_preparing receipt ->
        Alcotest.(check bool)
          "expression retains the reached delimiter" true
          (Option.equal ( == ) receipt.static_expression_delimiter_predecessor
             (List.nth_opt !saved 0));
        List.iter
          (fun delimiter ->
            Alcotest.(check bool)
              "delimiter expires before expression parsing" false
              (Parser.static_initializer_delimiter_is_current delimiter))
          !saved;
        append ("before " ^ receipt.static_expression_first.raw);
        Ok ()
    | Parser.Static_initializer_preparing receipt ->
        previous_leaf := Some receipt;
        append "after";
        Ok ()
    | _ -> Ok ()
  in
  let commands : Parser.command_sink =
    {
      lexical_lookup = None;
      checkpoint = None;
      call = None;
      implicit_output = None;
      query = None;
      dimension_count = None;
      reference =
        Some
          (fun _ ->
            append "reference";
            Ok ());
      declaration = Some declaration;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let parse path contents =
    let source = Session.add_source session ~path ~contents in
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session)
      ~config:(Preprocessor.Config.create ~compilation_mode:Jit () |> checked)
      source
  in
  let good =
    parse "static-delimiter-order.hc" "I64 N;I64 F(){static I64 A[2]={N,42};}"
  in
  Alcotest.(check bool) "delimiter source parses" false (Parser.has_errors good);
  Alcotest.(check (list string))
    "delimiter callbacks precede next expression"
    [
      "open";
      "before N";
      "reference";
      "after";
      "comma";
      "before 42";
      "after";
      "close";
    ]
    (List.rev !order);
  let good_saved = !saved in
  saved := [];
  previous_leaf := None;
  order := [];
  let bad =
    parse "static-delimiter-failure.hc" "I64 G(){static I64 B[2]={1,2+;};}"
  in
  Alcotest.(check bool)
    "failed leaf retains its parse error" true (Parser.has_errors bad);
  List.iter
    (fun receipt ->
      Alcotest.(check bool)
        "failed source leaves no delimiter authority" false
        (Parser.static_initializer_delimiter_is_current receipt))
    (good_saved @ !saved);
  saved := [];
  previous_leaf := None;
  order := [];
  let next =
    parse "static-delimiter-next.hc" "I64 H(){static I64 C[1]={42};}"
  in
  Alcotest.(check bool)
    "next source parses after failed delimiters" false (Parser.has_errors next)

let backed_forward_selection () =
  let original = ref None
  and array_target = ref None
  and scalar_phase = ref None in
  let on_declared resolve state source =
    if Option.is_some source.Parser.aggregate_header.binding then (
      let symbol =
        C.publication_aggregate_identity state.publication |> Option.get
      in
      let type_ = Type.make_aggregate ~symbol ~pointer_depth:0 |> checked in
      let node = Init.of_record ~resolve ~type_ state.record |> checked in
      original := Some node;
      scalar_phase := Some (Init.begin_scalar_evaluation node |> checked);
      Alcotest.(check int)
        "forward backing is absent before name lookahead" 0
        (Init.scalar_evaluation_before (Option.get !scalar_phase)
        |> Init.scalar_byte_size))
  in
  with_records ~on_declared "extern U16 class Item;U8 class Item{U8 value;};"
    (fun _ _ receipt ->
      let node = Option.get !original in
      match receipt.Parser.aggregate_item with
      | Ast.Aggregate_forward_declaration forward ->
          Alcotest.(check bool)
            "AST retains the original backing" true
            (Option.equal ( == ) forward.backing
               receipt.aggregate_publication.aggregate_backing);
          let target = Init.select_forwarding node |> checked in
          array_target := Some target;
          Alcotest.(check int)
            "forward declaration installs U16 backing" 2
            (Init.begin_scalar_evaluation target
            |> checked |> Init.scalar_evaluation_before |> Init.scalar_byte_size
            );
          Alcotest.(check int)
            "earlier scalar branch retains the nominal class" 0
            (Init.finish_scalar_evaluation (Option.get !scalar_phase)
            |> checked |> Init.scalar_byte_size)
      | Ast.Aggregate_definition _ ->
          Alcotest.(check int)
            "array rank keeps the cached forwarded target" 2
            (Init.begin_scalar_evaluation (Option.get !array_target)
            |> checked |> Init.scalar_evaluation_before |> Init.scalar_byte_size
            );
          Alcotest.(check int)
            "a later recursion can select new backing" 1
            (Init.select_forwarding node
            |> checked |> Init.begin_scalar_evaluation |> checked
            |> Init.scalar_evaluation_before |> Init.scalar_byte_size)
      | _ -> Alcotest.fail "unexpected backed forward item")

let backed_forward_syntax () =
  List.iter
    (fun mode ->
      let session = Session.create () in
      let source =
        Session.add_source session ~path:"backed-forward-syntax.hc"
          ~contents:
            "#define EXT extern\n\
             #define RAW U16\n\
             U16 class Base{U8 value;};EXT RAW class First;extern Base ** \
             union Second;extern I16i class Third;extern U8 **** class Fourth;"
      in
      let parsed =
        Parser.parse ~sources:(Session.sources session)
          ~definitions:(Session.definitions session)
          ~symbols:(Session.symbols session)
          ~config:
            (Preprocessor.Config.create ~compilation_mode:mode () |> checked)
          source
      in
      Alcotest.(check bool)
        "backed forward syntax parses" false (Parser.has_errors parsed);
      let ast = Option.get parsed.ast in
      let forwards =
        List.filter_map
          (function
            | Ast.Aggregate_forward_declaration forward -> Some forward
            | _ -> None)
          ast.items
      in
      Alcotest.(check (list int))
        "retained backing pointer layers" [ 0; 2; 0; 4 ]
        (List.map
           (fun (forward : Ast.aggregate_forward_declaration) ->
             List.length (Option.get forward.backing).backing_pointer_layers)
           forwards);
      let first = List.hd forwards in
      Alcotest.(check bool)
        "generated binding retains its definition" true
        (Option.is_some first.binding.location.defined_at);
      Alcotest.(check bool)
        "generated backing retains its definition" true
        (match (Option.get first.backing).backing_type_specifier with
        | Ast.Primitive_type_specifier type_ ->
            Option.is_some type_.location.defined_at
        | _ -> false);
      let json =
        Holyc_lib__Frontend.Ast_dump.to_yojson (Session.sources session) ast
      in
      let open Yojson.Safe.Util in
      let items =
        json |> member "module" |> member "items" |> to_list |> List.tl
      in
      List.iter
        (fun item ->
          Alcotest.(check bool)
            "public JSON retains backing" true
            (member "backing" item <> `Null))
        items)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let () =
  Alcotest.run "Original initializer scalar phases"
    [
      ( "source metadata",
        [
          Alcotest.test_case "reached width and original class replacement"
            `Quick reached_width_and_replacement;
          Alcotest.test_case "foreign records, replay and domain ownership"
            `Quick foreign_read_and_domain;
          Alcotest.test_case "late backing retains nominal width and raw words"
            `Quick late_backing_keeps_cached_class;
          Alcotest.test_case "fixed scalar class remains cached" `Quick
            fixed_scalar;
          Alcotest.test_case "original preparation before expression references"
            `Quick parser_timing;
          Alcotest.test_case "failed expressions expire before the next source"
            `Quick failed_expression_recovery;
          Alcotest.test_case
            "static preparation timing, identity and failed-source expiry"
            `Quick static_parser_phases;
          Alcotest.test_case
            "static delimiter order, identity and failure expiry" `Quick
            static_delimiter_phases;
          Alcotest.test_case
            "backed forward installation and cached array target" `Quick
            backed_forward_selection;
          Alcotest.test_case "backed forward syntax, provenance and public JSON"
            `Quick backed_forward_syntax;
        ] );
    ]
