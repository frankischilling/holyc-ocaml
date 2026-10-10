open Holyc_lib
module C = Semantic_declaration_collection
module Record = Semantic_compiler_record
module Source = Holyc_lib__Sema.Source_type_reference

module Type_specifiers = Hashtbl.Make (struct
  type t = Ast.type_specifier

  let equal left right = left == right
  let hash = Hashtbl.hash
end)

type aggregate = {
  source : Parser.aggregate_publication;
  publication : C.publication;
  progress : Record.aggregate_progress;
  initial : Record.t;
  mutable current : Record.t;
  mutable definition : Ast.aggregate_definition option;
}

type function_ = {
  source : Parser.function_publication;
  publication : C.publication;
}

type query = {
  root : Parser.query_root;
  original : Record.sizeof_read;
  mutable current : Record.sizeof_read;
  mutable pending :
    (Parser.query_member_start * Record.sizeof_member_start) option;
  mutable starts_rev :
    (Parser.query_member_start
    * Record.sizeof_read
    * Record.sizeof_member_start)
    list;
  mutable members_rev : (Parser.query_member * Record.sizeof_member_start) list;
  mutable reads_rev : Record.sizeof_read list;
}

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

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
        |> List.map (fun (diagnostic : Diagnostic.t) ->
            diagnostic.code ^ ": " ^ diagnostic.message)
        |> String.concat "; ")

let member_query_authority mode () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let foreign_namespace = C.create_namespace ~table () |> checked in
  let foreign_table = Session.semantic_symbols (Session.create ()) in
  let aggregates : aggregate list ref = ref [] in
  let functions : function_ list ref = ref [] in
  let types = Type_specifiers.create 16 in
  let queries : query list ref = ref [] in
  let completed = ref [] in
  let timing = ref [] in
  let foreign_start_checks = ref 0 in
  let out_of_order_checks = ref 0 in
  let stale_record_checks = ref 0 in
  let equal_sized_foreign_checks = ref 0 in
  let omitted_checks = ref 0 in
  let same_identity (left : aggregate) right =
    match C.publication_aggregate_identity left.publication with
    | Some symbol -> symbol == right
    | None -> false
  in
  let find_aggregate name =
    List.find
      (fun (state : aggregate) -> state.source.aggregate_name.spelling = name)
      !aggregates
  in
  let aggregate_for_symbol symbol =
    List.find_opt
      (fun (state : aggregate) -> same_identity state symbol)
      !aggregates
  in
  let current_aggregate symbol =
    Option.map
      (fun (state : aggregate) -> state.current)
      (aggregate_for_symbol symbol)
  in
  let select_original_type source =
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
        Type_specifiers.replace types type_specifier proof
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
          {
            source;
            publication;
            progress;
            initial;
            current = initial;
            definition = None;
          }
          :: !aggregates
    | Parser.Aggregate_advanced phase ->
        let own =
          List.find
            (fun (state : aggregate) -> state.source == phase.phase_aggregate)
            !aggregates
        in
        let member =
          match phase.phase_step with
          | Parser.Aggregate_member_prepared member -> (
              match member.member_selection with
              | None -> None
              | Some selection ->
                  let selected =
                    List.find
                      (fun (state : aggregate) ->
                        state.source.aggregate_entry == selection.entry)
                      !aggregates
                  in
                  let source = Source.Aggregate_member phase in
                  let proof =
                    Source.select_aggregate ~table ~namespace ~source
                      selected.publication
                    |> checked
                  in
                  Some
                    (Record.select_aggregate_member ~table ~namespace
                       ~selected_aggregate:proof phase (Some selected.current)
                    |> checked))
          | _ -> None
        in
        let base =
          match phase.phase_step with
          | Parser.Aggregate_base_attached selection ->
              let selected =
                List.find
                  (fun (state : aggregate) ->
                    state.source.aggregate_entry == selection.base_entry)
                  !aggregates
              in
              Some
                (Record.select_aggregate_base ~table ~namespace
                   ~selected_publication:selected.publication phase
                   selected.current
                |> checked)
          | _ -> None
        in
        Record.advance_aggregate
          ~members:(fun requested ->
            match member with
            | Some selected when requested == phase -> Ok selected
            | _ -> Error "no originally selected member")
          ~bases:(fun requested ->
            match base with
            | Some selected when requested == phase -> Ok selected
            | _ -> Error "no originally selected base")
          ~dimensions:(fun _ -> None)
          own.progress phase
        |> checked;
        own.current <- Record.aggregate_metadata own.progress |> checked
    | Parser.Aggregate_completed receipt -> (
        let state =
          List.find
            (fun (state : aggregate) ->
              state.source == receipt.aggregate_publication)
            !aggregates
        in
        state.current <-
          Record.complete_aggregate ~progress:state.progress ~table ~namespace
            state.publication receipt
          |> checked;
        match receipt.aggregate_item with
        | Ast.Aggregate_definition definition ->
            state.definition <- Some definition
        | Ast.Aggregate_forward_declaration _ -> ()
        | _ -> Alcotest.fail "expected the original aggregate completion")
    | Parser.Function_declared source ->
        let publication = C.publish_function namespace source |> checked in
        functions := { source; publication } :: !functions
    | Parser.Function_parameter_declared source ->
        select_original_type (Source.Function_parameter source)
    | Parser.Function_local_allocated source ->
        select_original_type (Source.Function_local source)
    | _ -> ());
    Ok ()
  in
  let read_root (root : Parser.query_root) =
    match root.query_local with
    | Some local ->
        let function_ =
          List.find
            (fun (value : function_) ->
              value.source.function_header.declaration_command
              == root.query_command)
            !functions
        in
        let type_specifier, pointer_layers =
          match local.local_source with
          | Parser.Local_variable local ->
              (local.local_type_specifier, local.local_pointer_layers)
          | Parser.Local_parameter parameter ->
              (parameter.type_specifier, parameter.pointer_layers)
          | Parser.Variadic_count _ | Parser.Variadic_vector _ ->
              Alcotest.fail "expected a selected aggregate object root"
        in
        let aggregate =
          if pointer_layers <> [] then None
          else
            Option.bind (Type_specifiers.find_opt types type_specifier)
              (fun proof ->
                Option.map
                  (fun (state : aggregate) -> (proof, state.current))
                  (aggregate_for_symbol (Source.selected_base_symbol proof)))
        in
        Record.read_local_sizeof ~table ~namespace
          ~function_publication:function_.publication ~root ~aggregate
          ~dimensions:[]
        |> checked
    | None -> (
        match root.query_lookup with
        | Symbol_visibility.Present entry ->
            let aggregate =
              List.find
                (fun (state : aggregate) ->
                  state.source.aggregate_entry == entry)
                !aggregates
            in
            Record.read_sizeof ~table ~root aggregate.current |> checked
        | _ -> Alcotest.fail "expected a selected aggregate type root")
  in
  let query_for (root : Parser.query_root) =
    List.find (fun (query : query) -> query.root == root) !queries
  in
  let root_name (root : Parser.query_root) =
    match root.query_node with
    | Parser.Sizeof_target name -> name.spelling
    | _ -> Alcotest.fail "expected a sizeof query"
  in
  let query event =
    (match event with
    | Parser.Query_root root ->
        Alcotest.(check bool)
          "the original sizeof root is current" true
          (Parser.query_root_is_current root);
        let read = read_root root in
        queries :=
          {
            root;
            original = read;
            current = read;
            pending = None;
            starts_rev = [];
            members_rev = [];
            reads_rev = [];
          }
          :: !queries
    | Parser.Query_member_started source ->
        let state = query_for source.member_start_root in
        Alcotest.(check bool)
          "dot start is active at its source callback" true
          (Parser.query_member_start_is_current source);
        (match
           List.find_opt (fun other -> other.root != state.root) !queries
         with
        | Some foreign ->
            reject "another root cannot authorize this member start"
              (Record.start_sizeof_member ~table ~namespace ~source
                 foreign.original);
            incr foreign_start_checks
        | None -> ());
        if source.member_start_ordinal > 0 then (
          reject "a member start cannot skip the preceding read"
            (Record.start_sizeof_member ~table ~namespace ~source state.original);
          incr out_of_order_checks);
        reject "foreign namespace cannot start member lookup"
          (Record.start_sizeof_member ~table ~namespace:foreign_namespace
             ~source state.current);
        reject "foreign table cannot start member lookup"
          (Record.start_sizeof_member ~table:foreign_table ~namespace ~source
             state.current);
        let prefix = state.current in
        let start =
          Record.start_sizeof_member ~table ~namespace ~source prefix |> checked
        in
        state.starts_rev <- (source, prefix, start) :: state.starts_rev;
        state.pending <- Some (source, start)
    | Parser.Query_member source ->
        let state = query_for source.query_member_root in
        Alcotest.(check bool)
          "member completion is active at its source callback" true
          (Parser.query_member_is_current source);
        let pending_source, start = Option.get state.pending in
        Alcotest.(check bool)
          "member must complete the exact original dot" true
          (source.query_member_start == pending_source);
        let read ?(table = table) ?(namespace = namespace)
            ?(aggregates = current_aggregate) proof =
          Record.read_sizeof_member ~table ~namespace ~aggregates ~source proof
        in
        reject "foreign table cannot read selected member"
          (read ~table:foreign_table start);
        reject "foreign namespace cannot read selected member"
          (read ~namespace:foreign_namespace start);
        (match state.members_rev with
        | (_, earlier_start) :: _ ->
            reject "an earlier ordinal cannot authorize this member"
              (read earlier_start)
        | [] -> ());
        (match source.query_member_node with
        | Parser.Sizeof_member member
          when member.sizeof_member_name.spelling = "child"
               || member.sizeof_member_name.spelling = "field" ->
            let expected =
              if member.sizeof_member_name.spelling = "child" then
                find_aggregate "Leaf"
              else find_aggregate "Late"
            in
            if expected.initial != expected.current then (
              reject "stale selected class metadata cannot supply member size"
                (read
                   ~aggregates:(fun symbol ->
                     if same_identity expected symbol then Some expected.initial
                     else current_aggregate symbol)
                   start);
              incr stale_record_checks);
            let equal_sized = find_aggregate "Equal" in
            if member.sizeof_member_name.spelling = "child" then (
              reject "equal-sized foreign class cannot replace selected member"
                (read
                   ~aggregates:(fun symbol ->
                     if same_identity expected symbol then
                       Some equal_sized.current
                     else current_aggregate symbol)
                   start);
              incr equal_sized_foreign_checks)
        | _ -> ());
        let next = read start |> checked in
        state.members_rev <- (source, start) :: state.members_rev;
        state.reads_rev <- next :: state.reads_rev;
        state.current <- next;
        state.pending <- None
    | Parser.Query_completed receipt ->
        let state = query_for receipt.query_root in
        Alcotest.(check bool)
          "no unfinished dot" true
          (Option.is_none state.pending);
        reject "root read cannot omit the original member path"
          (Record.complete_sizeof ~table ~receipt state.original);
        incr omitted_checks;
        (match state.reads_rev with
        | _last :: previous :: _ ->
            reject "a partial path cannot complete a full sizeof query"
              (Record.complete_sizeof ~table ~receipt previous)
        | _ -> ());
        Record.complete_sizeof ~table ~receipt state.current |> checked;
        let pointer =
          match receipt.query_expression with
          | Ast.Sizeof_expression expression ->
              expression.sizeof_pointer_layers <> []
          | _ -> Alcotest.fail "expected a completed sizeof expression"
        in
        let value = Record.sizeof_value state.current ~pointer in
        if root_name state.root = "held" then timing := value :: !timing;
        completed := (state, value, pointer) :: !completed);
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
    Session.add_source session ~path:"member-sizeof-source-authority.hc"
      ~contents:
        {|class Leaf { U16 value; };
class Equal { U16 value; };
class Wrap { Leaf child; Leaf *pointer; };
class Outer:Wrap { U8 tag; };
U0 F(Outer parameter) {
  Outer object;
  sizeof(object.child.value);
  sizeof(object.pointer);
  sizeof(object.tag);
  sizeof(Outer.child.value);
  sizeof(parameter.child);
  sizeof(object.child.value *);
}
U0 G() { Outer another; sizeof(another.child.value); }
U0 Timing() {
  extern class Late;
  class Holder { Late field; };
  Holder held;
  sizeof(held.field);
  class Late { U16 value; };
  sizeof(held.field);
}|}
  in
  parse session mode source commands;
  let results = List.rev_map (fun (_, value, _) -> value) !completed in
  let expected = [ 2L; 8L; 1L; 2L; 2L; 8L; 2L; 0L; 2L ] in
  Alcotest.(check (list int64))
    "member queries in exact source order" expected results;
  Alcotest.(check (list int64))
    "forward member completion changes only later sizeof values" [ 0L; 2L ]
    (List.rev !timing);
  let holder = find_aggregate "Holder" in
  let late = find_aggregate "Late" in
  let equal = find_aggregate "Equal" in
  let original_definition =
    match holder.definition with
    | Some definition -> definition
    | None -> Alcotest.fail "Holder has no original completed definition"
  in
  let type_specifier, declarator =
    match original_definition.members with
    | [ Ast.Aggregate_member_declaration declaration ] -> (
        match declaration.member_declarators with
        | [ declarator ] -> (declaration.member_type_specifier, declarator)
        | _ -> Alcotest.fail "Holder needs its sole original declarator")
    | _ -> Alcotest.fail "Holder needs its original member declaration"
  in
  let field =
    match Record.aggregate_fields holder.current with
    | [ field ] -> field
    | _ -> Alcotest.fail "Holder needs one saved original field placement"
  in
  Alcotest.(check bool)
    "field proof retains the physical original name" true
    (Record.aggregate_field_name field == declarator.member_name);
  let class_symbol (state : aggregate) =
    match C.publication_aggregate_identity state.publication with
    | Some symbol -> symbol
    | None -> Alcotest.fail "aggregate lacks its canonical class identity"
  in
  let aggregate = class_symbol holder in
  let type_ =
    Semantic_type.make_aggregate ~symbol:(class_symbol late) ~pointer_depth:0
    |> checked
  in
  let different_type =
    Semantic_type.make_aggregate ~symbol:(class_symbol equal) ~pointer_depth:0
    |> checked
  in
  let parent = C.namespace_scope namespace in
  let read_field ?(table = table) ?(parent = parent) ?(aggregate = aggregate)
      ?(type_specifier = type_specifier) ?(declarator = declarator)
      ?(type_ = type_) () =
    Record.aggregate_field_element_size ~table ~parent ~aggregate
      ~type_specifier ~declarator ~type_ field
  in
  Alcotest.(check int64)
    "original field stays zero after forward completion" 0L
    (read_field () |> checked);
  Alcotest.(check int64)
    "selected forward class is now two bytes" 2L
    (Record.return_class_size ~table ~namespace ~type_
       ~aggregate:(Some late.current)
    |> checked);
  reject "foreign semantic table cannot read field placement"
    (read_field ~table:foreign_table ());
  reject "different parent namespace cannot read field placement"
    (read_field ~parent:(C.namespace_scope foreign_namespace) ());
  reject "other canonical aggregate cannot claim field placement"
    (read_field ~aggregate:(class_symbol equal) ());
  reject "equal-sized but distinct member type cannot reuse field placement"
    (read_field ~type_:different_type ());
  let copied_name =
    Ast.make_identifier ~spelling:declarator.member_name.spelling
      ~location:declarator.member_name.location
  in
  let remake_declarator ~name ~pointer_layers ~array_dimensions =
    Ast.make_aggregate_member_declarator ~pointer_layers ~name
      ~function_pointer:declarator.member_function_pointer ~array_dimensions
      ~metadata:declarator.member_metadata
      ~delimiter:declarator.member_delimiter
      ~location:declarator.member_declarator_location
  in
  let copied_name_declarator =
    remake_declarator ~name:copied_name
      ~pointer_layers:declarator.member_pointer_layers
      ~array_dimensions:declarator.member_array_dimensions
  in
  reject "same-spelling cloned member name cannot authorize field placement"
    (read_field ~declarator:copied_name_declarator ());
  let copied_type =
    match type_specifier with
    | Ast.Named_type_specifier name ->
        Ast.Named_type_specifier
          (Ast.make_identifier ~spelling:name.spelling ~location:name.location)
    | _ -> Alcotest.fail "Holder's original member type must be named"
  in
  reject "same-spelling cloned type child cannot authorize field placement"
    (read_field ~type_specifier:copied_type ());
  let added_pointer =
    Ast.make_pointer_layer ~depth:1 ~spelling:"*"
      ~location:declarator.member_declarator_location
  in
  reject "substituted pointer child cannot authorize field placement"
    (read_field
       ~declarator:
         (remake_declarator ~name:declarator.member_name
            ~pointer_layers:[ added_pointer ]
            ~array_dimensions:declarator.member_array_dimensions)
       ());
  let added_dimension =
    Ast.make_array_dimension
      ~opening_bracket:declarator.member_declarator_location
      ~dimension_expression:None
      ~closing_bracket:declarator.member_declarator_location
      ~location:declarator.member_declarator_location
  in
  reject "substituted array child cannot authorize field placement"
    (read_field
       ~declarator:
         (remake_declarator ~name:declarator.member_name
            ~pointer_layers:declarator.member_pointer_layers
            ~array_dimensions:[ added_dimension ])
       ());
  let origin (location : Ast.location) =
    Semantic_symbol.Source_location
      {
        span = location.span;
        source_segments = location.source_segments;
        generated_from = location.generated_from;
        defined_at = location.defined_at;
      }
  in
  let member_scope =
    Semantic_symbol_table.create_scope table ~parent
      ~kind:Semantic_symbol_table.Aggregate ~name:"Holder proof control" ()
    |> checked
  in
  let member_symbol =
    Semantic_symbol_table.add table ~scope:member_scope
      ~name:declarator.member_name.spelling ~kind:Semantic_symbol.Member
      ~origin:(origin declarator.member_name.location)
    |> checked
  in
  let member_input : Semantic_aggregate_layout.member_input =
    {
      member_symbol;
      member_path = [ 0 ];
      member_declarator_index = 0;
      member_origin = origin declarator.member_declarator_location;
      member_type = type_;
      member_is_function_pointer = false;
      member_dimensions = [];
    }
  in
  let original_member =
    Semantic_aggregate_layout.original_member ~table ~parent ~aggregate
      ~type_specifier ~declarator member_input field
    |> checked
  in
  let semantic_input : Semantic_aggregate_layout.aggregate_input =
    {
      aggregate_symbol = aggregate;
      aggregate_scope = member_scope;
      aggregate_kind = Semantic_aggregate_layout.Class;
      aggregate_item_index = 0;
      aggregate_origin = origin original_definition.location;
      aggregate_base = None;
      aggregate_items = [ Semantic_aggregate_layout.Field member_input ];
    }
  in
  let checked_layout = function
    | Ok result -> result
    | Error error ->
        Alcotest.fail (Semantic_aggregate_layout.error_to_string error)
  in
  (match Semantic_aggregate_layout.layout ~table ~parent [ semantic_input ] with
  | Ok _ ->
      Alcotest.fail
        "unwitnessed forward aggregate member unexpectedly gained a layout"
  | Error error ->
      Alcotest.(check string)
        "ordinary unwitnessed layout still refuses unresolved forward type"
        "HCSEMA0002"
        (Semantic_aggregate_layout.error_code error));
  let original_layout =
    Semantic_aggregate_layout.layout ~original_members:[ original_member ]
      ~table ~parent [ semantic_input ]
    |> checked_layout |> Semantic_aggregate_layout.layouts
  in
  let selected_layout =
    match original_layout with
    | [ layout ] -> layout
    | _ -> Alcotest.fail "expected one authorized Holder metadata layout"
  in
  Alcotest.(check int64)
    "witnessed Holder keeps original zero-byte size" 0L selected_layout.size;
  let selected_field =
    match selected_layout.members with
    | [ member ] -> member
    | _ -> Alcotest.fail "expected one authorized Holder field"
  in
  Alcotest.(check int64)
    "semantic member layout retains original zero-byte element" 0L
    selected_field.element_size;
  let copied_input =
    { member_input with member_path = List.map Fun.id member_input.member_path }
  in
  Alcotest.(check bool)
    "cloned semantic member has a new physical identity" true
    (copied_input != member_input);
  let substituted_input =
    {
      semantic_input with
      aggregate_items = [ Semantic_aggregate_layout.Field copied_input ];
    }
  in
  reject "original member cannot authorize a cloned semantic input"
    (Semantic_aggregate_layout.layout ~original_members:[ original_member ]
       ~table ~parent [ substituted_input ]);
  reject "original member cannot authorize a different aggregate identity"
    (Semantic_aggregate_layout.layout ~original_members:[ original_member ]
       ~table ~parent
       [ { semantic_input with aggregate_symbol = class_symbol equal } ]);
  Alcotest.(check bool)
    "cross-root proof rejection was exercised" true
    (!foreign_start_checks > 0);
  Alcotest.(check bool)
    "skipped member proof rejection was exercised" true
    (!out_of_order_checks > 0);
  Alcotest.(check bool)
    "stale current-class record rejection was exercised" true
    (!stale_record_checks > 0);
  Alcotest.(check bool)
    "equal-sized foreign-class rejection was exercised" true
    (!equal_sized_foreign_checks > 0);
  Alcotest.(check int)
    "each completion checked for omitted members" (List.length expected)
    !omitted_checks;
  List.iter
    (fun (state, expected, pointer) ->
      Alcotest.(check int64)
        "finished value is an immutable snapshot" expected
        (Record.sizeof_value state.current ~pointer);
      Alcotest.(check bool)
        "the root callback expired" false
        (Parser.query_root_is_current state.root);
      List.iter
        (fun (source, prefix, _start) ->
          Alcotest.(check bool)
            "saved dot callback expired" false
            (Parser.query_member_start_is_current source);
          reject "an expired dot cannot mint another member start"
            (Record.start_sizeof_member ~table ~namespace ~source prefix))
        state.starts_rev;
      List.iter
        (fun (source, start) ->
          Alcotest.(check bool)
            "saved member callback expired" false
            (Parser.query_member_is_current source);
          reject "an expired member cannot mint another size read"
            (Record.read_sizeof_member ~table ~namespace
               ~aggregates:current_aggregate ~source start))
        state.members_rev)
    !completed

let () =
  Alcotest.run "Aggregate member sizeof source authority"
    [
      ( "original named member queries",
        [
          Alcotest.test_case "JIT live reads and provenance" `Quick
            (member_query_authority Preprocessor.Jit);
          Alcotest.test_case "AOT live reads and provenance" `Quick
            (member_query_authority Preprocessor.Aot);
        ] );
    ]
