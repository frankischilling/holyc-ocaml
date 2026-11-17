open Holyc_lib
module Shape = Holyc_lib__Ir.Integer_storage_shape
module Layout = Holyc_lib__Ir.Integer_initializer_layout
module Source = Holyc_lib__Sema.Initializer_source
module Globals = Semantic_global_type_resolution
module Init_type = Holyc_lib__Sema.Initializer_type

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let parsed = function
  | Ok value -> value
  | Error errors ->
      Alcotest.fail
        (String.concat "; "
           (List.map
              (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
              errors))

let inputs contents dimensions =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"aggregate-initializer-layout.hc" ~contents
  in
  let config = Preprocessor.Config.create () |> checked in
  let ast = parse_with_config session ~config ~source |> parsed in
  let declarations = collect_declarations session ast |> checked in
  let aggregates = resolve_aggregates session ~declarations ast |> checked in
  let headers =
    resolve_aggregate_headers session ~declarations ~aggregates ast |> checked
  in
  let members = collect_members session ~declarations ast |> checked in
  let members =
    resolve_member_types session ~declarations ~aggregates ~headers ~members ast
    |> checked
  in
  let layouts =
    layout_aggregates session ~declarations ~aggregates ~headers ~members ast
    |> checked
  in
  let members =
    index_aggregate_members session ~declarations ~headers ~members ~layouts
    |> checked
  in
  let globals =
    resolve_global_types session ~declarations ~aggregates ast |> checked
  in
  let global = List.hd (Globals.globals globals) in
  let initial =
    Globals.global_initializer global
    |> Option.get |> Globals.initializer_source |> Option.get
  in
  let shape =
    Shape.create_global ~headers ~members ~global ~dimensions () |> function
    | Ok shape -> shape
    | Error _ -> Alcotest.fail "checked source has no aggregate shape"
  in
  (headers, members, global, shape, initial)

let destinations source dimensions expected () =
  let _, _, _, shape, initial = inputs source dimensions in
  let layout = Layout.create ~shape initial |> checked in
  let destinations =
    Layout.entries layout
    |> List.map (fun entry ->
        let bytes =
          match Layout.operation entry with
          | Layout.Copy_bytes bytes -> String.length bytes
          | Layout.Scalar_store ->
              Layout.scalar entry |> Option.get |> Init_type.scalar_byte_size
        in
        (Layout.cell_offset entry, Layout.byte_offset entry, bytes))
  in
  Alcotest.(check (list (triple int int int)))
    "independent pinned byte-cursor destinations" expected destinations;
  List.iter
    (fun entry ->
      Alcotest.(check bool)
        "each destination retains its original leaf" true
        (Source.owns_leaf initial (Layout.leaf entry)))
    (Layout.entries layout)

let rejects source dimensions () =
  let _, _, _, shape, initial = inputs source dimensions in
  Alcotest.(check bool)
    "invalid original traversal rejects" true
    (Result.is_error (Layout.create ~shape initial))

let ownership () =
  let source = "class Box{U8 tag;U16 value;};Box object={7,42};" in
  let _, members, global, _, initial = inputs source [] in
  let foreign, _, _, _, _ = inputs source [] in
  let shape =
    Shape.create_global ~headers:foreign ~members ~global ~dimensions:[] ()
    |> Result.get_ok
  in
  Alcotest.(check bool)
    "a foreign equal-looking header supplies no initializer type" true
    (Result.is_error (Layout.create ~shape initial))

let raw_class_string () =
  let _, _, _, shape, initial =
    inputs "class B{U8 tag;};class Box:B{};Box objects[1]=\"A\";" [ 1L ]
  in
  let layout = Layout.create ~shape initial |> checked in
  match List.map Layout.operation (Layout.entries layout) with
  | [ Layout.Scalar_store ] -> ()
  | _ ->
      Alcotest.fail
        "a nominal one-byte class must retain its raw I64 scalar expression"

let callback_ownership () =
  let source = "class Box{U8 tag;U16 (*value)(I64);};Box object={7,42};" in
  let selected () =
    let _, _, _, shape, initial = inputs source [] in
    let layout = Layout.create ~shape initial |> checked in
    Layout.entries layout |> List.tl |> List.hd |> Layout.scalar |> Option.get
  in
  let original = selected () and foreign = selected () in
  Alcotest.(check int)
    "numeric callback storage keeps the pointer width" 8
    (Init_type.scalar_byte_size original);
  Alcotest.(check int)
    "its source-selected storage remains a pointer" 1
    (Semantic_type.pointer_depth (Init_type.scalar_type original));
  Alcotest.(check bool)
    "an equal-looking foreign callback header cannot replace this leaf" false
    (Init_type.scalar_equal original foreign)

let () =
  Alcotest.run "aggregate initializer layout"
    [
      ( "source cursor",
        [
          Alcotest.test_case
            "nominal class width cannot supply a raw byte string type" `Quick
            raw_class_string;
          Alcotest.test_case "packed independent field widths" `Quick
            (destinations "class Box{U8 tag;U16 value;};Box object={7,42};" []
               [ (0, 0, 1); (1, 1, 2) ]);
          Alcotest.test_case "padding does not reposition initializer writes"
            `Quick
            (destinations "class Box{U8 tag;$$=6;U16 value;};Box object={7,42};"
               []
               [ (0, 0, 1); (1, 1, 2) ]);
          Alcotest.test_case
            "inherited storage is independent of the direct member list" `Quick
            (destinations
               "class B{U8 tag;};class Box:B{U16 value;};Box object={42};" []
               [ (0, 0, 2) ]);
          Alcotest.test_case
            "an inherited empty direct list copies the raw class width" `Quick
            (destinations
               "class B{U8 tag;U16 value;};class Box:B{};Box object=42;" []
               [ (0, 0, 3) ]);
          Alcotest.test_case "backing forwarding precedes member initialization"
            `Quick
            (destinations "U16 class Box{U16 value;U8 guard;};Box object=42;" []
               [ (0, 0, 2) ]);
          Alcotest.test_case "backed arrays advance by the forwarded width"
            `Quick
            (destinations
               "U16 class Box{U16 value;U8 guard;};Box objects[2]={40,42};"
               [ 2L ]
               [ (0, 0, 2); (2, 2, 2) ]);
          Alcotest.test_case "nested member and root arrays" `Quick
            (destinations
               "class I{U16 value;};class Box{U8 tag;I children[2];};Box \
                objects[2]={{7,{{40},{41}}},{8,{{42},{43}}}};"
               [ 2L ]
               [
                 (0, 0, 1); (1, 1, 2); (3, 3, 2); (5, 5, 1); (6, 6, 2); (8, 8, 2);
               ]);
          Alcotest.test_case "member byte strings retain their own fixed count"
            `Quick
            (destinations
               "class Box{U8 text[3];U16 value;};Box object={\"AB\",42};" []
               [ (0, 0, 3); (3, 3, 2) ]);
          Alcotest.test_case "callback return size cannot resize stored words"
            `Quick
            (destinations
               "class Box{U8 tag;U16 (*value)(I64);U8 tail;};Box \
                object={7,42,9};"
               []
               [ (0, 0, 1); (1, 1, 8); (9, 9, 1) ]);
          Alcotest.test_case "void callback headers retain pointer storage"
            `Quick
            (destinations
               "class Box{U8 tag;U0 (**value)(I64);U8 tail;};Box \
                object={7,42,9};"
               []
               [ (0, 0, 1); (1, 1, 8); (9, 9, 1) ]);
          Alcotest.test_case "callback arrays keep their original word count"
            `Quick
            (destinations
               "class Box{U8 tag;U16 (*values)(I64)[2];U8 tail;};Box \
                object={7,{40,42},9};"
               []
               [ (0, 0, 1); (1, 1, 8); (9, 9, 8); (17, 17, 1) ]);
          Alcotest.test_case
            "anonymous unions retain the direct source member chain" `Quick
            (destinations
               "class Box{union{U8 first;U16 second;};$$=8;U8 tail[3];};Box \
                object={7,42,\"AB\"};"
               []
               [ (0, 0, 1); (1, 1, 2); (3, 3, 3) ]);
        ] );
      ( "rejections",
        [
          Alcotest.test_case "plain class requires member braces" `Quick
            (rejects "class Box{U16 value;};Box object=42;" []);
          Alcotest.test_case
            "backed scalar cannot replace its grammar with member braces" `Quick
            (rejects "U16 class Box{U16 value;};Box object={42};" []);
          Alcotest.test_case "missing members are not zero-filled" `Quick
            (rejects "class Box{U8 tag;U16 value;};Box object={7};" []);
          Alcotest.test_case "extra leaves do not gain destinations" `Quick
            (rejects "class Box{U8 tag;U16 value;};Box object={7,42,99};" []);
          Alcotest.test_case
            "union sequential writes cannot exceed the original extent" `Quick
            (rejects "union Box{U8 first;U16 second;};Box object={7,42};" []);
          Alcotest.test_case "foreign original headers reject" `Quick ownership;
          Alcotest.test_case "callback initializer retains its original header"
            `Quick callback_ownership;
        ] );
    ]
