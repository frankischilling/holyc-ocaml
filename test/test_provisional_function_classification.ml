open Holyc_lib
module P = Test_provisional_function_resolution
module R = Semantic_function_resolution
module C = Semantic_function_record_classification
module N = Semantic_function_record_phase
module H = Semantic_function_type_resolution

let checked = P.checked

let state ?(staging_mask = 0L)
    ?(compiler_option_mask = Compiler_option.initial_mask) () =
  C.make_declaration_state ~staging_mask ~compiler_option_mask ()

let public = Function_flag.apply_modifier ~mask:0L Function_flag.Modifier.Public

let original_state () =
  let f = P.fixture () in
  let function_ = P.typed f (List.hd f.samples) in
  let fact =
    R.make_provisional_declaration ~table:f.table ~namespace:f.namespace
      ~compiler_option_mask:Compiler_option.initial_mask ~function_
    |> checked
  in
  let resolution = P.resolve f fact |> checked in
  P.reject "partial source rejects forged public staging"
    (C.classify resolution [ state ~staging_mask:public () ]);
  let options, _ =
    Compiler_option.set ~mask:Compiler_option.initial_mask
      Compiler_option.Keep_private true
  in
  P.reject "partial source retains full option snapshot"
    (C.classify resolution [ state ~compiler_option_mask:options () ]);
  ignore (C.classify resolution [ state () ] |> checked)

let resumed_outer_flags () =
  let f = P.fixture ~contents:"public I64 F(I64 n=40)#exe {I64 F(){}}{}" () in
  let beginnings =
    List.filter_map
      (function
        | Parser.Function_declared _, snapshot -> Some snapshot
        | _ -> None)
      f.events
  in
  let outer_start, inner_start =
    match beginnings with
    | [ a; b ] -> (a, b)
    | _ -> Alcotest.fail "expected two declarations"
  in
  let classify ?(previous = []) resolution state =
    C.classify ~previous resolution [ state ]
    |> checked |> C.declarations |> List.hd
  in
  let function_ = P.typed f outer_start in
  let fact =
    R.make_provisional_declaration ~table:f.table ~namespace:f.namespace
      ~compiler_option_mask:Compiler_option.initial_mask ~function_
    |> checked
  in
  let resolution = P.resolve f fact |> checked in
  let outer = P.single resolution in
  let outer_record = classify resolution (state ~staging_mask:public ()) in
  let transition =
    N.transition ~earlier:outer_start ~later:inner_start |> checked
  in
  let fact =
    R.make_provisional_advance ~table:f.table ~namespace:f.namespace
      ~compiler_option_mask:Compiler_option.initial_mask ~current:outer
      ~transition ~function_:(P.typed f inner_start) ()
    |> checked
  in
  let resolution = P.resolve ~previous:[ outer ] f fact |> checked in
  let inner = P.single resolution in
  let inner_record =
    classify ~previous:[ outer_record ] resolution (state ())
  in
  let inner_snapshot, source, ordinary = List.hd f.headers in
  let inner_fixture =
    { f with P.final_snapshot = inner_snapshot; source; ordinary }
  in
  let fact = P.header_fact inner_fixture inner inner |> checked in
  let resolution = P.resolve ~previous:[ inner ] f fact |> checked in
  let inner_header = P.single resolution in
  let header_record =
    classify ~previous:[ inner_record ] resolution (state ())
  in
  let resolution =
    R.complete_pending ~table:f.table ~namespace:f.namespace
      ~pending:inner_header ~function_:ordinary
    |> checked
  in
  let inner_body = P.single resolution in
  let body_record =
    classify ~previous:[ header_record ] resolution (state ())
  in
  let transition =
    N.transition ~earlier:inner_snapshot ~later:f.final_snapshot |> checked
  in
  let callable_function =
    P.typed ~scope:(H.function_scope f.ordinary) f f.final_snapshot
  in
  let fact =
    R.make_header_advance ~table:f.table ~namespace:f.namespace ~pending:outer
      ~current:inner_body ~transition ~source:f.source ~function_:f.ordinary
      ~callable_function
    |> checked
  in
  let resolution = P.resolve ~previous:[ inner_body ] f fact |> checked in
  let record =
    classify ~previous:[ body_record ] resolution
      (state ~staging_mask:public ())
    |> C.classified_declaration_record
  in
  Alcotest.(check bool)
    "resumed outer header preserves nested PUBLIC mutation" false
    (C.is_public record);
  Alcotest.(check bool)
    "Ret1 uses current native count rather than outer source signature" false
    (C.Stored_flag.is_set ~mask:(C.stored_flag_mask record) C.Stored_flag.Ret1)

let tests =
  [
    Alcotest.test_case
      "partial classification validates original staging and options" `Quick
      original_state;
    Alcotest.test_case "resumed header preserves native flags and current count"
      `Quick resumed_outer_flags;
  ]
