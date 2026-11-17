(* Mutations exercise caller-visible source and graph objects, never private
   comparison receipts. Expected rejection follows original source ownership,
   not acceptance by another compiler path. *)
open Holyc_lib
module Plan = Holyc_lib__Backend.X86_64_float_plan
module Unit = Holyc_lib__Driver.Integer_unit
module F = Holyc_lib__Ir.Function_body
module G = Ir_block_graph
module S = Ir_instruction_sequence
module O = Ir_opcode
module Typed = Holyc_lib__Sema.Function_call_expression_result
module Source = Holyc_lib__Sema.Function_call_resolution

let checked = function
  | Ok x -> x
  | Error _ -> Alcotest.fail "original comparison fixture rejected"

let fixture ?(return_type = "I64") ?(body = "if((a+b)>a)return 1;return 0;") ()
    =
  let contents = return_type ^ " F(F64 a,F64 b){" ^ body ^ "}" in
  let fixture =
    Native_scalar_fixture.compile ~mode:Preprocessor.Aot
      ~path:"comparison-authority.hc" ~contents ()
    |> checked
  in
  let definition = List.hd (Unit.functions fixture.unit_) in
  let source : Plan.source =
    {
      runtime = Unit.runtime_calls fixture.unit_;
      body = definition.body;
      frame = definition.frame;
    }
  in
  (source, F.body definition.body)

let comparison d =
  List.mem d.S.opcode
    [
      O.Ic_less; Ic_greater_equ; Ic_greater; Ic_less_equ; Ic_equ_equ; Ic_not_equ;
    ]

let rejects label = function
  | Error (_ :: _) -> ()
  | _ -> Alcotest.fail (label ^ " admitted an unproved comparison")

let mutate graph select transform =
  let changed = ref false in
  let rec visit = function
    | [] -> ()
    | instruction :: rest as cell ->
        let d = S.description instruction in
        if (not !changed) && select d then (
          Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform d));
          changed := true)
        else visit rest
  in
  List.iter
    (fun block -> visit (S.instructions (G.instructions block)))
    (G.blocks graph);
  Alcotest.(check bool) "mutation found original instruction" true !changed

let invalidated label source graph plan =
  Alcotest.(check bool)
    (label ^ " invalidates retained receipt")
    true
    (Plan.validated_comparisons plan ~graph = None);
  rejects (label ^ " rejects a fresh receipt") (Plan.prepare ~source graph)

let owners () =
  let source, graph = fixture () in
  let foreign, foreign_graph = fixture () in
  let plan = Plan.prepare ~source graph |> checked in
  Alcotest.(check int)
    "one original comparison" 1
    (Plan.validated_comparisons plan ~graph |> Option.get |> List.length);
  rejects "missing original source" (Plan.prepare graph);
  List.iter
    (fun (label, source) -> rejects label (Plan.prepare ~source graph))
    [
      ("foreign runtime", { source with runtime = foreign.runtime });
      ("foreign frame", { source with frame = foreign.frame });
      ("foreign body", { source with body = foreign.body });
      ( "copied frame",
        { source with frame = Obj.obj (Obj.dup (Obj.repr source.frame)) } );
    ];
  Alcotest.(check bool)
    "foreign equal-text graph" true
    (Plan.validated_comparisons plan ~graph:foreign_graph = None);
  let copy =
    G.create
      ~entry:(G.block_id (G.entry graph))
      (List.map
         (fun block ->
           ({
              G.block_id = G.block_id block;
              instructions =
                S.instructions (G.instructions block) |> List.map S.description;
            }
             : G.block_description))
         (G.blocks graph))
    |> checked
  in
  rejects "copied graph" (Plan.prepare ~source copy);
  Alcotest.(check bool)
    "copied graph cannot reuse receipt" true
    (Plan.validated_comparisons plan ~graph:copy = None)

let constant_receipts () =
  List.iter
    (fun (expression, expected) ->
      let source, graph = fixture ~body:("return " ^ expression ^ ";") () in
      let plan = Plan.prepare ~source graph |> checked in
      let receipt =
        Plan.validated_comparisons plan ~graph |> Option.get |> List.hd
      in
      Alcotest.(check (option int64))
        (expression ^ " has its source-folded word")
        (Some expected) receipt.comparison_constant;
      Alcotest.(check int)
        "constant comparison needs no x87 stack" 0 receipt.comparison_peak_depth;
      rejects "constant comparison requires source" (Plan.prepare graph);
      let foreign, _ = fixture ~body:("return " ^ expression ^ ";") () in
      rejects "equal-text constant source is foreign"
        (Plan.prepare ~source:foreign graph))
    [
      ("1.0<2.0", 1L);
      ("1.0>=2.0", 0L);
      ("2.0>1.0", 1L);
      ("2.0<=1.0", 0L);
      ("1.0==1.0", 1L);
      ("1.0!=1.0", 0L);
      ("0.0<(-0.0)", 0L);
      ("0.0>=(-0.0)", 1L);
      ("0.0>(-0.0)", 0L);
      ("0.0<=(-0.0)", 1L);
      ("0.0==(-0.0)", 0L);
      ("0.0!=(-0.0)", 1L);
      ("9007199254740993==9007199254740992.0", 1L);
      ("9007199254740993>9007199254740992.0", 0L);
    ];
  List.iter
    (fun (label, select, transform) ->
      let source, graph = fixture ~body:"return 1.0<2.0;" () in
      let plan = Plan.prepare ~source graph |> checked in
      mutate graph select transform;
      invalidated label source graph plan)
    [
      ( "folded comparison opcode",
        comparison,
        fun d -> { d with S.opcode = O.Ic_greater } );
      ( "folded comparison source bits",
        (fun d -> d.S.opcode = O.Ic_imm_f64),
        fun d -> { d with S.payload = Some (S.Float_bits 0L) } );
      ( "folded comparison flags",
        comparison,
        fun d -> { d with S.flags = Int64.logor d.flags 0x1000000L } );
    ]

let graph_changes () =
  let i64 =
    Semantic_type.make_primitive ~form:Internal_storage
      ~primitive:Primitive_type.I64 ~pointer_depth:0
    |> checked
  in
  let f64 =
    Semantic_type.make_primitive ~form:Internal_storage
      ~primitive:Primitive_type.F64 ~pointer_depth:0
    |> checked
  in
  List.iter
    (fun (label, body, select, transform) ->
      let source, graph = fixture ~body () in
      let plan = Plan.prepare ~source graph |> checked in
      mutate graph select transform;
      invalidated label source graph plan)
    [
      ( "comparison operand order",
        "return a<b;",
        comparison,
        fun d -> { d with S.operands = List.rev d.operands } );
      ( "comparison flags",
        "return a<b;",
        comparison,
        fun d -> { d with S.flags = Int64.logor d.flags 0x1000000L } );
      ( "comparison opcode",
        "return a<b;",
        comparison,
        fun d -> { d with S.opcode = O.Ic_greater } );
      ( "comparison result type",
        "return a<b;",
        comparison,
        fun d -> { d with S.target_type = Some f64 } );
      ( "producer type",
        "return (a+b)>a;",
        (fun d -> d.S.opcode = O.Ic_add),
        fun d -> { d with S.target_type = Some i64 } );
      ( "producer flags",
        "return (a+b)>a;",
        (fun d -> d.S.opcode = O.Ic_add),
        fun d -> { d with S.flags = 2L } );
      ( "literal bits",
        "return a<1.0;",
        (fun d -> d.S.opcode = O.Ic_imm_f64),
        fun d -> { d with S.payload = Some (S.Float_bits 0x4000000000000000L) }
      );
      ( "branch polarity",
        "if(a<b)return 1;return 0;",
        (fun d -> d.S.opcode = O.Ic_br_zero),
        fun d -> { d with S.opcode = O.Ic_br_not_zero } );
      ( "branch operands",
        "if(a<b)return 1;return 0;",
        (fun d -> d.S.opcode = O.Ic_br_zero),
        fun d -> { d with S.operands = [] } );
      ( "branch target",
        "if(a<b)return 1;return 0;",
        (fun d -> d.S.opcode = O.Ic_br_zero),
        fun d -> { d with S.payload = None } );
    ]

let mutate_field target index replacement action =
  let object_ = Obj.repr target in
  let previous = Obj.field object_ index in
  Fun.protect
    ~finally:(fun () -> Obj.set_field object_ index previous)
    (fun () ->
      Obj.set_field object_ index replacement;
      action ())

let source_changes () =
  List.iter
    (fun (label, edit) ->
      let source, graph = fixture () in
      let plan = Plan.prepare ~source graph |> checked in
      let original = F.definition_source source.Plan.body |> Option.get in
      edit source original (fun () -> invalidated label source graph plan);
      ignore (Plan.prepare ~source graph |> checked))
    [
      ( "source condition role",
        fun _ original action ->
          let condition =
            Typed.function_conditions original
            |> List.hd |> Typed.condition_source
          in
          mutate_field condition 1 (Obj.repr Source.Do_while_condition) action
      );
      ( "source binary operator",
        fun _ original action ->
          let value =
            Typed.function_conditions original
            |> List.hd |> Typed.condition_value
          in
          let binary =
            match
              Source.argument_expression_kind (Typed.result_source value)
            with
            | Source.Binary_expression b -> b
            | _ -> Alcotest.fail "expected original source comparison"
          in
          mutate_field binary 0 (Obj.repr O.Ic_less) action );
      ( "typed operand class",
        fun _ original action ->
          let value =
            Typed.function_conditions original
            |> List.hd |> Typed.condition_value
          in
          let left, _ = Typed.result_binary_operands value |> Option.get in
          let i64 =
            Semantic_type.make_primitive ~form:Internal_storage
              ~primitive:Primitive_type.I64 ~pointer_depth:0
            |> checked
          in
          mutate_field left 13 (Obj.repr (Some i64)) action );
      ( "condition expression owner",
        fun _ original action ->
          let foreign, _ =
            fixture ~body:"if(!((a+b)>a))return 1;return 0;" ()
          in
          let foreign_source =
            F.definition_source foreign.Plan.body |> Option.get
          in
          let foreign_expression =
            Typed.function_conditions foreign_source
            |> List.hd |> Typed.condition_source |> Source.condition_expression
          in
          let condition =
            Typed.function_conditions original
            |> List.hd |> Typed.condition_source
          in
          mutate_field condition 3 (Obj.repr foreign_expression) action );
      ( "compiler options",
        fun source _ action ->
          mutate_field source.Plan.body 8
            (Obj.repr
               (Int64.logxor
                  (F.compiler_options source.body)
                  (Compiler_option.mask Compiler_option.No_reg_var)))
            action );
    ]

let with_entry_fixture expression action =
  let module Preparation = Holyc_lib__Driver.Native_default_preparation in
  let module Program = Holyc_lib__Ir.Default_fragment_program in
  let module Destination = Holyc_lib__Ir.Default_fragment_destination in
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"default-comparison-authority.hc"
      ~contents:("I64 F(I64 n=" ^ expression ^ "){return n;}F();")
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Preprocessor.Aot () |> checked
  in
  let visited = ref false in
  let native_default request =
    visited := true;
    let program = Preparation.native_program request in
    let destination = Program.destination program in
    let entry_source : Plan.entry_source =
      {
        lowered = Program.lowered program;
        globals = Destination.globals destination;
        value = Typed.top_level_root_value (Destination.root destination);
      }
    in
    action entry_source
      (Program.runtime_calls program)
      (Ir_x87_stack.graph (Program.entry program));
    Error
      [
        Diagnostic.make ~code:"HCTEST0001" ~severity:Diagnostic.Error
          ~message:"stop after checking original preparation authority"
          ~primary:(Destination.span destination)
          ();
      ]
  in
  ignore
    (Native_program.compile_with_preparation ~native_default
       ~preparation_steps:(ref 0) ~switch_work:(ref 0) ~dimension_work:(ref 0)
       ~default_bytes:(ref 0) session ~config ~source);
  Alcotest.(check bool) "original default callback reached" true !visited

let entry_owners () =
  let module Runtime = Holyc_lib__Ir.Runtime_call_context in
  List.iter
    (fun expression ->
      with_entry_fixture expression (fun source runtime graph ->
          let prepare ?(entry_source = source) ?(runtime = runtime) graph =
            Plan.prepare ~entry_source ~runtime ~owner:Runtime.Entry graph
          in
          let plan = prepare graph |> checked in
          let receipts = Plan.validated_comparisons plan ~graph |> Option.get in
          Alcotest.(check int)
            "one original entry comparison" 1 (List.length receipts);
          Alcotest.(check (option int64))
            "entry comparison saved word" (Some 1L)
            (List.hd receipts).comparison_constant;
          rejects "entry without lowering proof"
            (Plan.prepare ~runtime ~owner:Runtime.Entry graph);
          with_entry_fixture expression (fun foreign foreign_runtime _ ->
              List.iter
                (fun (label, entry_source) ->
                  rejects label (prepare ~entry_source graph))
                [
                  ( "foreign lowered entry",
                    { source with lowered = foreign.lowered } );
                  ( "foreign entry globals",
                    { source with globals = foreign.globals } );
                  ("foreign typed root", { source with value = foreign.value });
                ];
              rejects "foreign entry runtime"
                (prepare ~runtime:foreign_runtime graph));
          mutate graph comparison (fun d -> { d with S.opcode = O.Ic_less });
          Alcotest.(check bool)
            "entry graph mutation invalidates saved receipt" true
            (Plan.validated_comparisons plan ~graph = None);
          rejects "entry graph mutation rejects fresh proof" (prepare graph)))
    [ "1.5>1.0"; "(1.5>1.0)"; "(1.5>1.0)+41" ]

let compile_entry_calls contents =
  let fixture =
    match
      Native_scalar_fixture.compile ~mode:Preprocessor.Aot
        ~path:"call-conversion-authority.hc" ~contents ()
    with
    | Ok fixture -> fixture
    | Error diagnostics ->
        Alcotest.fail
          (String.concat "; "
             (List.map
                (fun (diagnostic : Diagnostic.t) ->
                  diagnostic.code ^ ": " ^ diagnostic.message)
                diagnostics))
  in
  ( Unit.runtime_calls fixture.unit_,
    Ir_x87_stack.graph (Unit.entry fixture.unit_) )

let call_result_conversions () =
  let module Runtime = Holyc_lib__Ir.Runtime_call_context in
  List.iter
    (fun (contents, direction) ->
      let runtime, graph = compile_entry_calls contents in
      let prepare ?(runtime = runtime) () =
        Plan.prepare ~runtime ~owner:Runtime.Entry graph
      in
      let plan = prepare () |> checked in
      let conversions = Plan.validated_conversions plan ~graph |> Option.get in
      Alcotest.(check int)
        "one exact call-result conversion" 1 (List.length conversions);
      Alcotest.(check bool)
        "original conversion direction" true
        ((List.hd conversions).conversion = direction);
      let converted =
        Plan.validated_entries plan ~graph
        |> Option.get
        |> List.filter (fun entry ->
            entry.Plan.operation = Plan.Convert_to_float)
      in
      Alcotest.(check int)
        "post-call FILD follows the proved consumer"
        (if direction = Plan.To_float then 1 else 0)
        (List.length converted);
      List.iter
        (fun entry ->
          Alcotest.(check int)
            "post-call conversion donates one extended value" 1
            (List.length entry.Plan.live_after))
        converted;
      let original =
        G.blocks graph
        |> List.concat_map (fun block ->
            List.map S.description (S.instructions (G.instructions block)))
        |> List.find (fun d ->
            d.S.opcode = O.Ic_call_end && Int64.logand d.flags 3L <> 0L)
      in
      let receipt =
        Runtime.original_call_result_conversion runtime ~owner:Runtime.Entry
          original
        |> Option.get
      in
      let source = Runtime.call_result_conversion_source receipt in
      let consumer = Runtime.call_result_conversion_consumer receipt in
      Alcotest.(check bool)
        "original call can select its replay target" true
        (Option.is_some
           (Runtime.original_expression_call_source runtime ~owner:Runtime.Entry
              source));
      let copied_source = Obj.obj (Obj.dup (Obj.repr source)) in
      Alcotest.(check bool)
        "copied typed call cannot select a replay target" true
        (Runtime.original_expression_call_source runtime ~owner:Runtime.Entry
           copied_source
        = None);
      let copied = Obj.obj (Obj.dup (Obj.repr original)) in
      Alcotest.(check bool)
        "copied call instruction has no conversion receipt" true
        (Runtime.original_call_result_conversion runtime ~owner:Runtime.Entry
           copied
        = None);
      let foreign, _ = compile_entry_calls contents in
      Alcotest.(check bool)
        "foreign source cannot select a replay target" true
        (Runtime.original_expression_call_source foreign ~owner:Runtime.Entry
           source
        = None);
      rejects "foreign call runtime" (prepare ~runtime:foreign ());
      let changed_conversion =
        match Typed.result_intrinsic_conversion source with
        | Typed.No_intrinsic_conversion -> Typed.Result_to_int
        | Result_to_f64 | Result_to_int -> Typed.No_intrinsic_conversion
      in
      mutate_field source 20 (Obj.repr changed_conversion) (fun () ->
          Alcotest.(check bool)
            "changed typed conversion invalidates plan" true
            (Plan.validated_conversions plan ~graph = None);
          rejects "changed typed conversion rejects fresh plan" (prepare ()));
      (match Typed.result_binary_operands consumer with
      | Some (left, right) ->
          let copied_left = Obj.obj (Obj.dup (Obj.repr left)) in
          mutate_field consumer 4
            (Obj.repr (Some (copied_left, right)))
            (fun () ->
              Alcotest.(check bool)
                "copied consumer child invalidates plan" true
                (Plan.validated_conversions plan ~graph = None);
              rejects "copied consumer child rejects fresh plan" (prepare ()))
      | None -> ());
      ignore (prepare () |> checked);
      mutate graph
        (fun d -> d.S.instruction_id = original.instruction_id)
        (fun d -> { d with S.flags = Int64.logxor d.flags 3L });
      Alcotest.(check bool)
        "changed call flags invalidate plan" true
        (Plan.validated_conversions plan ~graph = None);
      rejects "changed call flags reject fresh plan" (prepare ()))
    [
      ("I64 Word(){return 9007199254740993;}Word()+1.0;", Plan.To_float);
      ("I64 Word(){return 9007199254740993;}1.0+Word();", Plan.To_float);
      ("I64 Word(){return 9007199254740993;}(+(Word()))+1.0;", Plan.To_float);
      ( "I64 Word(){return 9007199254740993;}I64 (*p)()=&Word;p()+1.0;",
        Plan.To_float );
      ( "F64 Fraction(){return 3.75;}I64 Id(I64 n){return n;}Id(Fraction());",
        Plan.To_integer );
      ( "F64 Fraction(){return 3.75;}I64 Id(I64 n){return n;}Id((Fraction()));",
        Plan.To_integer );
      ( "F64 Fraction(){return 3.75;}F64 (*p)()=&Fraction;I64 Id(I64 n){return \
         n;}Id(p());",
        Plan.To_integer );
    ]

let converted_call graph =
  G.blocks graph
  |> List.concat_map (fun block ->
      List.map S.description (S.instructions (G.instructions block)))
  |> List.find (fun d ->
      d.S.opcode = O.Ic_call_end && Int64.logand d.flags 3L <> 0L)

let initializer_call_conversions () =
  let module Runtime = Holyc_lib__Ir.Runtime_call_context in
  List.iter
    (fun (contents, direction) ->
      let runtime, graph = compile_entry_calls contents in
      let prepare ?(runtime = runtime) () =
        Plan.prepare ~runtime ~owner:Runtime.Entry graph
      in
      let original = converted_call graph in
      Alcotest.(check bool)
        "declaration conversion retains its initializer authority" true
        (Option.is_some
           (Runtime.original_initializer_region runtime ~owner:Runtime.Entry
              original));
      Alcotest.(check bool)
        "declaration conversion requires complete initializer replay" true
        (Runtime.original_call_result_conversion runtime ~owner:Runtime.Entry
           original
        = None);
      let plan = prepare () |> checked in
      let conversions = Plan.validated_conversions plan ~graph |> Option.get in
      Alcotest.(check int)
        "one original declaration call conversion" 1 (List.length conversions);
      let conversion = List.hd conversions in
      Alcotest.(check bool)
        "initializer conversion preserves its original producer and direction"
        true
        (conversion.conversion_instruction = original.instruction_id
        && conversion.conversion = direction
        && (not conversion.deferred) && not conversion.elided);
      let copied = Obj.obj (Obj.dup (Obj.repr original)) in
      Alcotest.(check bool)
        "copied call cannot select an initializer region" true
        (Runtime.original_initializer_region runtime ~owner:Runtime.Entry copied
        = None);
      let foreign, _ = compile_entry_calls contents in
      rejects "foreign initializer call runtime" (prepare ~runtime:foreign ());
      mutate graph
        (fun d -> d.S.instruction_id = original.instruction_id)
        (fun d -> { d with S.flags = Int64.logxor d.flags 3L });
      Alcotest.(check bool)
        "changed initializer call flags invalidate retained conversion" true
        (Plan.validated_conversions plan ~graph = None);
      rejects "changed initializer call flags reject fresh conversion"
        (prepare ()))
    [
      ("I64 Seed(){return 42;}F64 value=Seed();value;", Plan.To_float);
      ("F64 Seed(){return 42.75;}I64 value=Seed();value;", Plan.To_integer);
      ( "I64 Seed(){return 42;}F64 Read(){static F64 value=Seed();return \
         value;}Read();",
        Plan.To_float );
      ( "F64 Seed(){return 42.75;}I64 Read(){static I64 value=Seed();return \
         value;}Read();",
        Plan.To_integer );
    ]

let initializer_nested_call_invalidation () =
  let module Runtime = Holyc_lib__Ir.Runtime_call_context in
  List.iter
    (fun contents ->
      let runtime, graph = compile_entry_calls contents in
      let prepare () = Plan.prepare ~runtime ~owner:Runtime.Entry graph in
      let plan = prepare () |> checked in
      let original = converted_call graph in
      let receipt =
        Runtime.original_call_result_conversion runtime ~owner:Runtime.Entry
          original
        |> Option.get
      in
      let source = Runtime.call_result_conversion_source receipt
      and consumer = Runtime.call_result_conversion_consumer receipt in
      Alcotest.(check bool)
        "ordinary converted call remains inside its initializer region" true
        (Option.is_some
           (Runtime.original_initializer_region runtime ~owner:Runtime.Entry
              original));
      let invalidated label =
        Alcotest.(check bool)
          (label ^ " revokes the original call receipt")
          true
          (Runtime.original_call_result_conversion runtime ~owner:Runtime.Entry
             original
          = None);
        Alcotest.(check bool)
          (label ^ " revokes initializer replay authority")
          true
          (Runtime.original_initializer_region runtime ~owner:Runtime.Entry
             original
          = None);
        Alcotest.(check bool)
          (label ^ " invalidates retained conversion")
          true
          (Plan.validated_conversions plan ~graph = None);
        rejects (label ^ " rejects a fresh initializer conversion") (prepare ())
      in
      let changed_conversion =
        match Typed.result_intrinsic_conversion source with
        | Typed.No_intrinsic_conversion -> Typed.Result_to_int
        | Result_to_f64 | Result_to_int -> Typed.No_intrinsic_conversion
      in
      mutate_field source 20 (Obj.repr changed_conversion) (fun () ->
          invalidated "changed nested call conversion");
      let left, right = Typed.result_binary_operands consumer |> Option.get in
      let copied_left = Obj.obj (Obj.dup (Obj.repr left)) in
      mutate_field consumer 4
        (Obj.repr (Some (copied_left, right)))
        (fun () -> invalidated "copied initializer consumer child");
      ignore (prepare () |> checked))
    [
      "I64 Seed(){return 42;}F64 value=Seed()+1.0;value;";
      "I64 Seed(){return 42;}F64 Read(){static F64 value=Seed()+1.0;return \
       value;}Read();";
    ]

let returned_comparison_conversions () =
  List.iter
    (fun body ->
      let source, graph = fixture ~return_type:"F64" ~body () in
      let prepare () = Plan.prepare ~source graph in
      let plan = prepare () |> checked in
      let comparisons = Plan.validated_comparisons plan ~graph |> Option.get
      and conversions = Plan.validated_conversions plan ~graph |> Option.get in
      Alcotest.(check int) "one returned comparison" 1 (List.length comparisons);
      Alcotest.(check int)
        "one comparison result conversion" 1 (List.length conversions);
      let comparison_plan = List.hd comparisons
      and conversion = List.hd conversions in
      Alcotest.(check bool)
        "conversion belongs to the original comparison result" true
        (conversion.conversion_instruction
         = comparison_plan.comparison_instruction
        && conversion.conversion_result = comparison_plan.comparison_result
        && conversion.conversion = Plan.To_float
        && (not conversion.deferred) && not conversion.elided);
      let original = F.definition_source source.Plan.body |> Option.get in
      let returned = Typed.function_returns original |> List.hd in
      let value = Typed.return_value returned |> Option.get in
      Alcotest.(check bool)
        "original return requests the F64 conversion" true
        (Typed.return_conversion returned = Typed.Result_to_f64
        && Typed.result_intrinsic_conversion value = Typed.Result_to_f64);
      mutate_field value 20 (Obj.repr Typed.No_intrinsic_conversion) (fun () ->
          Alcotest.(check bool)
            "changed returned conversion invalidates retained conversion" true
            (Plan.validated_conversions plan ~graph = None);
          invalidated "changed returned conversion" source graph plan);
      ignore (prepare () |> checked);
      mutate graph comparison (fun d ->
          { d with S.flags = Int64.logxor d.flags 1L });
      Alcotest.(check bool)
        "changed comparison flags invalidate retained conversion" true
        (Plan.validated_conversions plan ~graph = None);
      invalidated "changed comparison result conversion flags" source graph plan)
    [ "return a<b;"; "return (a+b)>a;" ]

let successor_changes () =
  let source, graph = fixture () in
  let plan = Plan.prepare ~source graph |> checked in
  let block =
    G.blocks graph |> List.find (fun b -> List.length (G.successors b) > 1)
  in
  mutate_field block 2
    (Obj.repr (List.rev (G.successors block)))
    (fun () -> invalidated "successor order" source graph plan)

let () =
  Alcotest.run "native F64 comparison authority"
    [
      ( "source receipts",
        [
          Alcotest.test_case "original ownership" `Quick owners;
          Alcotest.test_case "constant folding preserves source authority"
            `Quick constant_receipts;
          Alcotest.test_case "original declaration expression ownership" `Quick
            entry_owners;
          Alcotest.test_case "materialized source call conversions" `Quick
            call_result_conversions;
          Alcotest.test_case "AOT initializer call conversions" `Quick
            initializer_call_conversions;
          Alcotest.test_case "nested initializer call invalidation" `Quick
            initializer_nested_call_invalidation;
          Alcotest.test_case "returned comparison conversions" `Quick
            returned_comparison_conversions;
          Alcotest.test_case "instruction and branch snapshots" `Quick
            graph_changes;
          Alcotest.test_case "source context and options" `Quick source_changes;
          Alcotest.test_case "control-flow edge order" `Quick successor_changes;
        ] );
    ]
