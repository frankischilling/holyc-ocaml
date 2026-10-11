open Holyc_lib
module Cases = Exception_execution_cases
module Output = Test_integer_output

(* CMain.HC:589-598 makes ExePutS consume only Compiler and Break.
   CMain.HC:673-688 routes an AOT #exe StreamExePrint through that handler.
   Other source exceptions continue into the active caller's try region. *)
let child_call contents = Printf.sprintf "StreamExePrint(\"%%s\",%S)" contents

let directive body =
  (* The saved compiler and the #exe task have different declaration tables.
     Execute the saved compiler's headers through its own source bridge so
     its declarations have runtime admission before the throwing child. *)
  "#exe {\n" ^ Cases.headers ^ "\n" ^ child_call Cases.headers ^ ";\n" ^ body
  ^ "\n}\n"

let run ?max_frame_bytes ?max_global_bytes body =
  Output.run ~mode:Preprocessor.Aot ~max_steps:100_000 ?max_frame_bytes
    ?max_global_bytes (directive body)

let no_compiler_receipts label report =
  Alcotest.(check int)
    (label ^ " does not fabricate a parser Compiler receipt")
    0
    (List.length (integer_program_report_compiler_exceptions report))

let source_throw_preserves_effects () =
  let child =
    {|I64 Saved=10;U0 Leaf(){Saved+=30;Print("child;");throw('Leaf');Saved=999;Print("skipped;");}Leaf();|}
  in
  let body =
    Printf.sprintf
      {|I64 N=0;Print("before;");
        try {%s;N=999;Print("skipped;");}
        catch {if(Fs->except_ch=='Leaf'){
          N=%s;Print("caught;");Fs->catch_except=TRUE;
        }}
        Print("after;");StreamPrint("%%d;",N);|}
      (child_call child) (child_call "Saved+2;")
  in
  let report = run body in
  ignore (Output.expect "before;child;caught;after;" report);
  no_compiler_receipts "propagated Leaf" report

let division_fault_enters_outer_catch () =
  let child =
    {|I64 Divide(I64 n){Print("child;");return 21/n;}Divide(0);Print("skipped;");|}
  in
  let body =
    Printf.sprintf
      {|I64 N=0;
        try {%s;N=999;}
        catch {if(Fs->except_ch=='DivZero'){
          N=42;Print("caught;");Fs->catch_except=TRUE;
        }}
        StreamPrint("%%d;",N);|}
      (child_call child)
  in
  let report = run body in
  ignore (Output.expect "child;caught;" report);
  no_compiler_receipts "propagated DivZero" report

let exe_put_s_consumes_compiler_and_break () =
  List.iter
    (fun code ->
      let child =
        Printf.sprintf
          {|I64 Saved=40;Print("child;");throw('%s');Saved=999;Print("skipped;");|}
          code
      in
      let body =
        Printf.sprintf
          {|I64 N=-1,Outer=0;
            try {N=%s;}
            catch {Outer++;Print("outer;");Fs->catch_except=TRUE;}
            Print("%%d;%%d;",N,Outer);
            StreamPrint("%%d;",%s);|}
          (child_call child) (child_call "Saved+2;")
      in
      let report = run body in
      ignore (Output.expect "child;0;0;" report);
      no_compiler_receipts ("runtime " ^ code) report)
    [ "Compiler"; "Break" ]

let original_compiler_receipt label report =
  match integer_program_report_compiler_exceptions report with
  | [ receipt ] ->
      Alcotest.(check string)
        (label ^ " retains the actual parser exception")
        "HCPARSE0168" (Parser.compiler_exception_diagnostic receipt).code;
      Alcotest.(check int64)
        (label ^ " retains one LexExcept increment")
        1L
        (Parser.compiler_exception_error_count receipt)
  | receipts ->
      Alcotest.failf "%s expected one parser Compiler receipt, got %d" label
        (List.length receipts)

let parser_compiler_catch_updates_task_state () =
  let body =
    Printf.sprintf
      {|I64 N=0,Child=-1;
        try throw('Outer');
        catch {if(Fs->except_ch=='Outer'){
          Child=%s;
          if(Child==0 && Fs->except_ch=='Compiler' && Fs->catch_except)N=42;
        }}
        StreamPrint("%%d;",N);|}
      (child_call {|Print("child;");return 42;Print("skipped;");|})
  in
  let report = run body in
  (* The ExePutS Compiler catch sets the shared catch byte. The outer catch
     deliberately leaves it alone, just as KExcept.HC's dispatcher does. *)
  ignore (Output.expect "child;" report);
  original_compiler_receipt "Compiler state inside outer catch" report

let parser_compiler_catch_allocates_bounded_task_storage () =
  let body =
    child_call "return 42;" ^ ";Print(\"after;\");StreamPrint(\"42;\");"
  in
  let baseline = run body in
  ignore (Output.expect "after;" baseline);
  original_compiler_receipt "lazy Compiler task storage" baseline;
  let bytes =
    match integer_program_report_progress baseline with
    | None -> Alcotest.fail "Compiler catch needs a runtime progress report"
    | Some progress -> progress.runtime.global_bytes
  in
  Alcotest.(check bool)
    "Compiler catch allocates owned task fields without an explicit Fs call"
    true (bytes >= 889);
  ignore (run ~max_global_bytes:bytes body |> Output.expect "after;");
  let rejected = run ~max_global_bytes:(bytes - 1) body in
  let error = Output.fault "HCRUN0004" rejected in
  let needle = "HCIRVM0016" in
  let rec contains_code offset =
    offset + String.length needle <= String.length error.message
    && (String.sub error.message offset (String.length needle) = needle
       || contains_code (offset + 1))
  in
  Alcotest.(check bool)
    "child catch rejection identifies the cumulative global byte limit" true
    (contains_code 0);
  original_compiler_receipt "Compiler receipt survives task storage exhaustion"
    rejected

let retained_child =
  {|I64 Saved=0;U0 Child(){I64 Held[64];Held[0]=42;Saved=Held[0];Print("child;");throw('Leaf');}|}

let frame_inspection =
  {|I64 Inspect(){I64 Scratch[64];Scratch[0]=Saved;return Scratch[0];}|}

let catch_bridge_retains_child_frame_charge () =
  let body =
    Printf.sprintf
      {|I64 N=0;
        try {%s;N=999;}
        catch {if(Fs->except_ch=='Leaf'){
          Print("caught;");N=%s;Fs->catch_except=TRUE;
        }}
        StreamPrint("%%d;",N);|}
      (child_call (retained_child ^ "Child();"))
      (child_call (frame_inspection ^ "Inspect();"))
  in
  ignore (run body |> Output.expect "child;caught;");
  (* Each 512-byte automatic array fits with its own provider and handler
     overhead. The caught child's frame remains live during inspection, so
     the two frames cannot share an 800-byte task allowance. *)
  let report = run ~max_frame_bytes:800 body in
  ignore (Output.fault ~output:"child;caught;" "HCIRVM0011" report);
  no_compiler_receipts "retained child frame quota" report

let accepted_handler_releases_child_frame_charge () =
  let body =
    Printf.sprintf
      {|try {%s;}
        catch {if(Fs->except_ch=='Leaf'){
          Print("caught;");Fs->catch_except=TRUE;
        }}
        I64 N=%s;Print("%%d;",N);StreamPrint("%%d;",N);|}
      (child_call (retained_child ^ "Child();"))
      (child_call (frame_inspection ^ "Inspect();"))
  in
  (* Moving the same inspection past accepted unwind makes the two arrays
     use the allowance in turn. Saved still exposes the child's prior write. *)
  let report = run ~max_frame_bytes:800 body in
  ignore (Output.expect "child;caught;42;" report);
  no_compiler_receipts "accepted child frame release" report

let repeated_handlers_reuse_child_frame_allowance () =
  let body =
    Printf.sprintf
      {|%s;
        I64 I,N=0;
        for(I=0;I<3;I++){
          try {%s;N=999;}
          catch {if(Fs->except_ch=='Leaf'){
            Print("caught;");Fs->catch_except=TRUE;
          }}
          N=%s;Print("%%d;",N);
        }
        StreamPrint("%%d;",N);|}
      (child_call (retained_child ^ frame_inspection))
      (child_call "Child();") (child_call "Inspect();")
  in
  let report = run ~max_frame_bytes:800 body in
  ignore
    (Output.expect "child;caught;42;child;caught;42;child;caught;42;" report);
  no_compiler_receipts "repeated accepted child frames" report

let child_provider_fault_does_not_become_a_throw () =
  let child = {|Print("kept;");Print("%d");Print("skipped;");|} in
  let body =
    Printf.sprintf
      {|try {%s;}
        catch {Print("caught;");Fs->catch_except=TRUE;}
        Print("after;");StreamPrint("42;");|}
      (child_call child)
  in
  let report = run body in
  ignore (Output.fault ~output:"kept;" "HCIRVM0025" report);
  no_compiler_receipts "child provider fault" report;
  match integer_program_report_progress report with
  | None -> Alcotest.fail "reached child provider needs a progress report"
  | Some progress ->
      Alcotest.(check int)
        "provider fault leaves later generated source unpublished" 0
        progress.runtime.generated_bytes

let public_callback_diagnostics_are_not_exceptions () =
  let module VM = Ir_integer_interpreter in
  let module Fixture = Test_task_declarations in
  (* Keep this low-level callback fixture to original declarations and plain
     statements. The shared source cases separately execute the offset checks
     through the source pipeline that prepares their layout expressions. *)
  let contents =
    {|
#define FALSE 0
#define TRUE 1
class CTask {
  U8 original_prefix[816];
  I64 except_ch;
  U8 *except_callers[8];
  Bool catch_except;
};
extern U0 SysTry(U8 *catch_start,U8 *untry_start);
extern U0 SysUntry();
public _intern 21 CTask *Fs();
extern U0 Print(U8 *fmt,...);
extern I64 StreamExePrint(U8 *fmt,...);
I64 Probe() {
  I64 N=0;
  Print("before;");
  try {
    N=StreamExePrint("42;");
    Print("after;");
  } catch {
    Print("caught;");
    Fs->catch_except=TRUE;
    N=999;
  }
  return N;
}
Probe();
|}
  in
  List.iter
    (fun forged ->
      let label =
        Option.fold ~none:"successful public callback" ~some:fst forged
      in
      let session, runtime, ledger = Fixture.runtime_setup () in
      let parsed, _ =
        Fixture.parse
          ~reference:(Task_declarations.observe_reference ledger)
          session ledger contents
      in
      let ast = Test_parser.expect_ast parsed in
      let program =
        Test_task_command_order.compile session runtime ledger ast
      in
      let calls = ref [] in
      let stream_exe_print source =
        calls := source :: !calls;
        match forged with
        | None -> Ok 42L
        | Some (code, message) ->
            Error
              [
                Diagnostic.make ~code ~severity:Diagnostic.Error ~message
                  ~primary:ast.span ();
              ]
      in
      (* The public callback can supply any diagnostic, including a real
         source span and an exception-shaped code. Only the private runtime
         transfer carries authority to invoke a source catch. *)
      let result =
        VM.execute_task_program ~stream_exe_print runtime
          ~runtime_calls:(integer_program_runtime_calls program)
          ~globals:(integer_program_globals program)
          ~initialization:(integer_program_initialization program)
          ~functions:(integer_program_functions program)
          (integer_program_entry program)
      in
      (match result with
      | Error errors when !calls = [] ->
          Alcotest.fail
            (label ^ " failed before callback entry: "
            ^ Test_ir_integer_interpreter.show_vm_errors errors)
      | _ -> ());
      Alcotest.(check (list string))
        (label ^ " reaches the callback exactly once")
        [ "42;" ] (List.rev !calls);
      Alcotest.(check string)
        (label ^ " keeps only permitted output")
        (if Option.is_none forged then "before;after;" else "before;")
        (VM.task_output_bytes runtime);
      match (forged, result) with
      | None, Ok execution ->
          Alcotest.(check bool)
            "successful callback resumes through stream end" true
            (VM.termination execution = VM.Stream_end);
          Alcotest.(check (option int64))
            "successful callback returns its own value" (Some 42L)
            (Option.map
               (fun (word : VM.word) -> word.bits)
               (VM.final_value execution))
      | Some (code, message), Error (error :: _) ->
          Alcotest.(check bool)
            (label ^ " remains an execution failure")
            true
            (error.VM.stage = VM.Execution);
          Alcotest.(check string)
            (label ^ " preserves the callback diagnostic code")
            code error.code;
          Alcotest.(check bool)
            (label ^ " preserves the callback diagnostic message")
            true
            (String.ends_with ~suffix:message error.message)
      | Some _, Ok _ ->
          Alcotest.fail (label ^ " was swallowed by a source catch")
      | _, Error errors ->
          Alcotest.fail
            (label ^ ": " ^ Test_ir_integer_interpreter.show_vm_errors errors))
    [
      None;
      Some ("HCEX0001", "unhandled HolyC exception 0x6661654c");
      Some ("HCIRVM0009", "division by zero");
      Some ("HCIRVM0030", "undefined extern Missing");
    ]

let nested_source_throw_reaches_outermost_catch () =
  let leaf =
    {|Saved+=30;Print("level2;");throw('Leaf');Saved=999;Print("skipped-leaf;");|}
  in
  let child =
    (* The nested directive runs during lookahead, before the preceding
       ordinary Print command resumes. Put reached output in the directive. *)
    Printf.sprintf
      {|I64 Saved=10;Print("skipped-before-directive;");
        #exe {Print("level1;");%s;Print("skipped-inner;");}
        Saved=999;Print("skipped-outer;");|}
      (child_call leaf)
  in
  let body =
    Printf.sprintf
      {|I64 N=0;Print("before;");
        try {%s;N=999;Print("skipped-top;");}
        catch {if(Fs->except_ch=='Leaf'){
          N=%s;Print("caught;");Fs->catch_except=TRUE;
        }}
        Print("after;");StreamPrint("%%d;",N);|}
      (child_call child) (child_call "Saved+2;")
  in
  let report = run body in
  ignore (Output.expect "before;level1;level2;caught;after;" report);
  no_compiler_receipts "Leaf across two child source levels" report

let tests =
  [
    Alcotest.test_case "child throw reaches outer catch and preserves effects"
      `Quick source_throw_preserves_effects;
    Alcotest.test_case "child DivZero reaches outer catch" `Quick
      division_fault_enters_outer_catch;
    Alcotest.test_case "ExePutS consumes runtime Compiler and Break" `Quick
      exe_put_s_consumes_compiler_and_break;
    Alcotest.test_case "parser Compiler catch updates the shared task fields"
      `Quick parser_compiler_catch_updates_task_state;
    Alcotest.test_case "parser Compiler catch charges lazy task storage" `Quick
      parser_compiler_catch_allocates_bounded_task_storage;
    Alcotest.test_case "catch bridge charges the suspended child frame" `Quick
      catch_bridge_retains_child_frame_charge;
    Alcotest.test_case "accepted outer catch releases the child frame charge"
      `Quick accepted_handler_releases_child_frame_charge;
    Alcotest.test_case "repeated outer catches reuse the child frame allowance"
      `Quick repeated_handlers_reuse_child_frame_allowance;
    Alcotest.test_case
      "child provider failure has no source exception authority" `Quick
      child_provider_fault_does_not_become_a_throw;
    Alcotest.test_case "public callback diagnostics cannot enter a source catch"
      `Quick public_callback_diagnostics_are_not_exceptions;
    Alcotest.test_case "two child source levels preserve the original throw"
      `Quick nested_source_throw_reaches_outermost_catch;
  ]
