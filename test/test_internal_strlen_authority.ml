open Holyc_lib
module Unit = Holyc_lib__Driver.Integer_unit
module Program = X86_64_program
module VM = Ir_integer_interpreter
module Calls = Ir_runtime_call_context
module Graph = Ir_block_graph
module Seq = Ir_instruction_sequence

let source = "_intern 0x84 I64 Count(U8 *s);Count(\"abc\");"
let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let diagnostics errors =
  errors
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let fixture mode =
  match
    Native_scalar_fixture.compile ~mode ~path:"internal-strlen-authority.hc"
      ~contents:source ()
  with
  | Ok fixture -> fixture
  | Error errors -> Alcotest.fail (diagnostics errors)

let compile ?runtime_calls fixture =
  let unit_ = fixture.Native_scalar_fixture.unit_ in
  Program.compile_callable ~max_stack_bytes:4088 ~max_blocks:4096
    ~max_ir_instructions:4096 ~max_code_bytes:65536
    ~runtime_calls:
      (Option.value runtime_calls ~default:(Unit.runtime_calls unit_))
    ~initialization:(Unit.initialization unit_)
    ~entry:(Unit.entry unit_) ~functions:(Unit.functions unit_) ()

let execute ?runtime_calls fixture =
  let unit_ = fixture.Native_scalar_fixture.unit_ in
  VM.execute_program ~max_steps:1000 ~max_frame_bytes:1024 ~max_call_depth:16
    ~max_global_bytes:1024
    ~runtime_calls:
      (Option.value runtime_calls ~default:(Unit.runtime_calls unit_))
    ~globals:(Unit.globals unit_)
    ~initialization:(Unit.initialization unit_)
    ~functions:(Unit.functions unit_) (Unit.entry unit_)

let rejects label = function
  | Error (_ :: _) -> ()
  | Error [] -> Alcotest.fail (label ^ " returned no diagnostic")
  | Ok _ -> Alcotest.fail (label ^ " accepted changed internal-call authority")

let valid_control fixture =
  (match compile fixture with
  | Ok _ -> ()
  | Error errors ->
      errors
      |> List.map (fun (error : Program.error) ->
          error.code ^ ": " ^ error.message)
      |> String.concat "; " |> Alcotest.fail);
  match execute fixture with
  | Ok execution -> (
      match VM.final_value execution with
      | Some value ->
          Alcotest.(check int64) "valid original result" 3L value.bits
      | None -> Alcotest.fail "valid original has no result")
  | Error errors ->
      errors
      |> List.map (fun (error : VM.error) -> error.code ^ ": " ^ error.message)
      |> String.concat "; " |> Alcotest.fail

let find_cell fixture opcode =
  let rec find = function
    | [] -> None
    | instruction :: rest as cell ->
        let description = Seq.description instruction in
        if description.opcode = opcode then Some (cell, description)
        else find rest
  in
  fixture.Native_scalar_fixture.unit_ |> Unit.entry |> Ir_x87_stack.graph
  |> Graph.blocks
  |> List.find_map (fun block ->
      find (Graph.instructions block |> Seq.instructions))
  |> function
  | Some found -> found
  | None ->
      Alcotest.fail "valid source omitted its expected internal-call phase"

let exact_context_ownership () =
  List.iter
    (fun mode ->
      let original = fixture mode in
      let same_text = fixture mode in
      valid_control original;
      valid_control same_text;
      let foreign = Unit.runtime_calls same_text.unit_ in
      rejects "native foreign same-text context"
        (compile ~runtime_calls:foreign original);
      rejects "interpreter foreign same-text context"
        (execute ~runtime_calls:foreign original);
      let _, start = find_cell original Ir_opcode.Ic_call_start in
      let context = Unit.runtime_calls original.unit_ in
      let internal =
        Calls.find_intrinsic_start context ~owner:Calls.Entry
          start.instruction_id
        |> Option.get
      in
      Alcotest.(check bool)
        "ordinary call lookup cannot borrow internal authority" true
        (Option.is_none
           (Calls.find_start context ~owner:Calls.Entry start.instruction_id));
      Alcotest.(check bool)
        "operation lookup retains the exact selected internal record" true
        (Option.fold ~none:false
           ~some:(fun actual -> actual == internal)
           (Calls.find_intrinsic_instruction context ~owner:Calls.Entry
              (Calls.intrinsic_instruction internal)));
      Alcotest.(check bool)
        "call end retains that same record" true
        (Option.fold ~none:false
           ~some:(fun actual -> actual == internal)
           (Calls.find_intrinsic_end context ~owner:Calls.Entry
              (Calls.intrinsic_last internal))))
    modes

let malformed_original_graphs () =
  let mutations =
    [
      ( "internal flags",
        Ir_opcode.Ic_strlen,
        fun (d : Seq.description) -> { d with flags = 1L } );
      ( "internal operand",
        Ir_opcode.Ic_strlen,
        fun d -> { d with operands = [] } );
      ( "internal payload",
        Ir_opcode.Ic_strlen,
        fun d -> { d with payload = Some (Seq.Integer 0x84L) } );
      ( "internal return class",
        Ir_opcode.Ic_strlen,
        fun d -> { d with target_type = None } );
      ( "different internal opcode",
        Ir_opcode.Ic_strlen,
        fun d -> { d with opcode = Ir_opcode.Ic_rdtsc } );
      ( "argument push",
        Ir_opcode.Ic_str_const,
        fun d -> { d with flags = 0x2000L } );
      ( "call-start payload",
        Ir_opcode.Ic_call_start,
        fun d -> { d with payload = None } );
      ( "call-end result",
        Ir_opcode.Ic_call_end,
        fun d -> { d with result = None } );
      ( "call-end return type",
        Ir_opcode.Ic_call_end,
        fun d -> { d with target_type = None } );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, opcode, transform) ->
          let original = fixture mode in
          valid_control original;
          let cell, description = find_cell original opcode in
          (* Keep the sealed graph identity and alter the reached instruction,
             as the other native authority tests do. No forged constructor is
             exported to production code. *)
          Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform description));
          rejects ("native " ^ label) (compile original);
          rejects ("interpreter " ^ label) (execute original))
        mutations)
    modes

let tests =
  [
    Alcotest.test_case "internal calls retain exact context ownership" `Quick
      exact_context_ownership;
    Alcotest.test_case "changed internal-call phases cannot execute" `Quick
      malformed_original_graphs;
  ]
