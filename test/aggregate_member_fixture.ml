open Holyc_lib
module VM = Ir_integer_interpreter
module Body = Ir_function_body
module Graph = Ir_block_graph
module Seq = Ir_instruction_sequence

let contents =
  {|class Box{U8 tag;U16 value;};I64 F(){Box o;o.value=42;return o.value;}F();|}

let local_contents =
  {|I64 F(){class Box{U8 tag;U16 value;};Box o;o.value=42;return o.value;}F();|}

let local_pointer_contents =
  {|I64 F(){class Box{U8 tag;U16 value;};Box o[2];o[1].value=42;Box *p=o;p++;return p->value;}
I64 G(){class Box{U8 tag;U16 value;};Box o[2];o[1].value=42;Box *p=o;p++;return p->value;}F();|}

let local_backing_contents =
  {|I64 F(){class Box{U8 tag;U8 bytes[8];};Box o;o=42;o.bytes[0]=0;return o;}
I64 G(){class Box{U8 tag;U8 bytes[8];};Box o;o=42;o.bytes[0]=0;return o;}F();|}

let compile ?(contents = contents) mode =
  let session = Session.create () in
  let source = Session.add_source session ~path:"member-proof.hc" ~contents in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  match compile_integer_program session ~config ~source with
  | Ok compiled -> compiled.value
  | Error diagnostics ->
      failwith
        (String.concat "; "
           (List.map
              (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
              diagnostics))

let first_definition compiled = integer_program_functions compiled |> List.hd

let projection definition =
  Body.body definition.VM.body
  |> Graph.blocks
  |> List.concat_map (fun block -> Graph.instructions block |> Seq.instructions)
  |> List.find_map (fun instruction ->
      match (Seq.description instruction).payload with
      | Some (Seq.Member_projection _ as proof) -> Some proof
      | _ -> None)
  |> Option.get

let rebuild definition transform =
  let original = definition.VM.body in
  let graph = Body.body original in
  let blocks =
    Graph.blocks graph
    |> List.map (fun block ->
        {
          Graph.block_id = Graph.block_id block;
          instructions =
            Graph.instructions block |> Seq.instructions
            |> List.map (fun instruction ->
                transform (Seq.description instruction));
        })
  in
  let graph =
    Graph.create ~entry:(Graph.block_id (Graph.entry graph)) blocks
    |> Result.get_ok
  in
  let member member =
    {
      Body.position = Body.member_position member;
      symbol = Body.member_symbol member;
      type_ = Body.member_type member;
      span = Body.member_span member;
    }
  in
  let body =
    Body.create
      {
        function_id = Body.function_id original;
        symbol = Body.symbol original;
        function_scope = Body.function_scope original;
        return_type = Body.return_type original;
        parameters = List.map member (Body.parameters original);
        locals = List.map member (Body.locals original);
        stored_flags = Body.stored_flags original;
        compiler_options = Body.compiler_options original;
        span = Body.span original;
        body = graph;
      }
    |> Result.get_ok
  in
  { definition with VM.body }

let controls ?(contents = contents) mode =
  let original = compile ~contents mode and foreign = compile ~contents mode in
  let definition = first_definition original in
  let foreign_projection = projection (first_definition foreign) in
  let substitute payload (d : Seq.description) =
    match d.payload with
    | Some (Seq.Member_projection _) -> { d with payload }
    | _ -> d
  in
  let wrong_offset (d : Seq.description) =
    match (d.opcode, d.payload, d.target_type) with
    | Ir_opcode.Ic_imm_i64, Some (Seq.Integer 1L), Some type_
      when Semantic_type.pointer_depth type_ = 1 ->
        { d with payload = Some (Seq.Integer 2L) }
    | _ -> d
  in
  let wrong_stride (d : Seq.description) =
    match (d.opcode, d.payload, d.target_type) with
    | Ir_opcode.Ic_imm_i64, Some (Seq.Integer 3L), Some type_
      when Semantic_type.pointer_depth type_ = 1 ->
        { d with payload = Some (Seq.Integer 4L) }
    | _ -> d
  in
  let pointee_proofs =
    if
      contents <> Aggregate_pointer_cases.proof_source
      && contents <> local_pointer_contents
    then []
    else
      let proof definition =
        Body.body definition.VM.body
        |> Graph.blocks
        |> List.concat_map (fun block ->
            Graph.instructions block |> Seq.instructions)
        |> List.find_map (fun instruction ->
            match (Seq.description instruction).payload with
            | Some (Seq.Pointee_stride _ as proof) -> Some proof
            | _ -> None)
        |> Option.get
      in
      let substitute payload (d : Seq.description) =
        match d.payload with
        | Some (Seq.Pointee_stride _) -> { d with payload }
        | _ -> d
      in
      let foreign_proof = proof (first_definition foreign) in
      let later_function_proof =
        proof (List.nth (integer_program_functions original) 1)
      in
      let frame = definition.VM.frame in
      let instruction =
        Body.body definition.VM.body
        |> Graph.blocks
        |> List.concat_map (fun block ->
            Graph.instructions block |> Seq.instructions)
        |> List.find_map (fun instruction ->
            let description = Seq.description instruction in
            match description.payload with
            | Some (Seq.Pointee_stride layout) -> Some (description, layout)
            | _ -> None)
        |> Option.get
      in
      let description, layout = instruction in
      let pointer_type = Option.get description.target_type in
      let stride = Semantic_aggregate_pointee_layout.byte_size layout in
      let before_item_index =
        Semantic_function_frame_layout.function_item_index frame
      in
      let matches ?function_symbol () =
        Semantic_aggregate_pointee_layout.matches ?function_symbol layout
          ~before_item_index ~pointer_type ~stride
      in
      let own = Semantic_function_frame_layout.function_symbol frame in
      let other =
        List.nth (integer_program_functions original) 1 |> fun value ->
        Semantic_function_frame_layout.function_symbol value.VM.frame
      in
      if not (matches ~function_symbol:own ()) then
        failwith "original pointee proof lost its exact function owner";
      if matches ~function_symbol:other () then
        failwith "reused numeric function index admitted another function owner";
      if matches () then
        failwith "numeric function index alone admitted a source pointee proof";
      [
        ( "foreign equal-name pointee layout",
          rebuild definition (substitute (Some foreign_proof)) );
        ("missing selected pointee layout", rebuild definition (substitute None));
        ( "scaled offset differs from selected pointee",
          rebuild definition wrong_stride );
        ( "pointee layout belongs to another function",
          rebuild definition (substitute (Some later_function_proof)) );
      ]
  in
  let backing_proofs =
    if
      contents <> Backed_aggregate_cases.proof_source
      && contents <> local_backing_contents
    then []
    else
      let proof definition =
        Body.body definition.VM.body
        |> Graph.blocks
        |> List.concat_map (fun block ->
            Graph.instructions block |> Seq.instructions)
        |> List.find_map (fun instruction ->
            match (Seq.description instruction).payload with
            | Some (Seq.Backing_projection _ as proof) -> Some proof
            | _ -> None)
        |> Option.get
      in
      let substitute payload (d : Seq.description) =
        match d.payload with
        | Some (Seq.Backing_projection _) -> { d with payload }
        | _ -> d
      in
      let wrong_backing_offset (d : Seq.description) =
        match (d.opcode, d.payload, d.target_type) with
        | Ir_opcode.Ic_imm_i64, Some (Seq.Integer 0L), Some type_
          when Semantic_type.pointer_depth type_ = 1 ->
            { d with payload = Some (Seq.Integer 1L) }
        | _ -> d
      in
      [
        ( "foreign equal-name backing proof",
          rebuild definition
            (substitute (Some (proof (first_definition foreign)))) );
        ("missing backing proof", rebuild definition (substitute None));
        ( "backing proof belongs to another function",
          rebuild definition
            (substitute
               (Some (proof (List.nth (integer_program_functions original) 1))))
        );
        ( "backing projection changes its zero offset",
          rebuild definition wrong_backing_offset );
      ]
  in
  ( original,
    rebuild definition Fun.id,
    [
      ( "foreign equal-name field proof",
        rebuild definition (substitute (Some foreign_projection)) );
      ("missing field proof", rebuild definition (substitute None));
      ("offset differs from field proof", rebuild definition wrong_offset);
    ]
    @ pointee_proofs @ backing_proofs
    @
    if contents = Aggregate_array_cases.proof_source then
      [
        ( "root array stride differs from selected element",
          rebuild definition wrong_stride );
      ]
    else [] )
