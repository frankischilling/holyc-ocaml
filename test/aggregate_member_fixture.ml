open Holyc_lib
module VM = Ir_integer_interpreter
module Body = Ir_function_body
module Graph = Ir_block_graph
module Seq = Ir_instruction_sequence

let contents =
  {|class Box{U8 tag;U16 value;};I64 F(){Box o;o.value=42;return o.value;}F();|}

let compile mode =
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

let controls mode =
  let original = compile mode and foreign = compile mode in
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
  ( original,
    rebuild definition Fun.id,
    [
      ( "foreign equal-name field proof",
        rebuild definition (substitute (Some foreign_projection)) );
      ("missing field proof", rebuild definition (substitute None));
      ("offset differs from field proof", rebuild definition wrong_offset);
    ] )
