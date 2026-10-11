module Sequence = Instruction_sequence
module Source = Sema.Exception_region
module Int_map = Map.Make (Int)

type scope = Source.t * Source.arm
type block_context = { block : Sequence.Block_id.t; scopes : scope list }

type description = {
  source : Source.t;
  enter : Sequence.description;
  leave : Sequence.description;
  early_leaves : Sequence.description list;
  handler_return : Sequence.description;
  catch_target : Sequence.Block_id.t;
  untry_target : Sequence.Block_id.t;
}

type region = {
  source_ : Source.t;
  catch_target_ : Sequence.Block_id.t;
  untry_target_ : Sequence.Block_id.t;
}

type operation = Enter of region | Leave of region | Catch_finish of region

type t = {
  graph_ : Block_graph.t;
  regions_ : region list;
  markers_ : (Sequence.description * operation) Int_map.t;
  original_blocks_ : Block_graph.block list;
}

let source region = region.source_
let catch_target region = region.catch_target_
let untry_target region = region.untry_target_
let regions context = context.regions_
let is_empty context = context.regions_ = []

let matches_graph context graph =
  context.graph_ == graph
  &&
  let current = Block_graph.blocks graph in
  List.length current = List.length context.original_blocks_
  && List.for_all2 ( == ) current context.original_blocks_

let matches_records context records =
  List.for_all
    (fun region -> Source.matches_records region.source_ records)
    context.regions_

let matches_function context body =
  match body with
  | None ->
      List.for_all
        (fun region -> Source.matches_header region.source_ None)
        context.regions_
  | Some body -> (
      match Function_body.definition_declaration body with
      | None -> is_empty context
      | Some declaration ->
          let header =
            Sema.Function_resolution.resolved_declaration_site declaration
            |> Sema.Function_resolution.declaration_site_function
          in
          List.for_all
            (fun region -> Source.matches_header region.source_ (Some header))
            context.regions_)

let find context description =
  match
    Int_map.find_opt
      (Sequence.Instruction_id.to_int description.Sequence.instruction_id)
      context.markers_
  with
  | Some (original, operation) when original == description -> Some operation
  | _ -> None

let same_scopes left right =
  List.length left = List.length right
  && List.for_all2
       (fun (left, left_arm) (right, right_arm) ->
         left == right && left_arm = right_arm)
       left right

let matches_ancestry source scopes =
  let original = Source.ancestry source in
  List.length original = List.length scopes
  && List.for_all2
       (fun (original, arm) (source, actual_arm) ->
         original == Source.source source && arm = actual_arm)
       original scopes

let marker_shape opcode (description : Sequence.description) =
  description.opcode = opcode
  && description.operands = []
  && Option.is_none description.result
  && Option.is_none description.target_type
  && Option.is_none description.payload
  && description.flags = 0L

let create ~span ~graph ~blocks descriptions =
  let errors = ref [] in
  let add ?(code = "HCRUN0004") ?(at = span) message =
    errors :=
      Common.Diagnostic.make ~code ~severity:Common.Diagnostic.Error ~message
        ~primary:at ()
      :: !errors
  in
  let graph_blocks = Block_graph.blocks graph in
  let graph_index =
    List.fold_left
      (fun found block ->
        Int_map.add
          (Sequence.Block_id.to_int (Block_graph.block_id block))
          block found)
      Int_map.empty graph_blocks
  in
  let instruction_index =
    List.fold_left
      (fun found block ->
        Sequence.instructions (Block_graph.instructions block)
        |> List.fold_left
             (fun found instruction ->
               let description = Sequence.description instruction in
               Int_map.add
                 (Sequence.Instruction_id.to_int description.instruction_id)
                 description found)
             found)
      Int_map.empty graph_blocks
  in
  let contexts =
    List.fold_left
      (fun found (context : block_context) ->
        let id = Sequence.Block_id.to_int context.block in
        if Int_map.mem id found then
          add "exception block context repeats an original block";
        if not (Int_map.mem id graph_index) then
          add "exception block context names a foreign block";
        Int_map.add id context.scopes found)
      Int_map.empty blocks
  in
  if Int_map.cardinal contexts <> List.length graph_blocks then
    add "exception contexts do not cover every original graph block";
  let context_for target =
    match Int_map.find_opt (Sequence.Block_id.to_int target) contexts with
    | Some scopes -> scopes
    | None ->
        add "exception target has no original block context";
        []
  in
  let regions =
    List.map
      (fun description ->
        {
          source_ = description.source;
          catch_target_ = description.catch_target;
          untry_target_ = description.untry_target;
        })
      descriptions
  in
  let known source =
    List.exists (fun region -> region.source_ == source) regions
  in
  let seen_sources = ref [] in
  List.iter
    (fun region ->
      if
        List.exists
          (fun previous ->
            Source.source previous == Source.source region.source_)
          !seen_sources
      then add "exception source was lowered more than once in this graph";
      seen_sources := region.source_ :: !seen_sources)
    regions;
  let rec check_scopes = function
    | [] -> ()
    | (source, _) :: rest ->
        if not (known source) then add "exception scope has no original region";
        if not (matches_ancestry source rest) then
          add "exception scope does not preserve its original source ancestry";
        check_scopes rest
  in
  Int_map.iter (fun _ scopes -> check_scopes scopes) contexts;
  if context_for (Block_graph.block_id (Block_graph.entry graph)) <> [] then
    add "graph entry begins inside an exception scope";
  let markers = ref Int_map.empty in
  let add_marker opcode operation (description : Sequence.description) =
    let id = Sequence.Instruction_id.to_int description.instruction_id in
    let at = Option.value description.span ~default:span in
    if not (marker_shape opcode description) then
      add ~at "exception marker has a changed instruction shape";
    (match Int_map.find_opt id instruction_index with
    | Some original when original == description -> ()
    | _ -> add ~at "exception marker is not its physical original instruction");
    if Int_map.mem id !markers then
      add ~at "one instruction claims more than one exception operation";
    markers := Int_map.add id (description, operation) !markers
  in
  List.iter2
    (fun description region ->
      add_marker Opcode.Ic_nop1 (Enter region) description.enter;
      add_marker Opcode.Ic_nop1 (Leave region) description.leave;
      List.iter
        (add_marker Opcode.Ic_nop1 (Leave region))
        description.early_leaves;
      add_marker Opcode.Ic_ret (Catch_finish region) description.handler_return;
      let parents =
        match context_for description.catch_target with
        | (source, Source.Catch_body) :: parents when source == region.source_
          -> parents
        | _ ->
            add "catch target does not begin in its original catch scope";
            []
      in
      if not (matches_ancestry region.source_ parents) then
        add "catch target does not retain its original parent scopes";
      if
        not
          (same_scopes
             (context_for description.untry_target)
             ((region.source_, Source.Try_body) :: parents))
      then add "untry target does not retain its original active handler";
      match
        Int_map.find_opt
          (Sequence.Block_id.to_int description.untry_target)
          graph_index
      with
      | None -> add "untry target is outside its original graph"
      | Some block ->
          let first =
            Sequence.instructions (Block_graph.instructions block)
            |> List.find_map (fun instruction ->
                let original = Sequence.description instruction in
                if original.opcode = Opcode.Ic_label then None
                else Some original)
          in
          if
            not (Option.fold ~none:false ~some:(( == ) description.leave) first)
          then add "untry target does not start with its original leave marker")
    descriptions regions;
  let context =
    {
      graph_ = graph;
      regions_ = regions;
      markers_ = !markers;
      original_blocks_ = graph_blocks;
    }
  in
  List.iter
    (fun block ->
      let scopes = ref (context_for (Block_graph.block_id block)) in
      Sequence.instructions (Block_graph.instructions block)
      |> List.iter (fun instruction ->
          let description = Sequence.description instruction in
          let at = Option.value description.span ~default:span in
          match find context description with
          | Some (Enter region) ->
              if not (matches_ancestry region.source_ !scopes) then
                add ~at "exception entry has a foreign source scope";
              scopes := (region.source_, Source.Try_body) :: !scopes
          | Some (Leave region) -> (
              match !scopes with
              | (source, Source.Try_body) :: rest when source == region.source_
                -> scopes := rest
              | _ ->
                  add ~at "exception leave does not own the innermost handler")
          | Some (Catch_finish region) -> (
              match !scopes with
              | (source, Source.Catch_body) :: rest
                when source == region.source_ && matches_ancestry source rest ->
                  ()
              | _ -> add ~at "catch return does not own its original handler")
          | None -> (
              match (description.opcode, description.payload, !scopes) with
              | (Opcode.Ic_ret | Opcode.Ic_end), None, _ :: _ ->
                  add ~code:"HCRUN0001" ~at
                    "return leaves an active try or catch without its original \
                     cleanup"
              | _ -> ()));
      List.iter
        (fun target ->
          if not (same_scopes !scopes (context_for target)) then
            add ~code:"HCRUN0001"
              "control flow crosses a try/catch boundary without its original \
               handler transition")
        (Block_graph.successors block))
    graph_blocks;
  match List.rev !errors with
  | [] -> Ok context
  | diagnostics -> Error diagnostics
