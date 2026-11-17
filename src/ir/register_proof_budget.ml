module Seq = Instruction_sequence
module Values = Map.Make (Seq.Value_id)
module Typed = Sema.Function_call_expression_result

type t = { mutable remaining : int }

exception Exhausted

let max_depth = 256
let remaining budget = budget.remaining

let add left right =
  if left < 0 || right < 0 || left > max_int - right then raise Exhausted;
  left + right

let capacity ~source_bytes ~retained_nodes =
  let units = add (add 1 source_bytes) retained_nodes in
  if units > max_int / 64 then raise Exhausted;
  units * 64

let charge budget work =
  if work < 0 || work > budget.remaining then raise Exhausted;
  budget.remaining <- budget.remaining - work

let enter budget depth =
  if depth <= 0 || depth > max_depth then raise Exhausted;
  charge budget 1

let create ?max_work body =
  let source_bytes =
    let definition =
      Option.bind
        (Function_body.definition_source body)
        Typed.function_source_body
    in
    match (Function_body.span body, definition) with
    | Some span, Some definition
      when span = definition.Frontend.Ast.location.span
           && span.start >= 0 && span.stop >= span.start ->
        span.stop - span.start
    | _ -> raise Exhausted
  in
  let count = ref 0 in
  let include_list items = List.iter (fun _ -> count := add !count 1) items in
  include_list (Function_body.parameters body);
  include_list (Function_body.locals body);
  Option.iter
    (fun source -> include_list (Typed.function_all_results source))
    (Function_body.definition_source body);
  List.iter
    (fun block ->
      count := add !count 1;
      include_list (Seq.instructions (Block_graph.instructions block)))
    (Block_graph.blocks (Function_body.body body));
  let capacity = capacity ~source_bytes ~retained_nodes:!count in
  let remaining =
    match max_work with
    | None -> capacity
    | Some limit when limit >= 0 -> min limit capacity
    | Some _ -> raise Exhausted
  in
  { remaining }

let protect f = try f () with Exhausted -> None

let users budget code =
  let index =
    List.fold_left
      (fun index (item : Seq.description) ->
        charge budget (1 + List.length item.operands);
        List.fold_left
          (fun index operand ->
            let previous =
              Option.value ~default:[] (Values.find_opt operand index)
            in
            Values.add operand (item :: previous) index)
          index
          (List.sort_uniq Seq.Value_id.compare item.operands))
      Values.empty code
    |> Values.map List.rev
  in
  fun id ->
    charge budget 1;
    Option.value ~default:[] (Values.find_opt id index)

module Identity (Key : sig
  type t
end) =
Hashtbl.Make (struct
  type t = Key.t

  let equal = ( == )
  let hash = Hashtbl.hash
end)
