module Source = Sema.Initializer_source
module Shape = Integer_storage_shape

type operation = Scalar_store | Copy_bytes of string

type entry = {
  leaf : Source.leaf;
  cell_offset : int;
  byte_offset : int;
  operation : operation;
}

type t = { source : Source.t; entries : entry list }
type token = Open | Close | Comma | Expression of Source.leaf

let source layout = layout.source
let entries layout = layout.entries
let leaf entry = entry.leaf
let cell_offset entry = entry.cell_offset
let byte_offset entry = entry.byte_offset
let operation entry = entry.operation

let find layout leaf =
  List.find_opt (fun entry -> entry.leaf == leaf) layout.entries

let ( let* ) = Result.bind

let tokens source =
  let rec value leaves reversed = function
    | Frontend.Ast.Scalar_initializer expression ->
        begin match leaves with
        | leaf :: rest when Source.leaf_expression_ast leaf == expression ->
            (rest, Expression leaf :: reversed)
        | _ -> invalid_arg "initializer source lost its original leaf"
        end
    | Frontend.Ast.Braced_initializer group ->
        let leaves, reversed =
          elements leaves (Open :: reversed) group.initializer_elements
        in
        (leaves, Close :: reversed)
    | Frontend.Ast.Unbraced_array_initializer group ->
        let leaves, reversed =
          elements leaves reversed group.unbraced_initializer_elements
        in
        ( leaves,
          if Option.is_some group.unbraced_initializer_closing_brace then
            Close :: reversed
          else reversed )
  and elements leaves reversed = function
    | [] -> (leaves, reversed)
    | element :: rest ->
        let leaves, reversed =
          value leaves reversed element.Frontend.Ast.initializer_element_value
        in
        let reversed =
          if Option.is_some element.initializer_element_comma then
            Comma :: reversed
          else reversed
        in
        elements leaves reversed rest
  in
  let remaining, reversed =
    value (Source.leaves source) [] (Source.source_ast source)
  in
  assert (remaining = []);
  List.rev reversed

let create ~shape source =
  let invalid detail =
    Error ("HCRUN0006: persistent array initializer " ^ detail)
  in
  let width = Shape.byte_size shape / Shape.element_count shape in
  let entry leaf cell_offset operation =
    { leaf; cell_offset; byte_offset = cell_offset * width; operation }
  in
  let rec consume dimensions offset reversed input =
    match (dimensions, input) with
    | [], Expression leaf :: rest ->
        Ok (entry leaf offset Scalar_store :: reversed, rest)
    | [], _ -> invalid "requires an expression for each scalar element"
    | count :: tail, Expression leaf :: rest when width = 1 ->
        begin match Source.leaf_expression_ast leaf with
        | Frontend.Ast.String_literal literal ->
            if tail <> [] then
              invalid "has unresolved direct-string copying at a non-final rank"
            else
              begin match literal.literal_value with
              | Frontend.Ast.Bytes_value bytes ->
                  let count = Int64.to_int count in
                  if count > String.length bytes + 1 then
                    invalid
                      "string copy extends beyond its source-owned terminator"
                  else
                    let copied =
                      if count <= String.length bytes then
                        String.sub bytes 0 count
                      else bytes ^ "\000"
                    in
                    Ok (entry leaf offset (Copy_bytes copied) :: reversed, rest)
              | _ -> invalid "has inconsistent string source evidence"
              end
        | _ -> array count tail offset reversed input
        end
    | count :: tail, _ -> array count tail offset reversed input
  and array count tail offset reversed input =
    let count = Int64.to_int count in
    let stride =
      List.fold_left (fun n extent -> n * Int64.to_int extent) 1 tail
    in
    let input =
      match input with
      | Open :: rest -> rest
      | _ -> input
    in
    let rec loop index reversed input =
      if index = count then
        Ok
          ( reversed,
            match input with
            | Close :: rest -> rest
            | _ -> input )
      else
        let* reversed, input =
          consume tail (offset + (index * stride)) reversed input
        in
        let input =
          match input with
          | Comma :: rest when count > 1 -> rest
          | _ -> input
        in
        loop (index + 1) reversed input
    in
    loop 0 reversed input
  in
  let* reversed, remaining =
    consume (Shape.dimensions shape) 0 [] (tokens source)
  in
  if remaining <> [] then
    invalid "has extra expressions or unmatched delimiters"
  else Ok { source; entries = List.rev reversed }
