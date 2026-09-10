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

let invalid detail = Error ("HCRUN0006: persistent array initializer " ^ detail)

type continuation =
  | Consume of int64 list * int
  | Array_start of int * int64 list * int
  | Array_next of int * int64 list * int * int
  | Optional_comma of int
  | Optional_close

let advance ~shape ~final work input =
  let width = Shape.byte_size shape / Shape.element_count shape in
  let entry leaf cell_offset operation =
    { leaf; cell_offset; byte_offset = cell_offset * width; operation }
  in
  let rec run work reversed input =
    match (work, input) with
    | [], [] -> Ok ([], List.rev reversed)
    | [], _ -> invalid "has extra expressions or unmatched delimiters"
    | _, [] when not final -> Ok (work, List.rev reversed)
    | Consume ([], offset) :: rest, Expression leaf :: input ->
        run rest (entry leaf offset Scalar_store :: reversed) input
    | Consume ([], _) :: _, _ ->
        invalid "requires an expression for each scalar element"
    | Consume (count :: tail, offset) :: rest, Expression leaf :: input
      when width = 1
           &&
           match Source.leaf_expression_ast leaf with
           | Frontend.Ast.String_literal _ -> true
           | _ -> false -> (
        if tail <> [] then
          invalid "has unresolved direct-string copying at a non-final rank"
        else
          match Source.leaf_expression_ast leaf with
          | Frontend.Ast.String_literal
              { literal_value = Frontend.Ast.Bytes_value bytes; _ } ->
              let count = Int64.to_int count in
              if count > String.length bytes + 1 then
                invalid "string copy extends beyond its source-owned terminator"
              else
                let copied =
                  if count <= String.length bytes then String.sub bytes 0 count
                  else bytes ^ "\000"
                in
                run rest
                  (entry leaf offset (Copy_bytes copied) :: reversed)
                  input
          | _ -> invalid "has inconsistent string source evidence")
    | Consume (count :: tail, offset) :: rest, _ ->
        run
          (Array_start (Int64.to_int count, tail, offset) :: rest)
          reversed input
    | Array_start (count, tail, offset) :: rest, _ ->
        let input =
          match input with
          | Open :: rest -> rest
          | _ -> input
        in
        run (Array_next (count, tail, offset, 0) :: rest) reversed input
    | Array_next (count, _, _, index) :: rest, _ when count = index ->
        run (Optional_close :: rest) reversed input
    | Array_next (count, tail, offset, index) :: rest, _ ->
        let stride =
          List.fold_left (fun n extent -> n * Int64.to_int extent) 1 tail
        in
        run
          (Consume (tail, offset + (index * stride))
          :: Optional_comma count
          :: Array_next (count, tail, offset, index + 1)
          :: rest)
          reversed input
    | Optional_comma count :: rest, _ ->
        let input =
          match input with
          | Comma :: rest when count > 1 -> rest
          | _ -> input
        in
        run rest reversed input
    | Optional_close :: rest, _ ->
        let input =
          match input with
          | Close :: rest -> rest
          | _ -> input
        in
        run rest reversed input
  in
  run work [] input

let create ~shape source =
  let* _, entries =
    advance ~shape ~final:true
      [ Consume (Shape.dimensions shape, 0) ]
      (tokens source)
  in
  Ok { source; entries }

type live = {
  declaration : Sema.Compiler_record.declared_global;
  shape : Shape.t;
  work : continuation list;
  entries_rev : entry list;
  last_delimiter : Frontend.Parser.completed_initializer_delimiter option;
  delimiters_rev : Frontend.Parser.initializer_delimiter list;
}

let begin_live declaration =
  let type_ =
    declaration |> Sema.Compiler_record.declared_global_type
    |> Sema.Type_reference.resolved_type
  in
  let dimensions =
    Sema.Compiler_record.declared_global_dimensions declaration
  in
  match Shape.create ~type_ ~dimensions with
  | Error _ -> invalid "has no checked supported live storage shape"
  | Ok shape ->
      Ok
        {
          declaration;
          shape;
          work = [ Consume (dimensions, 0) ];
          entries_rev = [];
          last_delimiter = None;
          delimiters_rev = [];
        }

let same_optional_identity left right =
  match (left, right) with
  | None, None -> true
  | Some left, Some right -> left == right
  | _ -> false

let previous_leaf live =
  match live.entries_rev with
  | [] -> None
  | previous :: _ -> Source.leaf_parser_receipt previous.leaf

let observe_live_delimiter live
    (receipt : Frontend.Parser.completed_initializer_delimiter) =
  let expected_index =
    Option.fold ~none:0
      ~some:(fun previous -> previous.Frontend.Parser.delimiter_index + 1)
      live.last_delimiter
  in
  if
    receipt.delimiter_initializer.initializer_owner
    != Sema.Compiler_record.declared_global_source live.declaration
    || receipt.delimiter_index <> expected_index
    || (not
          (same_optional_identity live.last_delimiter
             receipt.delimiter_predecessor))
    || not
         (same_optional_identity (previous_leaf live)
            receipt.delimiter_leaf_predecessor)
  then invalid "live delimiter is foreign, repeated or out of order"
  else
    let token =
      match receipt.delimiter_value with
      | Frontend.Parser.Initializer_open _ -> Open
      | Frontend.Parser.Initializer_close _ -> Close
      | Frontend.Parser.Initializer_comma _ -> Comma
    in
    let* work, entries =
      advance ~shape:live.shape ~final:false live.work [ token ]
    in
    if entries <> [] then invalid "delimiter produced a scalar destination"
    else
      Ok
        {
          live with
          work;
          last_delimiter = Some receipt;
          delimiters_rev = receipt.delimiter_value :: live.delimiters_rev;
        }

let prepare_live live leaf =
  match Source.leaf_parser_receipt leaf with
  | None -> invalid "live leaf has no original parser receipt"
  | Some receipt -> (
      let expected_index, predecessor =
        match live.entries_rev with
        | [] -> (0, None)
        | previous :: _ ->
            ( Source.leaf_index previous.leaf + 1,
              Source.leaf_parser_receipt previous.leaf )
      in
      let same_predecessor =
        match (predecessor, receipt.leaf_predecessor) with
        | None, None -> true
        | Some a, Some b -> a == b
        | _ -> false
      in
      if
        receipt.leaf_initializer.initializer_owner
        != Sema.Compiler_record.declared_global_source live.declaration
        || Source.leaf_index leaf <> expected_index
        || (not same_predecessor)
        || (not
              (same_optional_identity live.last_delimiter
                 receipt.leaf_delimiter_predecessor))
        || List.length live.delimiters_rev
           <> List.length receipt.leaf_delimiters
        || not
             (List.for_all2 ( == )
                (List.rev live.delimiters_rev)
                receipt.leaf_delimiters)
      then invalid "live leaf is foreign, repeated or out of order"
      else
        let* work, entries =
          advance ~shape:live.shape ~final:false live.work [ Expression leaf ]
        in
        match entries with
        | [ entry ] when entry.leaf == leaf ->
            Ok
              ( {
                  live with
                  work;
                  entries_rev = entry :: live.entries_rev;
                  delimiters_rev = [];
                },
                entry )
        | _ -> invalid "live layout did not consume its exact source leaf")

let complete_live live source =
  let* () =
    if
      same_optional_identity live.last_delimiter
        (Source.last_parser_delimiter source)
    then Ok ()
    else invalid "completion omitted original trailing delimiter observations"
  in
  let* _, _ = advance ~shape:live.shape ~final:true live.work [] in
  let* layout = create ~shape:live.shape source in
  let expected = List.rev live.entries_rev in
  if
    List.length expected <> List.length layout.entries
    || not
         (List.for_all2
            (fun left right ->
              left.leaf == right.leaf
              && left.cell_offset = right.cell_offset
              && left.byte_offset = right.byte_offset
              && left.operation = right.operation)
            expected layout.entries)
  then invalid "completion differs from its original live destinations"
  else Ok layout
