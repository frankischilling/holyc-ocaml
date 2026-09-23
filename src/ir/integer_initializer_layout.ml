module Source = Sema.Initializer_source
module Shape = Integer_storage_shape

type operation = Scalar_store | Copy_bytes of string

type entry = {
  leaf : Source.leaf;
  cell_offset : int;
  byte_offset : int;
  operation : operation;
  declared_owner_ : Sema.Compiler_record.declared_global option;
}

type t = { source : Source.t; entries : entry list }
type 'leaf token = Open | Close | Comma | Expression of 'leaf

let source layout = layout.source
let entries layout = layout.entries
let leaf entry = entry.leaf
let cell_offset entry = entry.cell_offset
let byte_offset entry = entry.byte_offset
let operation entry = entry.operation
let declared_owner entry = entry.declared_owner_

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
  | Consume of int64 list
  | Array_start of int * int64 list
  | Array_next of int * int64 list * int
  | Optional_comma of int
  | Optional_close

type progress = { pending : continuation list; next_cell : int }

let initial_progress dimensions =
  { pending = [ Consume dimensions ]; next_cell = 0 }

let advance_tokens ~shape ~final ~expression_of_leaf ~make_entry progress input
    =
  let width = Shape.byte_size shape / Shape.element_count shape in
  let entry leaf cell_offset operation =
    make_entry leaf cell_offset (cell_offset * width) operation
  in
  let rec run work offset reversed input =
    match (work, input) with
    | [], [] -> Ok ({ pending = []; next_cell = offset }, List.rev reversed)
    | [], _ -> invalid "has extra expressions or unmatched delimiters"
    | _, [] when not final ->
        Ok ({ pending = work; next_cell = offset }, List.rev reversed)
    | Consume [] :: rest, Expression leaf :: input ->
        if offset >= Shape.element_count shape then
          invalid "scalar destination exceeds its declared object"
        else
          run rest (offset + 1)
            (entry leaf offset Scalar_store :: reversed)
            input
    | Consume [] :: _, _ ->
        invalid "requires an expression for each scalar element"
    | Consume (count :: _) :: rest, Expression leaf :: input
      when width = 1
           &&
           match expression_of_leaf leaf with
           | Frontend.Ast.String_literal _ -> true
           | _ -> false -> (
        match expression_of_leaf leaf with
        | Frontend.Ast.String_literal
            { literal_value = Frontend.Ast.Bytes_value bytes; _ } ->
            let count = Int64.to_int count in
            if count > String.length bytes + 1 then
              invalid "string copy extends beyond its source-owned terminator"
            else if count > Shape.element_count shape - offset then
              invalid "string copy extends beyond its declared object"
            else
              let copied =
                if count <= String.length bytes then String.sub bytes 0 count
                else bytes ^ "\000"
              in
              run rest (offset + count)
                (entry leaf offset (Copy_bytes copied) :: reversed)
                input
        | _ -> invalid "has inconsistent string source evidence")
    | Consume (count :: tail) :: rest, _ ->
        run
          (Array_start (Int64.to_int count, tail) :: rest)
          offset reversed input
    | Array_start (count, tail) :: rest, _ ->
        let input =
          match input with
          | Open :: rest -> rest
          | _ -> input
        in
        run (Array_next (count, tail, 0) :: rest) offset reversed input
    | Array_next (count, _, index) :: rest, _ when count = index ->
        run (Optional_close :: rest) offset reversed input
    | Array_next (count, tail, index) :: rest, _ ->
        run
          (Consume tail :: Optional_comma count
          :: Array_next (count, tail, index + 1)
          :: rest)
          offset reversed input
    | Optional_comma count :: rest, _ ->
        let input =
          match input with
          | Comma :: rest when count > 1 -> rest
          | _ -> input
        in
        run rest offset reversed input
    | Optional_close :: rest, _ ->
        let input =
          match input with
          | Close :: rest -> rest
          | _ -> input
        in
        run rest offset reversed input
  in
  run progress.pending progress.next_cell [] input

let advance ~shape ~final work input =
  advance_tokens ~shape ~final ~expression_of_leaf:Source.leaf_expression_ast
    ~make_entry:(fun leaf cell_offset byte_offset operation ->
      { leaf; cell_offset; byte_offset; operation; declared_owner_ = None })
    work input

type stream = { stream_shape : Shape.t; stream_work : progress }

let begin_stream shape =
  {
    stream_shape = shape;
    stream_work = initial_progress (Shape.dimensions shape);
  }

let delimiter_token = function
  | Frontend.Parser.Initializer_open _ -> Open
  | Frontend.Parser.Initializer_close _ -> Close
  | Frontend.Parser.Initializer_comma _ -> Comma

let prepare_stream stream ~delimiters ~value =
  match value with
  | Frontend.Ast.Braced_initializer _
  | Frontend.Ast.Unbraced_array_initializer _ ->
      invalid "stream requires one original scalar leaf"
  | Frontend.Ast.Scalar_initializer expression -> (
      let* work, entries =
        advance_tokens ~shape:stream.stream_shape ~final:false
          ~expression_of_leaf:Fun.id
          ~make_entry:(fun _ cell_offset byte_offset operation ->
            (cell_offset, byte_offset, operation))
          stream.stream_work
          (List.map delimiter_token delimiters @ [ Expression expression ])
      in
      match entries with
      | [ entry ] -> Ok ({ stream with stream_work = work }, entry)
      | _ -> invalid "stream did not consume exactly one original leaf")

let create ~shape source =
  let* _, entries =
    advance ~shape ~final:true
      (initial_progress (Shape.dimensions shape))
      (tokens source)
  in
  Ok { source; entries }

type live = {
  declaration : Sema.Compiler_record.declared_global;
  shape : Shape.t;
  work : progress;
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
          work = initial_progress dimensions;
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
    let token = delimiter_token receipt.delimiter_value in
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
            let entry =
              { entry with declared_owner_ = Some live.declaration }
            in
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
