module Source = Sema.Initializer_source
module Shape = Integer_storage_shape

type operation = Scalar_store | Copy_bytes of string

type entry = {
  leaf : Source.leaf;
  cell_offset : int;
  byte_offset : int;
  operation : operation;
  scalar_ : Sema.Initializer_type.scalar option;
  declared_owner_ : Sema.Compiler_record.declared_global option;
  evaluation_ : Sema.Initializer_type.scalar_evaluation option;
  settled_ : bool;
}

type t = {
  source : Source.t;
  entries : entry list;
  layout_declaration : Sema.Compiler_record.declared_global option;
  layout_shape : Shape.t;
}

type 'leaf token = Open | Close | Comma | Expression of 'leaf

let source layout = layout.source
let entries layout = layout.entries

let belongs_to_declared_shape layout declaration shape =
  Option.fold ~none:false ~some:(( == ) declaration) layout.layout_declaration
  && Shape.byte_size layout.layout_shape = Shape.byte_size shape
  && Shape.element_size layout.layout_shape = Shape.element_size shape
  && Shape.scalar layout.layout_shape = Shape.scalar shape
  && Shape.dimensions layout.layout_shape = Shape.dimensions shape
  &&
  match Sema.Compiler_record.declared_global_aggregate declaration with
  | Some allocation ->
      let matches shape =
        Shape.matches shape
          ~symbol:(Sema.Compiler_record.declared_global_symbol declaration)
          ~type_:(Sema.Compiler_record.global_aggregate_type allocation)
          ~dimensions:
            (Sema.Compiler_record.declared_global_dimensions declaration)
      in
      matches layout.layout_shape && matches shape
  | None -> not (Shape.is_aggregate shape)

let leaf entry = entry.leaf
let cell_offset entry = entry.cell_offset
let byte_offset entry = entry.byte_offset
let operation entry = entry.operation
let scalar entry = entry.scalar_
let declared_owner entry = entry.declared_owner_

let requires_settlement entry =
  Option.is_some entry.evaluation_ && not entry.settled_

let normalized_word scalar bits =
  if Sema.Initializer_type.scalar_byte_size scalar = 0 then Ok bits
  else
    match
      Integer_scalar_storage.of_type
        (Sema.Initializer_type.scalar_value_type scalar)
    with
    | Some storage when Sema.Initializer_type.scalar_byte_size scalar <= 8 ->
        Ok (Integer_scalar_storage.normalize storage bits)
    | _ ->
        Error
          "HCRUN0006: initializer leaf requires supported integer value storage"

let word_bytes scalar bits =
  let ( let* ) = Result.bind in
  let* bits = normalized_word scalar bits in
  let count = Sema.Initializer_type.scalar_byte_size scalar in
  Ok
    (String.init count (fun index ->
         Int64.shift_right_logical bits (8 * index)
         |> Int64.logand 255L |> Int64.to_int |> Char.chr))

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

module Initializer_type = Sema.Initializer_type

type continuation =
  | Consume of Initializer_type.t * int64 list
  | Scalar_value of Initializer_type.scalar
  | Array_start of int * Initializer_type.t * int64 list
  | Array_next of int * Initializer_type.t * int64 list * int
  | Aggregate_start of Initializer_type.element list
  | Aggregate_next of Initializer_type.element list
  | Optional_comma of bool
  | Optional_close
  | Required_close
  | Missing_type

type progress = { pending : continuation list; next_byte : int }

let initial_progress shape =
  {
    pending =
      (match Shape.initializer_type shape with
      | Some type_ -> [ Consume (type_, Shape.dimensions shape) ]
      | None -> [ Missing_type ]);
    next_byte = 0;
  }

let advance_tokens ~shape ~final ~expression_of_leaf ~make_entry progress input
    =
  let physical_width = Integer_scalar_storage.byte_size (Shape.scalar shape) in
  let entry leaf byte_offset operation scalar =
    make_entry leaf (byte_offset / physical_width) byte_offset operation scalar
  in
  let fits offset bytes =
    offset >= 0 && bytes >= 0
    && offset <= Shape.byte_size shape
    && bytes <= Shape.byte_size shape - offset
  in
  let rec run work offset reversed input =
    match (work, input) with
    | [], [] -> Ok ({ pending = []; next_byte = offset }, List.rev reversed)
    | [], _ -> invalid "has extra expressions or unmatched delimiters"
    | _, [] when not final ->
        Ok ({ pending = work; next_byte = offset }, List.rev reversed)
    | Missing_type :: _, _ ->
        invalid "requires its original selected initializer type"
    | Consume (type_, []) :: rest, _ -> (
        let* value = Initializer_type.view type_ in
        match value with
        | Initializer_type.Scalar scalar ->
            run (Scalar_value scalar :: rest) offset reversed input
        | Initializer_type.Aggregate members ->
            run (Aggregate_start members :: rest) offset reversed input)
    | Consume (type_, count :: tail) :: rest, _ -> (
        let* type_ = Initializer_type.select_forwarding type_ in
        let* value =
          match input with
          | Expression leaf :: _ -> (
              match expression_of_leaf leaf with
              | Frontend.Ast.String_literal _ ->
                  Initializer_type.view type_ |> Result.map Option.some
              | _ -> Ok None)
          | _ -> Ok None
        in
        match (value, input) with
        | Some (Initializer_type.Scalar scalar), Expression leaf :: input
          when Initializer_type.scalar_byte_size scalar = 1
               && (let type_ = Initializer_type.scalar_type scalar in
                   Sema.Type.pointer_depth type_ = 0
                   &&
                   match Sema.Type.base type_ with
                   | Sema.Type.Primitive
                       (_, (Sema.Primitive_type.I8 | U8 | Bool)) -> true
                   | _ -> false)
               &&
               match expression_of_leaf leaf with
               | Frontend.Ast.String_literal _ -> true
               | _ -> false -> (
            match expression_of_leaf leaf with
            | Frontend.Ast.String_literal
                { literal_value = Frontend.Ast.Bytes_value bytes; _ } ->
                if count <= 0L || count > Int64.of_int Int.max_int then
                  invalid "has an invalid string count"
                else
                  let count = Int64.to_int count in
                  if count > String.length bytes + 1 then
                    invalid
                      "string copy extends beyond its source-owned terminator"
                  else if not (fits offset count) then
                    invalid "string copy extends beyond its declared object"
                  else
                    let copied =
                      if count <= String.length bytes then
                        String.sub bytes 0 count
                      else bytes ^ "\000"
                    in
                    run rest (offset + count)
                      (entry leaf offset (Copy_bytes copied) None :: reversed)
                      input
            | _ -> invalid "has inconsistent string source evidence")
        | _ ->
            if count <= 0L || count > Int64.of_int Int.max_int then
              invalid "has an invalid fixed array count"
            else
              run
                (Array_start (Int64.to_int count, type_, tail) :: rest)
                offset reversed input)
    | Scalar_value scalar :: rest, Expression leaf :: input ->
        let bytes = Initializer_type.scalar_byte_size scalar in
        if not (fits offset bytes) then
          invalid "scalar destination exceeds its declared object"
        else
          run rest (offset + bytes)
            (entry leaf offset Scalar_store
               (if Shape.is_aggregate shape then Some scalar else None)
            :: reversed)
            input
    | Scalar_value _ :: _, _ ->
        invalid "requires an expression for each scalar element"
    | Array_start (count, type_, tail) :: rest, _ ->
        let input =
          match input with
          | Open :: tail -> tail
          | _ -> input
        in
        run (Array_next (count, type_, tail, 0) :: rest) offset reversed input
    | Array_next (count, _, _, index) :: rest, _ when count = index ->
        run (Optional_close :: rest) offset reversed input
    | Array_next (count, type_, tail, index) :: rest, _ ->
        run
          (Consume (type_, tail)
          :: Optional_comma (count > 1)
          :: Array_next (count, type_, tail, index + 1)
          :: rest)
          offset reversed input
    | Aggregate_start members :: rest, Open :: input ->
        run (Aggregate_next members :: rest) offset reversed input
    | Aggregate_start _ :: _, _ ->
        invalid "requires '{' for a direct member list"
    | Aggregate_next [] :: rest, _ ->
        run (Required_close :: rest) offset reversed input
    | Aggregate_next (member :: members) :: rest, _ ->
        run
          (Consume
             ( Initializer_type.element_type member,
               Initializer_type.element_dimensions member )
          :: Optional_comma true :: Aggregate_next members :: rest)
          offset reversed input
    | Optional_comma consume :: rest, _ ->
        let input =
          match input with
          | Comma :: tail when consume -> tail
          | _ -> input
        in
        run rest offset reversed input
    | Optional_close :: rest, _ ->
        let input =
          match input with
          | Close :: tail -> tail
          | _ -> input
        in
        run rest offset reversed input
    | Required_close :: rest, Close :: input -> run rest offset reversed input
    | Required_close :: _, _ ->
        invalid "requires '}' after the direct member list"
  in
  run progress.pending progress.next_byte [] input

let advance ~shape ~final work input =
  advance_tokens ~shape ~final ~expression_of_leaf:Source.leaf_expression_ast
    ~make_entry:(fun leaf cell_offset byte_offset operation scalar_ ->
      {
        leaf;
        cell_offset;
        byte_offset;
        operation;
        scalar_;
        declared_owner_ = None;
        evaluation_ = None;
        settled_ = true;
      })
    work input

type prepared_kind =
  | Prepared_scalar of Initializer_type.scalar_evaluation
  | Prepared_string of int

let rec select_prepared_kind ~(first : Frontend.Token.t) = function
  | Consume (type_, []) :: rest ->
      let* phase = Initializer_type.begin_scalar_evaluation type_ in
      Ok (Prepared_scalar phase, rest)
  | Consume (type_, count :: tail) :: rest -> (
      let* type_ = Initializer_type.select_forwarding type_ in
      if count <= 0L || count > Int64.of_int Int.max_int then
        invalid "has an invalid fixed array count"
      else
        let byte_string =
          if first.kind <> Frontend.Token_kind.String then None
          else
            match Initializer_type.begin_scalar_evaluation type_ with
            | Ok phase ->
                let scalar = Initializer_type.scalar_evaluation_before phase in
                let type_ = Initializer_type.scalar_type scalar in
                if
                  Initializer_type.scalar_byte_size scalar = 1
                  && Sema.Type.pointer_depth type_ = 0
                  &&
                  match Sema.Type.base type_ with
                  | Sema.Type.Primitive (_, (Sema.Primitive_type.I8 | U8 | Bool))
                    -> true
                  | _ -> false
                then Some (Prepared_string (Int64.to_int count), rest)
                else None
            | Error _ -> None
        in
        match byte_string with
        | Some result -> Ok result
        | None ->
            select_prepared_kind ~first
              (Array_next (Int64.to_int count, type_, tail, 0) :: rest))
  | Array_start (count, type_, tail) :: rest ->
      select_prepared_kind ~first (Array_next (count, type_, tail, 0) :: rest)
  | Array_next (count, _, _, index) :: rest when count = index ->
      select_prepared_kind ~first (Optional_close :: rest)
  | Array_next (count, type_, tail, index) :: rest ->
      select_prepared_kind ~first
        (Consume (type_, tail)
        :: Optional_comma (count > 1)
        :: Array_next (count, type_, tail, index + 1)
        :: rest)
  | Aggregate_next (member :: members) :: rest ->
      select_prepared_kind ~first
        (Consume
           ( Initializer_type.element_type member,
             Initializer_type.element_dimensions member )
        :: Optional_comma true :: Aggregate_next members :: rest)
  | Optional_comma _ :: rest | Optional_close :: rest ->
      select_prepared_kind ~first rest
  | Scalar_value _ :: _ ->
      invalid "scalar preparation lost its original selected type"
  | _ -> invalid "requires its original scalar branch before expression parsing"

type stream = { stream_shape : Shape.t; stream_work : progress }

type stream_preparation = {
  selected_stream : stream;
  selected_kind : prepared_kind;
  selected_rest : continuation list;
  selected_offset : int;
  selected_dynamic : bool;
  mutable selected_value : Frontend.Ast.initial_value option;
  mutable selected_result :
    (stream * (int * int * operation * Sema.Initializer_type.scalar option))
    option;
  mutable selected_settled : bool;
  mutable selected_failed : bool;
}

let begin_stream shape =
  { stream_shape = shape; stream_work = initial_progress shape }

let delimiter_token = function
  | Frontend.Parser.Initializer_open _ -> Open
  | Frontend.Parser.Initializer_close _ -> Close
  | Frontend.Parser.Initializer_comma _ -> Comma

let observe_stream_delimiter stream delimiter =
  let* work, entries =
    advance_tokens ~shape:stream.stream_shape ~final:false
      ~expression_of_leaf:Fun.id
      ~make_entry:(fun _ _ _ _ _ -> ())
      stream.stream_work
      [ delimiter_token delimiter ]
  in
  if entries <> [] then invalid "delimiter produced a scalar destination"
  else Ok { stream with stream_work = work }

let prepare_stream_expression stream ~first ~delimiters =
  let* work, entries =
    advance_tokens ~shape:stream.stream_shape ~final:false
      ~expression_of_leaf:Fun.id
      ~make_entry:(fun _ _ _ _ _ -> ())
      stream.stream_work
      (List.map delimiter_token delimiters)
  in
  if entries <> [] then invalid "expression preparation consumed a source leaf"
  else
    let* selected_kind, selected_rest =
      select_prepared_kind ~first work.pending
    in
    let selected_dynamic =
      match selected_kind with
      | Prepared_scalar phase ->
          Initializer_type.scalar_evaluation_may_change phase
      | Prepared_string _ -> false
    in
    Ok
      {
        selected_stream = stream;
        selected_kind;
        selected_rest;
        selected_offset = work.next_byte;
        selected_dynamic;
        selected_value = None;
        selected_result = None;
        selected_settled = not selected_dynamic;
        selected_failed = false;
      }

let stream_preparation_requires_settlement preparation =
  preparation.selected_dynamic && not preparation.selected_settled

let stream_preparation_result preparation =
  match (preparation.selected_failed, preparation.selected_result) with
  | true, _ -> invalid "prepared scalar settlement failed"
  | false, Some result -> Ok result
  | false, None -> invalid "prepared expression has no original completed leaf"

let prepare_stream_leaf preparation ~value =
  match preparation.selected_value with
  | Some original when original == value ->
      stream_preparation_result preparation
  | Some _ ->
      invalid "completed expression substituted its original prepared leaf"
  | None ->
      let shape = preparation.selected_stream.stream_shape in
      let* expression =
        match value with
        | Frontend.Ast.Scalar_initializer expression -> Ok expression
        | _ -> invalid "prepared stream requires one original scalar leaf"
      in
      let* operation, scalar, bytes =
        match preparation.selected_kind with
        | Prepared_scalar phase ->
            let scalar = Initializer_type.scalar_evaluation_before phase in
            Ok
              ( Scalar_store,
                (if Shape.is_aggregate shape then Some scalar else None),
                Initializer_type.scalar_byte_size scalar )
        | Prepared_string count -> (
            match expression with
            | Frontend.Ast.String_literal
                { literal_value = Frontend.Ast.Bytes_value bytes; _ }
              when count <= String.length bytes + 1 ->
                let copied =
                  if count <= String.length bytes then String.sub bytes 0 count
                  else bytes ^ "\000"
                in
                Ok (Copy_bytes copied, None, count)
            | _ -> invalid "prepared stream string lost its original literal")
      in
      let offset = preparation.selected_offset in
      if
        offset < 0
        || offset > Shape.byte_size shape
        || bytes < 0
        || bytes > Shape.byte_size shape - offset
      then invalid "prepared stream destination exceeds its original allocation"
      else
        let next =
          {
            preparation.selected_stream with
            stream_work =
              {
                pending = preparation.selected_rest;
                next_byte = offset + bytes;
              };
          }
        in
        let result =
          ( next,
            ( offset / Integer_scalar_storage.byte_size (Shape.scalar shape),
              offset,
              operation,
              scalar ) )
        in
        preparation.selected_value <- Some value;
        preparation.selected_result <- Some result;
        Ok result

let settle_stream_preparation preparation =
  let* _, (cell, byte, operation, _) = stream_preparation_result preparation in
  if not (stream_preparation_requires_settlement preparation) then
    invalid "stream settlement is fixed, repeated or has no original evaluation"
  else
    let outcome =
      let* scalar =
        match preparation.selected_kind with
        | Prepared_scalar phase ->
            Initializer_type.finish_scalar_evaluation phase
        | Prepared_string _ ->
            invalid "string copy cannot settle a scalar phase"
      in
      let shape = preparation.selected_stream.stream_shape in
      let bytes = Initializer_type.scalar_byte_size scalar in
      preparation.selected_settled <- true;
      if
        byte < 0
        || byte > Shape.byte_size shape
        || bytes < 0
        || bytes > Shape.byte_size shape - byte
      then invalid "reached stream destination exceeds its original allocation"
      else
        let next =
          {
            preparation.selected_stream with
            stream_work =
              { pending = preparation.selected_rest; next_byte = byte + bytes };
          }
        in
        let result = (next, (cell, byte, operation, Some scalar)) in
        preparation.selected_result <- Some result;
        Ok ()
    in
    (match outcome with
    | Ok () -> ()
    | Error _ -> preparation.selected_failed <- true);
    outcome

let prepare_stream_typed stream ~delimiters ~value =
  match value with
  | Frontend.Ast.Braced_initializer _
  | Frontend.Ast.Unbraced_array_initializer _ ->
      invalid "stream requires one original scalar leaf"
  | Frontend.Ast.Scalar_initializer expression -> (
      let* work, entries =
        advance_tokens ~shape:stream.stream_shape ~final:false
          ~expression_of_leaf:Fun.id
          ~make_entry:(fun _ cell byte operation scalar ->
            (cell, byte, operation, scalar))
          stream.stream_work
          (List.map delimiter_token delimiters @ [ Expression expression ])
      in
      match entries with
      | [ entry ] -> Ok ({ stream with stream_work = work }, entry)
      | _ -> invalid "stream did not consume exactly one original leaf")

let prepare_stream stream ~delimiters ~value =
  let* next, (cell, byte, operation, scalar) =
    prepare_stream_typed stream ~delimiters ~value
  in
  if Option.is_some scalar then
    invalid "requires its original typed scalar destination"
  else Ok (next, (cell, byte, operation))

let create ~shape source =
  let* _, entries =
    advance ~shape ~final:true (initial_progress shape) (tokens source)
  in
  Ok { source; entries; layout_declaration = None; layout_shape = shape }

let create_static_receipt ~allocation ~shape ~cursor ~delimiters source reached
    =
  let leaves = Source.leaves source in
  if List.length leaves <> List.length reached then
    invalid "static completion omitted or added original source leaves"
  else
    let rec join reversed leaves reached =
      match (leaves, reached) with
      | [], [] -> Ok (List.rev reversed)
      | ( leaf :: tail,
          (receipt, cell_offset, byte_offset, operation, scalar_) :: rest ) ->
          let matches =
            match receipt.Frontend.Parser.static_leaf_value with
            | Frontend.Ast.Scalar_initializer expression ->
                Source.leaf_expression_ast leaf == expression
                && Source.leaf_path leaf = receipt.static_leaf_path
                && Source.leaf_index leaf = receipt.static_leaf_index
            | _ -> false
          in
          let bytes =
            match (operation, scalar_) with
            | Copy_bytes bytes, _ -> String.length bytes
            | Scalar_store, Some scalar ->
                Initializer_type.scalar_byte_size scalar
            | Scalar_store, None ->
                Integer_scalar_storage.byte_size (Shape.scalar shape)
          in
          let completed =
            Option.bind
              (Frontend.Parser.static_initializer_completed_declarator receipt)
              (fun declarator -> declarator.Frontend.Ast.local_initializer)
            |> Option.fold ~none:false ~some:(fun initial ->
                initial.Frontend.Ast.local_initializer_value
                == Source.source_ast source)
          in
          if
            (not matches)
            || receipt.static_allocation != allocation
            || (not completed) || byte_offset < 0
            || byte_offset > Shape.byte_size shape
            || bytes < 0
            || bytes > Shape.byte_size shape - byte_offset
            || cell_offset
               <> byte_offset
                  / Integer_scalar_storage.byte_size (Shape.scalar shape)
          then
            invalid
              "static completion replaced an original source leaf or allocation"
          else
            let entry =
              {
                leaf;
                cell_offset;
                byte_offset;
                operation;
                scalar_;
                declared_owner_ = None;
                evaluation_ = None;
                settled_ = true;
              }
            in
            join (entry :: reversed) tail rest
      | _ -> assert false
    in
    let* entries = join [] leaves reached in
    let source_tokens = tokens source in
    let source_delimiters =
      List.filter
        (function
          | Expression _ -> false
          | _ -> true)
        source_tokens
    in
    if source_delimiters <> List.map delimiter_token delimiters then
      invalid "static completion replaced or omitted original delimiters"
    else
      let* _, _ = advance ~shape ~final:true cursor.stream_work [] in
      Ok { source; entries; layout_declaration = None; layout_shape = shape }

type prepared_leaf = {
  receipt : Frontend.Parser.initializer_leaf_preparation;
  kind : prepared_kind;
  rest : continuation list;
}

type live = {
  declaration : Sema.Compiler_record.declared_global;
  shape : Shape.t;
  work : progress;
  entries_rev : entry list;
  last_delimiter : Frontend.Parser.completed_initializer_delimiter option;
  delimiters_rev : Frontend.Parser.initializer_delimiter list;
  preparation : prepared_leaf option;
  phased : bool;
}

type static_snapshot = {
  static_allocation : Sema.Compiler_record.automatic_aggregate_allocation;
  static_table : Sema.Symbol_table.t;
  static_parent : Sema.Symbol_table.scope;
  static_shape : Shape.t;
  mutable static_cursor : stream;
  mutable static_delimiters : Frontend.Parser.initializer_delimiter list;
  mutable static_last_delimiter :
    Frontend.Parser.completed_static_initializer_delimiter option;
  mutable static_preparing :
    (Frontend.Parser.static_leaf_preparation * stream_preparation) option;
  mutable static_reached :
    (Frontend.Parser.static_initializer_preparation
    * int
    * int
    * operation
    * Sema.Initializer_type.scalar option)
    list;
  mutable static_completed : Frontend.Ast.local_declarator option;
}

let create_static_retained ~allocation ~shape ~cursor ~delimiters source reached
    =
  create_static_receipt
    ~allocation:(Sema.Compiler_record.static_allocation_receipt allocation)
    ~shape ~cursor ~delimiters source reached

let begin_static_snapshot ~table ~parent ~allocation ~dimensions =
  let receipt = Sema.Compiler_record.automatic_aggregate_source allocation in
  if
    (not (Frontend.Parser.function_local_allocation_is_current receipt))
    || receipt.allocation_storage <> Frontend.Ast.Static_local
    || not
         (Sema.Compiler_record.automatic_aggregate_owns allocation ~table
            ~parent)
  then invalid "static metadata requires its original allocation callback"
  else
    let* shape =
      Shape.create_initializer_aggregate ~allocation ~dimensions ()
      |> Result.map_error (fun _ ->
          "HCRUN0006: static metadata has no supported original shape")
    in
    Ok
      {
        static_allocation = allocation;
        static_table = table;
        static_parent = parent;
        static_shape = shape;
        static_cursor = begin_stream shape;
        static_delimiters = [];
        static_last_delimiter = None;
        static_preparing = None;
        static_reached = [];
        static_completed = None;
      }

let static_snapshot_receipt value =
  Sema.Compiler_record.automatic_aggregate_source value.static_allocation

let observe_static_snapshot_delimiter value receipt =
  let prior =
    List.nth_opt value.static_reached 0 |> Option.map (fun (r, _, _, _, _) -> r)
  in
  if
    Option.is_some value.static_completed
    || Option.is_some value.static_preparing
    || (not (Frontend.Parser.static_initializer_delimiter_is_current receipt))
    || receipt.Frontend.Parser.static_delimiter_start.static_start_allocation
       != static_snapshot_receipt value
    || (not
          (Option.equal ( == ) value.static_last_delimiter
             receipt.static_delimiter_predecessor))
    || (not
          (Option.equal ( == ) prior receipt.static_delimiter_leaf_predecessor))
    || receipt.static_delimiter_index
       <> Option.fold ~none:0
            ~some:(fun r -> r.Frontend.Parser.static_delimiter_index + 1)
            value.static_last_delimiter
  then invalid "static metadata delimiter is foreign, repeated or out of order"
  else
    let* cursor =
      observe_stream_delimiter value.static_cursor
        receipt.static_delimiter_value
    in
    value.static_cursor <- cursor;
    value.static_last_delimiter <- Some receipt;
    value.static_delimiters <-
      receipt.static_delimiter_value :: value.static_delimiters;
    Ok ()

let prepare_static_snapshot_expression value receipt =
  if
    Option.is_some value.static_completed
    || Option.is_some value.static_preparing
    || (not (Frontend.Parser.static_leaf_preparation_is_current receipt))
    || receipt.Frontend.Parser.static_expression_start.static_start_allocation
       != static_snapshot_receipt value
    || not
         (Option.equal ( == ) value.static_last_delimiter
            receipt.static_expression_delimiter_predecessor)
  then invalid "static metadata expression is foreign, repeated or out of order"
  else
    let* selected =
      prepare_stream_expression value.static_cursor
        ~first:receipt.static_expression_first ~delimiters:[]
    in
    value.static_preparing <- Some (receipt, selected);
    Ok ()

let prepare_static_snapshot_leaf value receipt =
  let previous =
    List.nth_opt value.static_reached 0 |> Option.map (fun (r, _, _, _, _) -> r)
  in
  if
    Option.is_some value.static_completed
    || (not (Frontend.Parser.static_initializer_is_current receipt))
    || receipt.Frontend.Parser.static_allocation
       != static_snapshot_receipt value
    || receipt.static_leaf_index <> List.length value.static_reached
    || not (Option.equal ( == ) previous receipt.static_leaf_predecessor)
  then invalid "static metadata leaf is foreign, repeated or out of order"
  else
    match value.static_preparing with
    | Some (original, selected)
      when original == receipt.static_expression_preparation ->
        let* _ =
          prepare_stream_leaf selected ~value:receipt.static_leaf_value
        in
        let* () =
          if stream_preparation_requires_settlement selected then
            settle_stream_preparation selected
          else Ok ()
        in
        let* cursor, (cell, byte, operation, scalar) =
          stream_preparation_result selected
        in
        value.static_cursor <- cursor;
        value.static_reached <-
          (receipt, cell, byte, operation, scalar) :: value.static_reached;
        value.static_preparing <- None;
        Ok ()
    | _ ->
        invalid "static metadata leaf lacks its original expression preparation"

let complete_static_snapshot value receipt =
  let previous =
    List.nth_opt value.static_reached 0 |> Option.map (fun (r, _, _, _, _) -> r)
  in
  if
    Option.is_some value.static_completed
    || Option.is_some value.static_preparing
    || (not (Frontend.Parser.static_initializer_completion_is_current receipt))
    || receipt.Frontend.Parser.static_completed_start.static_start_allocation
       != static_snapshot_receipt value
    || not (Option.equal ( == ) previous receipt.static_preparation)
  then invalid "static metadata completion is foreign, repeated or out of order"
  else (
    value.static_completed <- Some receipt.static_declarator;
    Ok ())

let static_snapshot_leaf_destination value receipt =
  if
    (not (Frontend.Parser.static_initializer_is_current receipt))
    || receipt.Frontend.Parser.static_allocation
       != static_snapshot_receipt value
  then invalid "static cursor read requires its original current leaf"
  else
    match value.static_reached with
    | (original, cell, byte, operation, scalar) :: _ when original == receipt ->
        Ok (cell, byte, operation, scalar)
    | _ -> invalid "static cursor read has no original reached leaf"

let static_snapshot_matches value ~frame ~location =
  let module Frame = Sema.Function_frame_layout in
  match
    ( Sema.Symbol_table.parent (Frame.function_scope frame),
      value.static_completed )
  with
  | Some parent, Some local
    when parent == value.static_parent
         && Frame.location_kind location = Frame.Static_local
         && Option.fold ~none:false ~some:(( == ) location)
              (Frame.find_location frame (Frame.location_symbol location))
         && local.Frontend.Ast.local_name
            == Sema.Compiler_record.automatic_aggregate_local_name
                 value.static_allocation ->
      Result.is_ok
        (Sema.Compiler_record.validate_automatic_aggregate
           ~table:value.static_table ~parent
           ~function_symbol:(Frame.function_symbol frame)
           ~local_symbol:(Frame.location_symbol location)
           ~local_name:local.local_name
           ~checked_type:(Frame.location_checked_type location)
           value.static_allocation)
  | _ -> false

let static_snapshot_layout value ~frame ~location ~shape ~source =
  if
    (not (static_snapshot_matches value ~frame ~location))
    || Shape.byte_size shape <> Shape.byte_size value.static_shape
    || Shape.dimensions shape <> Shape.dimensions value.static_shape
    || not
         (Option.fold ~none:false
            ~some:(fun local ->
              Option.fold ~none:false
                ~some:(fun initial ->
                  initial.Frontend.Ast.local_initializer_value
                  == Source.source_ast source)
                local.Frontend.Ast.local_initializer)
            value.static_completed)
  then invalid "static metadata layout has another frame, source or extent"
  else
    create_static_receipt
      ~allocation:(static_snapshot_receipt value)
      ~shape ~cursor:value.static_cursor
      ~delimiters:(List.rev value.static_delimiters)
      source
      (List.rev value.static_reached)

let begin_live declaration =
  let shape =
    if
      Option.is_some
        (Sema.Compiler_record.declared_global_source declaration)
          .global_function_pointer
    then
      let type_ =
        Sema.Type.make_primitive ~form:Sema.Type.Public_spelling
          ~primitive:Sema.Primitive_type.I64 ~pointer_depth:0
        |> Result.get_ok
      in
      Shape.create ~type_
        ~dimensions:
          (Sema.Compiler_record.declared_global_dimensions declaration)
    else Shape.create_declared_global declaration
  in
  match shape with
  | Error _ -> invalid "has no checked supported live storage shape"
  | Ok shape ->
      Ok
        {
          declaration;
          shape;
          work = initial_progress shape;
          entries_rev = [];
          last_delimiter = None;
          delimiters_rev = [];
          preparation = None;
          phased = false;
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

let prepare_live_expression live
    (receipt : Frontend.Parser.initializer_leaf_preparation) =
  let expected_index = List.length live.entries_rev in
  if
    Option.is_some live.preparation
    || (match live.entries_rev with
      | previous :: _ -> requires_settlement previous
      | [] -> false)
    || receipt.preparation_initializer.initializer_owner
       != Sema.Compiler_record.declared_global_source live.declaration
    || receipt.preparation_index <> expected_index
    || (not
          (same_optional_identity (previous_leaf live)
             receipt.preparation_predecessor))
    || (not
          (same_optional_identity live.last_delimiter
             receipt.preparation_delimiter_predecessor))
    || List.length live.delimiters_rev
       <> List.length receipt.preparation_delimiters
    || not
         (List.for_all2 ( == )
            (List.rev live.delimiters_rev)
            receipt.preparation_delimiters)
  then invalid "expression preparation is foreign, repeated or out of order"
  else
    let* kind, rest =
      select_prepared_kind ~first:receipt.preparation_first live.work.pending
    in
    Ok { live with preparation = Some { receipt; kind; rest }; phased = true }

let prepared_entry live preparation leaf =
  let* operation, scalar_, bytes, evaluation_ =
    match preparation.kind with
    | Prepared_scalar phase ->
        let scalar = Initializer_type.scalar_evaluation_before phase in
        Ok
          ( Scalar_store,
            (if Shape.is_aggregate live.shape then Some scalar else None),
            Initializer_type.scalar_byte_size scalar,
            if Initializer_type.scalar_evaluation_may_change phase then
              Some phase
            else None )
    | Prepared_string count -> (
        match Source.leaf_expression_ast leaf with
        | Frontend.Ast.String_literal
            { literal_value = Frontend.Ast.Bytes_value bytes; _ }
          when count <= String.length bytes + 1 ->
            let copied =
              if count <= String.length bytes then String.sub bytes 0 count
              else bytes ^ "\000"
            in
            Ok (Copy_bytes copied, None, count, None)
        | _ -> invalid "prepared string copy lost its source-owned bytes")
  in
  let offset = live.work.next_byte in
  if
    offset < 0
    || offset > Shape.byte_size live.shape
    || bytes < 0
    || bytes > Shape.byte_size live.shape - offset
  then invalid "prepared scalar destination exceeds its declared object"
  else
    let entry =
      {
        leaf;
        cell_offset =
          offset / Integer_scalar_storage.byte_size (Shape.scalar live.shape);
        byte_offset = offset;
        operation;
        scalar_;
        declared_owner_ = Some live.declaration;
        evaluation_;
        settled_ = Option.is_none evaluation_;
      }
    in
    Ok ({ pending = preparation.rest; next_byte = offset + bytes }, entry)

let settle_live_entry live entry =
  match (live.entries_rev, entry.evaluation_) with
  | original :: rest, Some phase when original == entry && not entry.settled_ ->
      let* scalar = Initializer_type.finish_scalar_evaluation phase in
      let bytes = Initializer_type.scalar_byte_size scalar in
      if
        entry.byte_offset < 0
        || entry.byte_offset > Shape.byte_size live.shape
        || bytes < 0
        || bytes > Shape.byte_size live.shape - entry.byte_offset
      then
        invalid
          "reached scalar destination exceeds its original declared object"
      else
        let settled = { entry with scalar_ = Some scalar; settled_ = true } in
        Ok
          ( {
              live with
              entries_rev = settled :: rest;
              work = { live.work with next_byte = entry.byte_offset + bytes };
            },
            settled )
  | _ ->
      invalid
        "scalar settlement is foreign, repeated or has no original evaluation"

let observe_live_delimiter live
    (receipt : Frontend.Parser.completed_initializer_delimiter) =
  let expected_index =
    Option.fold ~none:0
      ~some:(fun previous -> previous.Frontend.Parser.delimiter_index + 1)
      live.last_delimiter
  in
  if
    Option.is_some live.preparation
    || (match live.entries_rev with
      | previous :: _ -> requires_settlement previous
      | [] -> false)
    || receipt.delimiter_initializer.initializer_owner
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
          match live.preparation with
          | None ->
              advance ~shape:live.shape ~final:false live.work
                [ Expression leaf ]
          | Some preparation
            when preparation.receipt == receipt.leaf_preparation ->
              let* work, entry = prepared_entry live preparation leaf in
              Ok (work, [ entry ])
          | Some _ ->
              invalid
                "completed leaf replaced its original expression preparation"
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
                  preparation = None;
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
  let expected = List.rev live.entries_rev in
  let* layout =
    if live.phased then
      if
        Option.is_some live.preparation
        || List.exists requires_settlement expected
        || List.length expected <> List.length (Source.leaves source)
        || not
             (List.for_all2
                (fun entry leaf -> entry.leaf == leaf)
                expected (Source.leaves source))
      then invalid "completion has unsettled or substituted original leaves"
      else
        Ok
          {
            source;
            entries = expected;
            layout_declaration = Some live.declaration;
            layout_shape = live.shape;
          }
    else create ~shape:live.shape source
  in
  if
    List.length expected <> List.length layout.entries
    || not
         (List.for_all2
            (fun left right ->
              left.leaf == right.leaf
              && left.cell_offset = right.cell_offset
              && left.byte_offset = right.byte_offset
              && left.operation = right.operation
              && Option.equal Initializer_type.scalar_equal left.scalar_
                   right.scalar_)
            expected layout.entries)
  then invalid "completion differs from its original live destinations"
  else Ok { layout with layout_declaration = Some live.declaration }
