type termination =
  | Physical_eof
  | Nul_terminated of { terminator_offset : int; trailing_bytes : int }

type t = {
  source : Common.Source_file.t;
  contents : string;
  mode : Token.mode;
  generated_from : Common.Span.t option;
  defined_at : Common.Span.t option;
  nul_terminates : bool;
  recover_normalized_doldoc : bool;
  binary_table : (Doldoc_binary.t, Doldoc_binary.error) result Lazy.t option;
  caller : t option;
  mutable offset : int;
  mutable emitted_eof : bool;
  mutable termination : termination option;
}

type cursor = { positions : (t * int) list }
type consumed_range = { owner : t; start : int; stop : int }
type located_byte = { owner : t; offset : int; byte : char }
type item = Token of Token.t | Diagnostic of Common.Diagnostic.t
type definition_terminator = End_of_line | End_of_file | Nul

type definition_replacement = {
  replacement : string;
  replacement_span : Common.Span.t;
  segments : Definition.segment list;
  terminator : definition_terminator;
}

let create ?(mode = Token.Raw) ?generated_from ?defined_at ?caller
    ?(nul_terminates = false) ?(recover_normalized_doldoc = false) source =
  let contents = Common.Source_file.contents source in
  {
    source;
    contents;
    mode;
    generated_from;
    defined_at;
    nul_terminates;
    recover_normalized_doldoc;
    binary_table =
      (if nul_terminates then
         Some
           (lazy
             (Doldoc_binary.decode ~recover_normalized:recover_normalized_doldoc
                contents))
       else None);
    caller;
    offset = 0;
    emitted_eof = false;
    termination = None;
  }

let source_id (lexer : t) = Common.Source_file.id lexer.source
let offset (lexer : t) = lexer.offset
let termination (lexer : t) = lexer.termination
let source_length (lexer : t) = String.length lexer.contents
let local_at_end (lexer : t) = lexer.offset >= source_length lexer

let rec at_end (lexer : t) =
  if not (local_at_end lexer) then false
  else Option.fold ~none:true ~some:at_end lexer.caller

let rec terminal_lexer (lexer : t) =
  match lexer.caller with
  | None -> lexer
  | Some caller -> terminal_lexer caller

let rec owner_at_distance (lexer : t) distance =
  let remaining = source_length lexer - lexer.offset in
  if distance < remaining then Some (lexer, lexer.offset + distance)
  else
    match lexer.caller with
    | None -> None
    | Some caller -> owner_at_distance caller (distance - remaining)

let peek (lexer : t) distance =
  Option.map
    (fun (owner, index) -> owner.contents.[index])
    (owner_at_distance lexer distance)

let advance_located (lexer : t) =
  match owner_at_distance lexer 0 with
  | None -> None
  | Some (owner, offset) ->
      let byte = owner.contents.[offset] in
      owner.offset <- owner.offset + 1;
      Some { owner; offset; byte }

let advance (lexer : t) =
  Option.map (fun located -> located.byte) (advance_located lexer)

let rec advance_count (lexer : t) count =
  if count <= 0 then ()
  else
    match advance lexer with
    | None -> ()
    | Some _ -> advance_count lexer (count - 1)

let cursor (lexer : t) =
  let rec collect (current : t) =
    (current, current.offset)
    :: Option.fold ~none:[] ~some:collect current.caller
  in
  { positions = collect lexer }

let ranges_between start_cursor stop_cursor =
  let rec collect starts stops ranges =
    match (starts, stops) with
    | [], [] -> List.rev ranges
    | (owner, start) :: start_rest, (stop_owner, stop) :: stop_rest ->
        if owner != stop_owner then
          invalid_arg "lexer cursors belong to different frame chains";
        let ranges =
          if stop > start then { owner; start; stop } :: ranges else ranges
        in
        collect start_rest stop_rest ranges
    | _ -> invalid_arg "lexer cursors have different frame depths"
  in
  collect start_cursor.positions stop_cursor.positions []

let spans_of_ranges ranges =
  List.map
    (fun (range : consumed_range) ->
      Common.Span.unsafe_make ~source:(source_id range.owner) ~start:range.start
        ~stop:range.stop)
    ranges

let point_owner cursor =
  let rec find = function
    | [] -> invalid_arg "empty lexer cursor"
    | [ (owner, offset) ] -> (owner, offset)
    | (owner, offset) :: rest ->
        if offset < source_length owner then (owner, offset) else find rest
  in
  find cursor.positions

let span_between start_cursor stop_cursor =
  match ranges_between start_cursor stop_cursor |> spans_of_ranges with
  | span :: _ -> span
  | [] ->
      let owner, offset = point_owner start_cursor in
      Common.Span.unsafe_make ~source:(source_id owner) ~start:offset
        ~stop:offset

let source_segments_between start_cursor stop_cursor =
  match ranges_between start_cursor stop_cursor |> spans_of_ranges with
  | [] -> [ span_between start_cursor stop_cursor ]
  | spans -> spans

let raw_between start_cursor stop_cursor =
  let ranges = ranges_between start_cursor stop_cursor in
  let size =
    List.fold_left
      (fun total (range : consumed_range) -> total + range.stop - range.start)
      0 ranges
  in
  let buffer = Buffer.create size in
  List.iter
    (fun (range : consumed_range) ->
      Buffer.add_substring buffer range.owner.contents range.start
        (range.stop - range.start))
    ranges;
  Buffer.contents buffer

let local_span owner start stop =
  Common.Span.unsafe_make ~source:(source_id owner) ~start ~stop

let consume_continuation_marker lexer =
  match peek lexer 0 with
  | Some '\\' ->
      let start = cursor lexer in
      ignore (advance lexer);
      Some (span_between start (cursor lexer))
  | _ -> None

let is_non_eol_whitespace = function
  | ' ' | '\t' | '\x1f' -> true
  | _ -> false

let capture_definition_replacement lexer =
  while Option.fold ~none:false ~some:is_non_eol_whitespace (peek lexer 0) do
    ignore (advance lexer)
  done;
  let replacement_start = cursor lexer in
  let buffer = Buffer.create 64 in
  let segments_rev = ref [] in
  let append_source_byte located =
    let generated_start = Buffer.length buffer in
    Buffer.add_char buffer located.byte;
    match !segments_rev with
    | ({ Definition.generated_stop; source_span; _ } as previous) :: rest
      when generated_stop = generated_start
           && Common.Source_id.equal source_span.Common.Span.source
                (source_id located.owner)
           && source_span.Common.Span.stop = located.offset ->
        let source_span =
          Common.Span.unsafe_make ~source:source_span.source
            ~start:source_span.start ~stop:(located.offset + 1)
        in
        segments_rev :=
          { previous with generated_stop = generated_start + 1; source_span }
          :: rest
    | _ ->
        let source_span =
          local_span located.owner located.offset (located.offset + 1)
        in
        segments_rev :=
          {
            Definition.generated_start;
            generated_stop = generated_start + 1;
            source_span;
          }
          :: !segments_rev
  in
  let rec finish_comment () =
    match peek lexer 0 with
    | None -> End_of_file
    | Some '\x00' ->
        ignore (advance lexer);
        Nul
    | Some ('\r' | '\n') ->
        ignore (advance lexer);
        End_of_line
    | Some _ ->
        ignore (advance lexer);
        finish_comment ()
  in
  let rec scan ~initial ~in_string =
    match peek lexer 0 with
    | None -> (End_of_file, cursor lexer)
    | Some '\x00' ->
        let raw_stop = cursor lexer in
        ignore (advance lexer);
        (Nul, raw_stop)
    | Some ('\r' | '\n') ->
        let raw_stop = cursor lexer in
        ignore (advance lexer);
        (End_of_line, raw_stop)
    | Some '\\' -> (
        let backslash = Option.get (advance_located lexer) in
        match peek lexer 0 with
        | None ->
            append_source_byte backslash;
            (End_of_file, cursor lexer)
        | Some '\n' ->
            ignore (advance lexer);
            scan ~initial:false ~in_string
        | Some '\r' -> (
            let raw_stop = cursor lexer in
            ignore (advance lexer);
            match peek lexer 0 with
            | Some '\n' ->
                ignore (advance lexer);
                scan ~initial:false ~in_string
            | _ -> (End_of_line, raw_stop))
        | Some _ ->
            append_source_byte backslash;
            append_source_byte (Option.get (advance_located lexer));
            scan ~initial:false ~in_string)
    | Some '/' when (not initial) && not in_string -> (
        match peek lexer 1 with
        | Some '/' ->
            let raw_stop = cursor lexer in
            advance_count lexer 2;
            (finish_comment (), raw_stop)
        | _ ->
            append_source_byte (Option.get (advance_located lexer));
            scan ~initial:false ~in_string)
    | Some '"' ->
        append_source_byte (Option.get (advance_located lexer));
        scan ~initial:false ~in_string:(not in_string)
    | Some _ ->
        append_source_byte (Option.get (advance_located lexer));
        scan ~initial:false ~in_string
  in
  let terminator, replacement_stop = scan ~initial:true ~in_string:false in
  {
    replacement = Buffer.contents buffer;
    replacement_span = span_between replacement_start replacement_stop;
    segments = List.rev !segments_rev;
    terminator;
  }

let make_diagnostic lexer ?help ~code ~message ~start () =
  let stop = cursor lexer in
  let source_segments = source_segments_between start stop in
  let primary = List.hd source_segments in
  let secondary =
    List.tl source_segments
    |> List.map (fun span ->
        {
          Common.Diagnostic.span;
          message = "the same lexical item continues here";
        })
  in
  Common.Diagnostic.make ?help ~secondary ~code
    ~severity:Common.Diagnostic.Error ~message ~primary ()

let binary_table_diagnostic lexer (error : Doldoc_binary.error) =
  let stop = min (String.length lexer.contents) (error.offset + 1) in
  let primary = local_span lexer error.offset stop in
  Common.Diagnostic.make ~code:"HCLEX0008" ~severity:Common.Diagnostic.Error
    ~message:error.message ~primary
    ~help:
      "Read the file as binary data and check the CDocBin headers and payload \
       lengths after its text terminator."
    ()

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' | '\x1f' -> true
  | _ -> false

let is_ascii_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

let is_identifier_start byte =
  is_ascii_letter byte || Char.equal byte '_' || Char.equal byte '@'
  || Char.code byte >= 128

let is_identifier_continue byte =
  is_identifier_start byte
  ||
  match byte with
  | '0' .. '9' -> true
  | _ -> false

let trivia lexer kind start =
  let stop = cursor lexer in
  let source_segments = source_segments_between start stop in
  {
    Trivia.kind;
    raw = raw_between start stop;
    span = List.hd source_segments;
    source_segments;
  }

type inserted_command_kind = Insert_binary | Insert_binary_size

type inserted_command = {
  command_kind : inserted_command_kind;
  record_number : int64;
  stop_offset : int;
}

type inserted_command_scan =
  | Not_inserted_command
  | Inserted_command of inserted_command
  | Invalid_inserted_command of { stop_offset : int; message : string }

let local_has_prefix owner offset prefix =
  let prefix_length = String.length prefix in
  offset + prefix_length <= String.length owner.contents
  && String.equal (String.sub owner.contents offset prefix_length) prefix

let split_command_fields contents start stop =
  let rec loop index field_start in_string fields =
    if index = stop then
      List.rev (String.sub contents field_start (stop - field_start) :: fields)
    else
      match contents.[index] with
      | '"' -> loop (index + 1) field_start (not in_string) fields
      | ',' when not in_string ->
          let field = String.sub contents field_start (index - field_start) in
          loop (index + 1) (index + 1) in_string (field :: fields)
      | _ -> loop (index + 1) field_start in_string fields
  in
  loop start start false []

let uint32_decimal text =
  let length = String.length text in
  let rec loop index value =
    if index = length then Some value
    else
      match text.[index] with
      | '0' .. '9' as digit ->
          let digit = Int64.of_int (Char.code digit - Char.code '0') in
          if value > 429496729L then None
          else
            let value = Int64.add (Int64.mul value 10L) digit in
            if value > 0xffff_ffffL then None else loop (index + 1) value
      | _ -> None
  in
  if length = 0 then None else loop 0 0L

let classify_inserted_command lexer =
  match owner_at_distance lexer 0 with
  | None -> Not_inserted_command
  | Some (owner, start_offset) -> (
      if not owner.nul_terminates then Not_inserted_command
      else
        let command_kind, command_prefix =
          if local_has_prefix owner start_offset "$IB," then
            (Some Insert_binary, "$IB,")
          else if local_has_prefix owner start_offset "$IS," then
            (Some Insert_binary_size, "$IS,")
          else (None, "")
        in
        match command_kind with
        | None -> Not_inserted_command
        | Some command_kind -> (
            let command_start = start_offset + 1 in
            let rec find_stop offset in_string =
              if offset >= String.length owner.contents then None
              else
                match owner.contents.[offset] with
                | '"' -> find_stop (offset + 1) (not in_string)
                | '$' when not in_string -> Some offset
                | _ -> find_stop (offset + 1) in_string
            in
            match
              find_stop (start_offset + String.length command_prefix) false
            with
            | None -> Not_inserted_command
            | Some closing_offset -> (
                let fields =
                  split_command_fields owner.contents command_start
                    closing_offset
                  |> List.map String.trim
                in
                if
                  List.exists
                    (fun field -> String.starts_with ~prefix:"BP=" field)
                    fields
                then Not_inserted_command
                else
                  let binary_fields =
                    List.filter
                      (fun field -> String.starts_with ~prefix:"BI=" field)
                      fields
                  in
                  match binary_fields with
                  | [] -> Not_inserted_command
                  | [ field ] -> (
                      let spelling =
                        String.sub field 3 (String.length field - 3)
                      in
                      let stop_offset = closing_offset + 1 in
                      match uint32_decimal spelling with
                      | Some record_number ->
                          Inserted_command
                            { command_kind; record_number; stop_offset }
                      | None ->
                          Invalid_inserted_command
                            {
                              stop_offset;
                              message =
                                "this hosted frontend requires BI= to contain \
                                 one unsigned 32-bit decimal record number";
                            })
                  | _ ->
                      Invalid_inserted_command
                        {
                          stop_offset = closing_offset + 1;
                          message =
                            "a DolDoc inserted-binary command contains more \
                             than one BI= field";
                        })))

let rec skip_trivia lexer accumulated =
  match (peek lexer 0, peek lexer 1) with
  | Some '\\', Some (('\n' | '\r') as newline) ->
      let start = cursor lexer in
      advance_count lexer 2;
      if
        Char.equal newline '\r'
        && Option.fold ~none:false ~some:(Char.equal '\n') (peek lexer 0)
      then ignore (advance lexer);
      skip_trivia lexer (trivia lexer Line_continuation start :: accumulated)
  | Some byte, _ when is_whitespace byte ->
      let start = cursor lexer in
      while Option.fold ~none:false ~some:is_whitespace (peek lexer 0) do
        ignore (advance lexer)
      done;
      skip_trivia lexer (trivia lexer Whitespace start :: accumulated)
  | Some '/', Some '/' ->
      let start = cursor lexer in
      advance_count lexer 2;
      while
        Option.fold ~none:false
          ~some:(fun byte -> not (Char.equal byte '\n'))
          (peek lexer 0)
      do
        ignore (advance lexer)
      done;
      skip_trivia lexer (trivia lexer Line_comment start :: accumulated)
  | Some '/', Some '*' ->
      let start = cursor lexer in
      advance_count lexer 2;
      let depth = ref 1 in
      while !depth > 0 && not (at_end lexer) do
        match (peek lexer 0, peek lexer 1) with
        | Some '/', Some '*' ->
            advance_count lexer 2;
            incr depth
        | Some '*', Some '/' ->
            advance_count lexer 2;
            decr depth
        | _ -> ignore (advance lexer)
      done;
      if !depth = 0 then
        skip_trivia lexer (trivia lexer Block_comment start :: accumulated)
      else
        let diagnostic =
          make_diagnostic lexer ~code:"HCLEX0002"
            ~message:"unterminated block comment" ~start ()
        in
        (List.rev accumulated, Some diagnostic)
  | Some '$', Some next when not (Char.equal next '$') -> (
      match classify_inserted_command lexer with
      | Inserted_command _ | Invalid_inserted_command _ ->
          (List.rev accumulated, None)
      | Not_inserted_command ->
          let start = cursor lexer in
          ignore (advance lexer);
          while
            Option.fold ~none:false
              ~some:(fun byte -> not (Char.equal byte '$'))
              (peek lexer 0)
          do
            ignore (advance lexer)
          done;
          if at_end lexer then
            let diagnostic =
              make_diagnostic lexer ~code:"HCLEX0007"
                ~message:"unterminated dollar comment" ~start ()
            in
            (List.rev accumulated, Some diagnostic)
          else (
            ignore (advance lexer);
            skip_trivia lexer (trivia lexer Dollar_comment start :: accumulated))
      )
  | _ -> (List.rev accumulated, None)

let make_token ?binary_record lexer leading_trivia ~kind ~value start =
  let stop = cursor lexer in
  let source_segments = source_segments_between start stop in
  let owner, _ = point_owner start in
  {
    Token.kind;
    raw = raw_between start stop;
    value;
    binary_record;
    span = List.hd source_segments;
    source_segments;
    origin =
      {
        frame = source_id owner;
        generated_from = owner.generated_from;
        defined_at = owner.defined_at;
      };
    leading_trivia;
    mode = owner.mode;
  }

let consume_to_local_offset lexer (owner : t) stop_offset =
  while owner.offset < stop_offset do
    ignore (advance lexer)
  done

let scan_inserted_command lexer leading_trivia command_scan =
  let start = cursor lexer in
  let owner, _ = Option.get (owner_at_distance lexer 0) in
  match command_scan with
  | Not_inserted_command ->
      invalid_arg "scan_inserted_command requires a recognized command"
  | Invalid_inserted_command { stop_offset; message } ->
      consume_to_local_offset lexer owner stop_offset;
      Diagnostic
        (make_diagnostic lexer ~code:"HCLEX0009" ~message
           ~help:
             "Use one BI=<decimal record number> field, or leave the command \
              as ordinary DolDoc text."
           ~start ())
  | Inserted_command { command_kind; record_number; stop_offset } -> (
      consume_to_local_offset lexer owner stop_offset;
      match Option.map Lazy.force owner.binary_table with
      | Some (Error error) -> Diagnostic (binary_table_diagnostic owner error)
      | Some (Ok table) -> (
          match Doldoc_binary.find table record_number with
          | None when not owner.recover_normalized_doldoc ->
              Diagnostic
                (make_diagnostic lexer ~code:"HCLEX0010"
                   ~message:
                     (Printf.sprintf
                        "DolDoc binary record %Ld is referenced here but is \
                         not present after the text terminator"
                        record_number)
                   ~help:
                     "Restore the matching CDocBin record or use the canonical \
                      binary Git object instead of a newline-converted \
                      worktree copy."
                   ~start ())
          | None ->
              let kind, value =
                match command_kind with
                | Insert_binary -> (Token_kind.Inserted_binary, Token.Bytes "")
                | Insert_binary_size ->
                    (Token_kind.Inserted_binary_size, Token.Int64 0L)
              in
              Token
                (make_token
                   ~binary_record:
                     {
                       Token.number = record_number;
                       declared_size = 0L;
                       payload_complete = false;
                     }
                   lexer leading_trivia ~kind ~value start)
          | Some record ->
              let kind, value =
                match command_kind with
                | Insert_binary ->
                    (Token_kind.Inserted_binary, Token.Bytes record.payload)
                | Insert_binary_size ->
                    ( Token_kind.Inserted_binary_size,
                      Token.Int64 record.declared_size )
              in
              Token
                (make_token
                   ~binary_record:
                     {
                       Token.number = record_number;
                       declared_size = record.declared_size;
                       payload_complete = record.payload_complete;
                     }
                   lexer leading_trivia ~kind ~value start))
      | None ->
          invalid_arg
            "an inserted-binary command was recognized without DolDoc decoding")

let scan_to_directive_marker lexer =
  let rec scan () =
    match peek lexer 0 with
    | None -> Ok None
    | Some '\x00' ->
        let start = cursor lexer in
        ignore (advance lexer);
        Error
          (make_diagnostic lexer ~code:"HCLEX0006"
             ~message:"embedded NUL byte in source" ~start ())
    | Some '#' ->
        let start = cursor lexer in
        ignore (advance lexer);
        Ok
          (Some
             (make_token lexer [] ~kind:(Token_kind.Punctuation '#')
                ~value:Token.No_value start))
    | Some _ ->
        ignore (advance lexer);
        scan ()
  in
  scan ()

let hex_digit byte =
  match Char.uppercase_ascii byte with
  | '0' .. '9' as digit -> Some (Char.code digit - Char.code '0')
  | 'A' .. 'F' as digit -> Some (Char.code digit - Char.code 'A' + 10)
  | _ -> None

let decimal_digit = function
  | '0' .. '9' as digit -> Some (Char.code digit - Char.code '0')
  | _ -> None

let binary_digit = function
  | '0' -> Some 0
  | '1' -> Some 1
  | _ -> None

let consume_digits lexer ~digit ~base initial =
  let value = ref initial in
  let count = ref 0 in
  while
    Option.fold ~none:false
      ~some:(fun byte -> Option.is_some (digit byte))
      (peek lexer 0)
  do
    let byte = Option.get (advance lexer) in
    let item = Option.get (digit byte) in
    value := Common.Int64_ops.mul_add ~base !value ~digit:item;
    incr count
  done;
  (!value, !count)

let scan_number lexer leading_trivia =
  let start = cursor lexer in
  match peek lexer 0 with
  | Some '.' ->
      ignore (advance lexer);
      let value, fraction_digits =
        consume_digits lexer ~digit:decimal_digit ~base:10L 0L
      in
      let exponent = ref 0 in
      let negative_exponent = ref false in
      (match peek lexer 0 with
      | Some ('e' | 'E') ->
          ignore (advance lexer);
          (match peek lexer 0 with
          | Some '-' ->
              ignore (advance lexer);
              negative_exponent := true
          | _ -> ());
          let exponent_value, _ =
            consume_digits lexer ~digit:decimal_digit ~base:10L 0L
          in
          exponent := Int64.to_int exponent_value
      | _ -> ());
      let power =
        (if !negative_exponent then - !exponent else !exponent)
        - fraction_digits
      in
      let float_value = Int64.to_float value *. (10. ** float_of_int power) in
      make_token lexer leading_trivia ~kind:Token_kind.Float
        ~value:(Token.Float64 float_value) start
  | Some first -> (
      ignore (advance lexer);
      let initial = Int64.of_int (Option.get (decimal_digit first)) in
      let prefixed =
        match peek lexer 0 with
        | Some ('x' | 'X') -> Some (hex_digit, 16L)
        | Some ('b' | 'B') -> Some (binary_digit, 2L)
        | _ -> None
      in
      match prefixed with
      | Some (digit, base) ->
          ignore (advance lexer);
          let value, _ = consume_digits lexer ~digit ~base initial in
          make_token lexer leading_trivia ~kind:Token_kind.Integer
            ~value:(Token.Int64 value) start
      | None ->
          let integer, _ =
            consume_digits lexer ~digit:decimal_digit ~base:10L initial
          in
          let fraction_digits = ref 0 in
          let is_float = ref false in
          let value = ref integer in
          (match (peek lexer 0, peek lexer 1) with
          | Some '.', Some '.' -> ()
          | Some '.', _ ->
              is_float := true;
              ignore (advance lexer);
              let with_fraction, count =
                consume_digits lexer ~digit:decimal_digit ~base:10L !value
              in
              value := with_fraction;
              fraction_digits := count
          | _ -> ());
          let exponent = ref 0 in
          let negative_exponent = ref false in
          (match peek lexer 0 with
          | Some ('e' | 'E') ->
              is_float := true;
              ignore (advance lexer);
              (match peek lexer 0 with
              | Some '-' ->
                  ignore (advance lexer);
                  negative_exponent := true
              | _ -> ());
              let exponent_value, _ =
                consume_digits lexer ~digit:decimal_digit ~base:10L 0L
              in
              exponent := Int64.to_int exponent_value
          | _ -> ());
          if !is_float then
            let power =
              (if !negative_exponent then - !exponent else !exponent)
              - !fraction_digits
            in
            let float_value =
              Int64.to_float !value *. (10. ** float_of_int power)
            in
            make_token lexer leading_trivia ~kind:Token_kind.Float
              ~value:(Token.Float64 float_value) start
          else
            make_token lexer leading_trivia ~kind:Token_kind.Integer
              ~value:(Token.Int64 !value) start)
  | None -> assert false

let decoded_byte lexer =
  match peek lexer 0 with
  | None -> None
  | Some '\\' -> (
      ignore (advance lexer);
      match peek lexer 0 with
      | None -> Some (Char.code '\\')
      | Some byte -> (
          match byte with
          | '0' ->
              ignore (advance lexer);
              Some 0
          | '\'' | '`' | '"' | '\\' ->
              ignore (advance lexer);
              Some (Char.code byte)
          | 'd' ->
              ignore (advance lexer);
              Some (Char.code '$')
          | 'n' ->
              ignore (advance lexer);
              Some (Char.code '\n')
          | 'r' ->
              ignore (advance lexer);
              Some (Char.code '\r')
          | 't' ->
              ignore (advance lexer);
              Some (Char.code '\t')
          | 'x' | 'X' ->
              ignore (advance lexer);
              let value = ref 0 in
              let count = ref 0 in
              while
                !count < 2
                && Option.fold ~none:false
                     ~some:(fun item -> Option.is_some (hex_digit item))
                     (peek lexer 0)
              do
                let item =
                  Option.get (advance lexer) |> hex_digit |> Option.get
                in
                value := (!value lsl 4) lor item;
                incr count
              done;
              Some !value
          | _ -> Some (Char.code '\\')))
  | Some '$' ->
      ignore (advance lexer);
      (match peek lexer 0 with
      | Some '$' -> ignore (advance lexer)
      | _ -> ());
      Some (Char.code '$')
  | Some byte ->
      ignore (advance lexer);
      Some (Char.code byte)

let scan_string lexer leading_trivia =
  let start = cursor lexer in
  ignore (advance lexer);
  let decoded = Buffer.create 32 in
  let rec has_doldoc_command_end distance =
    match peek lexer distance with
    | None | Some '\x00' -> false
    | Some '$' -> true
    | Some _ -> has_doldoc_command_end (distance + 1)
  in
  let is_doldoc_command_name_byte = function
    | 'A' .. 'Z' | '0' .. '9' | '+' | '-' -> true
    | _ -> false
  in
  let rec has_doldoc_command_name distance saw_name_byte =
    match peek lexer distance with
    | Some byte when is_doldoc_command_name_byte byte ->
        has_doldoc_command_name (distance + 1) true
    | Some '$' -> saw_name_byte
    | Some ',' -> saw_name_byte && has_doldoc_command_end (distance + 1)
    | None | Some _ -> false
  in
  let rec loop () =
    match peek lexer 0 with
    | None ->
        Diagnostic
          (make_diagnostic lexer ~code:"HCLEX0003"
             ~message:"unterminated string literal" ~start ())
    | Some '\x00' ->
        let nul = cursor lexer in
        ignore (advance lexer);
        Diagnostic
          (make_diagnostic lexer ~code:"HCLEX0006"
             ~message:"embedded NUL byte in source" ~start:nul ())
    | Some '"' ->
        ignore (advance lexer);
        Token
          (make_token lexer leading_trivia ~kind:Token_kind.String
             ~value:(Token.Bytes (Buffer.contents decoded))
             start)
    | Some '$' when peek lexer 1 <> Some '$' && has_doldoc_command_name 1 false
      ->
        ignore (advance lexer);
        Buffer.add_char decoded '$';
        dollar_command ()
    | Some _ ->
        let byte = Option.get (decoded_byte lexer) in
        Buffer.add_char decoded (Char.chr byte);
        loop ()
  and dollar_command () =
    match peek lexer 0 with
    | None ->
        Diagnostic
          (make_diagnostic lexer ~code:"HCLEX0003"
             ~message:"unterminated string literal" ~start ())
    | Some '\x00' ->
        let nul = cursor lexer in
        ignore (advance lexer);
        Diagnostic
          (make_diagnostic lexer ~code:"HCLEX0006"
             ~message:"embedded NUL byte in source" ~start:nul ())
    | Some '$' ->
        ignore (advance lexer);
        Buffer.add_char decoded '$';
        loop ()
    | Some byte ->
        ignore (advance lexer);
        Buffer.add_char decoded byte;
        dollar_command ()
  in
  loop ()

let recover_character_literal lexer =
  while
    Option.fold ~none:false
      ~some:(fun byte -> not (Char.equal byte '\''))
      (peek lexer 0)
  do
    ignore (advance lexer)
  done;
  match peek lexer 0 with
  | Some '\'' -> ignore (advance lexer)
  | _ -> ()

let scan_character lexer leading_trivia =
  let start = cursor lexer in
  ignore (advance lexer);
  let value = ref 0L in
  let count = ref 0 in
  let rec loop () =
    match peek lexer 0 with
    | None ->
        Diagnostic
          (make_diagnostic lexer ~code:"HCLEX0004"
             ~message:"unterminated character literal" ~start ())
    | Some '\x00' ->
        let nul = cursor lexer in
        ignore (advance lexer);
        Diagnostic
          (make_diagnostic lexer ~code:"HCLEX0006"
             ~message:"embedded NUL byte in source" ~start:nul ())
    | Some '\'' ->
        ignore (advance lexer);
        Token
          (make_token lexer leading_trivia ~kind:Token_kind.Character
             ~value:(Token.Int64 !value) start)
    | Some _ when !count >= 8 ->
        recover_character_literal lexer;
        Diagnostic
          (make_diagnostic lexer ~code:"HCLEX0005"
             ~message:"character literal exceeds eight bytes" ~start ())
    | Some _ ->
        let byte = Option.get (decoded_byte lexer) in
        value :=
          Int64.logor !value (Int64.shift_left (Int64.of_int byte) (!count * 8));
        incr count;
        loop ()
  in
  loop ()

let scan_identifier lexer leading_trivia =
  let start = cursor lexer in
  ignore (advance lexer);
  while Option.fold ~none:false ~some:is_identifier_continue (peek lexer 0) do
    ignore (advance lexer)
  done;
  let text = raw_between start (cursor lexer) in
  match Keyword.find text with
  | Some keyword ->
      make_token lexer leading_trivia ~kind:(Token_kind.Keyword keyword)
        ~value:(Token.Text text) start
  | None ->
      make_token lexer leading_trivia ~kind:Token_kind.Identifier
        ~value:(Token.Text text) start

let punctuation = function
  | '!'
  | '%'
  | '&'
  | '('
  | ')'
  | '*'
  | '+'
  | ','
  | '-'
  | '/'
  | ':'
  | ';'
  | '<'
  | '='
  | '>'
  | '?'
  | '['
  | ']'
  | '^'
  | '{'
  | '|'
  | '}'
  | '~'
  | '`'
  | '#'
  | '.' -> true
  | _ -> false

let scan_operator_or_punctuation lexer leading_trivia =
  let start = cursor lexer in
  let has_prefix spelling =
    let rec matches index =
      if index = String.length spelling then true
      else
        match peek lexer index with
        | Some byte when Char.equal byte spelling.[index] -> matches (index + 1)
        | _ -> false
    in
    matches 0
  in
  match
    List.find_map
      (fun (spelling, operator) ->
        if has_prefix spelling then Some (operator, String.length spelling)
        else None)
      Operator.all
  with
  | Some (operator, width) ->
      advance_count lexer width;
      Token
        (make_token lexer leading_trivia ~kind:(Token_kind.Operator operator)
           ~value:Token.No_value start)
  | None ->
      let byte = Option.get (advance lexer) in
      Token
        (make_token lexer leading_trivia ~kind:(Token_kind.Punctuation byte)
           ~value:Token.No_value start)

let eof_token lexer leading_trivia =
  let start = cursor lexer in
  let terminal = terminal_lexer lexer in
  if Option.is_none terminal.termination then
    terminal.termination <- Some Physical_eof;
  terminal.emitted_eof <- true;
  make_token lexer leading_trivia ~kind:Token_kind.Eof ~value:Token.No_value
    start

let next lexer =
  if (terminal_lexer lexer).emitted_eof then Token (eof_token lexer [])
  else
    let leading_trivia, trivia_error = skip_trivia lexer [] in
    match trivia_error with
    | Some diagnostic -> Diagnostic diagnostic
    | None -> (
        match peek lexer 0 with
        | None -> Token (eof_token lexer leading_trivia)
        | Some '\x00' when lexer.nul_terminates ->
            let owner, terminator_offset =
              Option.get (owner_at_distance lexer 0)
            in
            let trailing_bytes =
              String.length owner.contents - terminator_offset - 1
            in
            ignore (advance lexer);
            owner.termination <-
              Some (Nul_terminated { terminator_offset; trailing_bytes });
            Token (eof_token lexer leading_trivia)
        | Some '\x00' ->
            let start = cursor lexer in
            ignore (advance lexer);
            Diagnostic
              (make_diagnostic lexer ~code:"HCLEX0006"
                 ~message:"embedded NUL byte in source" ~start ())
        | Some byte when is_identifier_start byte ->
            Token (scan_identifier lexer leading_trivia)
        | Some '0' .. '9' -> Token (scan_number lexer leading_trivia)
        | Some '.' -> (
            match peek lexer 1 with
            | Some '0' .. '9' -> Token (scan_number lexer leading_trivia)
            | _ -> scan_operator_or_punctuation lexer leading_trivia)
        | Some '"' -> scan_string lexer leading_trivia
        | Some '\'' -> scan_character lexer leading_trivia
        | Some '$' -> (
            match classify_inserted_command lexer with
            | (Inserted_command _ | Invalid_inserted_command _) as command ->
                scan_inserted_command lexer leading_trivia command
            | Not_inserted_command ->
                scan_operator_or_punctuation lexer leading_trivia)
        | Some byte when punctuation byte ->
            scan_operator_or_punctuation lexer leading_trivia
        | Some byte ->
            let start = cursor lexer in
            ignore (advance lexer);
            Diagnostic
              (make_diagnostic lexer ~code:"HCLEX0001"
                 ~message:
                   (Printf.sprintf "invalid source byte 0x%02x" (Char.code byte))
                 ~help:
                   "Remove the byte or place it inside a string or character \
                    literal."
                 ~start ()))

let lex_all ?mode ?nul_terminates ?recover_normalized_doldoc source =
  let lexer = create ?mode ?nul_terminates ?recover_normalized_doldoc source in
  let rec loop tokens diagnostics =
    match next lexer with
    | Token token when token.Token.kind = Token_kind.Eof ->
        let tokens = List.rev (token :: tokens) in
        if diagnostics = [] then Ok tokens else Error (List.rev diagnostics)
    | Token token -> loop (token :: tokens) diagnostics
    | Diagnostic diagnostic -> loop tokens (diagnostic :: diagnostics)
  in
  loop [] []
