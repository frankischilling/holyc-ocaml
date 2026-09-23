type work_budget = { limit : int; mutable count : int }
type byte_budget = { capacity : int; mutable committed : int }
type t = { output : Buffer.t; bytes : byte_budget; work_budget : work_budget }
type 'pointer argument = Word of int64 | Pointer of 'pointer

type 'error failure =
  | Memory of 'error
  | Output_limit
  | Work_limit
  | Offset_overflow
  | Invalid_format of string
  | Invalid_argument of string

type format_options = {
  left_justify : bool;
  pad_zero : bool;
  width : int64;
  comma : bool;
  truncate : bool;
}

let create ~max_output_bytes ~max_output_work =
  let capacity = min max_output_bytes Sys.max_string_length in
  {
    output = Buffer.create (min 256 capacity);
    bytes = { capacity; committed = 0 };
    work_budget = { limit = max_output_work; count = 0 };
  }

let share_work state ~max_output_bytes =
  let capacity = min max_output_bytes Sys.max_string_length in
  {
    output = Buffer.create (min 256 capacity);
    bytes = { capacity; committed = 0 };
    work_budget = state.work_budget;
  }

let fork state =
  { state with output = Buffer.create (min 256 state.bytes.capacity) }

let contents state = Buffer.contents state.output
let work state = state.work_budget.count
let committed_bytes state = state.bytes.committed
let ( let* ) = Result.bind

let charge state =
  if state.work_budget.count >= state.work_budget.limit then Error Work_limit
  else (
    state.work_budget.count <- state.work_budget.count + 1;
    Ok ())

let append state ~capacity buffer byte =
  let* () = charge state in
  if Buffer.length buffer >= capacity then Error Output_limit
  else (
    Buffer.add_char buffer byte;
    Ok ())

let next_offset offset =
  if offset = Int64.max_int then Error Offset_overflow
  else Ok (Int64.succ offset)

let format_draft state ~read_byte ~format arguments =
  let capacity = state.bytes.capacity - state.bytes.committed in
  let draft = Buffer.create (min 256 capacity) in
  let emit byte = append state ~capacity draft byte in
  let read pointer offset =
    let* () = charge state in
    Result.map_error (fun error -> Memory error) (read_byte pointer offset)
  in
  let emit_repeat byte count =
    let remaining = ref count in
    let failed = ref None in
    while Int64.compare !remaining 0L > 0 && Option.is_none !failed do
      match emit byte with
      | Ok () -> remaining := Int64.pred !remaining
      | Error error -> failed := Some error
    done;
    match !failed with
    | None -> Ok ()
    | Some error -> Error error
  in
  let string_plain pointer =
    let offset = ref 0L in
    let complete = ref false in
    let failed = ref None in
    while (not !complete) && Option.is_none !failed do
      match read pointer !offset with
      | Error error -> failed := Some error
      | Ok '\000' -> complete := true
      | Ok byte -> (
          match emit byte with
          | Error error -> failed := Some error
          | Ok () -> (
              match next_offset !offset with
              | Ok next -> offset := next
              | Error error -> failed := Some error))
    done;
    match !failed with
    | None -> Ok ()
    | Some error -> Error error
  in
  let string_length pointer =
    let offset = ref 0L in
    let complete = ref false in
    let failed = ref None in
    while (not !complete) && Option.is_none !failed do
      match read pointer !offset with
      | Error error -> failed := Some error
      | Ok '\000' -> complete := true
      | Ok _ -> (
          match next_offset !offset with
          | Ok next -> offset := next
          | Error error -> failed := Some error)
    done;
    match !failed with
    | Some error -> Error error
    | None -> Ok !offset
  in
  let string_prefix pointer count =
    let offset = ref 0L in
    let remaining = ref count in
    let failed = ref None in
    while Int64.compare !remaining 0L > 0 && Option.is_none !failed do
      match read pointer !offset with
      | Error error -> failed := Some error
      | Ok '\000' -> remaining := 0L
      | Ok byte -> (
          match emit byte with
          | Error error -> failed := Some error
          | Ok () -> (
              remaining := Int64.pred !remaining;
              if Int64.compare !remaining 0L > 0 then
                match next_offset !offset with
                | Ok next -> offset := next
                | Error error -> failed := Some error))
    done;
    match !failed with
    | None -> Ok ()
    | Some error -> Error error
  in
  let nonnegative value = if Int64.compare value 0L < 0 then 0L else value in
  let field_counts options content_length =
    let width = nonnegative options.width in
    let output_length =
      if options.truncate && Int64.compare content_length width > 0 then width
      else content_length
    in
    let padding =
      if Int64.compare width output_length > 0 then
        Int64.sub width output_length
      else 0L
    in
    (output_length, padding)
  in
  let string_field pointer options =
    if Int64.compare options.width 0L <= 0 && not options.truncate then
      string_plain pointer
    else
      let* length = string_length pointer in
      let output_length, padding = field_counts options length in
      if options.left_justify then
        let* () = string_prefix pointer output_length in
        emit_repeat ' ' padding
      else
        let* () = emit_repeat ' ' padding in
        string_prefix pointer output_length
  in
  let packed_plain bits =
    let shifted = ref bits in
    let remaining = ref 8 in
    let complete = ref false in
    let failed = ref None in
    while !remaining > 0 && (not !complete) && Option.is_none !failed do
      match charge state with
      | Error error -> failed := Some error
      | Ok () -> (
          let byte = Int64.to_int (Int64.logand !shifted 255L) in
          if byte = 0 then complete := true
          else
            match emit (Char.chr byte) with
            | Error error -> failed := Some error
            | Ok () ->
                decr remaining;
                shifted := Int64.shift_right_logical !shifted 8)
    done;
    match !failed with
    | None -> Ok ()
    | Some error -> Error error
  in
  let packed_length bits =
    let shifted = ref bits in
    let length = ref 0 in
    let visits = ref 0 in
    let complete = ref false in
    let failed = ref None in
    while !visits < 8 && (not !complete) && Option.is_none !failed do
      match charge state with
      | Error error -> failed := Some error
      | Ok () ->
          incr visits;
          let byte = Int64.to_int (Int64.logand !shifted 255L) in
          if byte = 0 then complete := true
          else (
            incr length;
            shifted := Int64.shift_right_logical !shifted 8)
    done;
    match !failed with
    | None -> Ok !length
    | Some error -> Error error
  in
  let packed_prefix bits count =
    let index = ref 0 in
    let failed = ref None in
    while !index < count && Option.is_none !failed do
      let byte =
        Int64.shift_right_logical bits (8 * !index)
        |> Int64.logand 255L |> Int64.to_int |> Char.chr
      in
      match emit byte with
      | Ok () -> incr index
      | Error error -> failed := Some error
    done;
    match !failed with
    | None -> Ok ()
    | Some error -> Error error
  in
  let packed_field bits options =
    if Int64.compare options.width 0L <= 0 && not options.truncate then
      packed_plain bits
    else
      let* length = packed_length bits in
      let width = nonnegative options.width in
      let output_length =
        if options.truncate then
          if Int64.compare width (Int64.of_int length) < 0 then
            Int64.to_int width
          else length
        else length
      in
      let padding =
        let output_length = Int64.of_int output_length in
        if Int64.compare width output_length > 0 then
          Int64.sub width output_length
        else 0L
      in
      if options.left_justify then
        let* () = packed_prefix bits output_length in
        emit_repeat ' ' padding
      else
        let* () = emit_repeat ' ' padding in
        packed_prefix bits output_length
  in
  let digit byte = byte >= '0' && byte <= '9' in
  let digit_value byte = Char.code byte - Char.code '0' in
  let literal_digit label value byte =
    let digit = Int64.of_int (digit_value byte) in
    let limit = Int64.div (Int64.sub Int64.max_int digit) 10L in
    if Int64.compare value limit > 0 then
      Error
        (Invalid_format
           ("Print format " ^ label ^ " exceeds the signed I64 range"))
    else Ok (Int64.add (Int64.mul value 10L) digit)
  in
  let number_buffer bits ~signed ~base ~uppercase ~comma =
    let bytes = Bytes.create 79 in
    let length = ref 0 in
    let group = if base = 10 then 3 else 4 in
    let comma_count = ref group in
    let negative = signed && Int64.compare bits 0L < 0 in
    let value = ref (if negative then Int64.neg bits else bits) in
    let push byte =
      if !length >= Bytes.length bytes then
        invalid_arg "integer output number buffer overflow";
      Bytes.set bytes !length byte;
      incr length
    in
    let digit_byte value =
      if value < 10 then Char.chr (Char.code '0' + value)
      else Char.chr (Char.code (if uppercase then 'A' else 'a') + (value - 10))
    in
    let divisor = Int64.of_int base in
    let complete = ref false in
    while not !complete do
      let remainder = Int64.unsigned_rem !value divisor |> Int64.to_int in
      let quotient = Int64.unsigned_div !value divisor in
      push (digit_byte remainder);
      if quotient = 0L then complete := true
      else (
        value := quotient;
        if comma then (
          decr comma_count;
          if !comma_count = 0 then (
            push ',';
            comma_count := group)))
    done;
    (bytes, !length, !comma_count, group, negative)
  in
  let emit_reverse bytes length =
    let index = ref (length - 1) in
    let failed = ref None in
    while !index >= 0 && Option.is_none !failed do
      match emit (Bytes.get bytes !index) with
      | Ok () -> decr index
      | Error error -> failed := Some error
    done;
    match !failed with
    | None -> Ok ()
    | Some error -> Error error
  in
  let comma_zero_padding padding ~group ~comma_count =
    if Int64.compare padding 0L <= 0 then Ok ()
    else
      let modulus = Int64.of_int (group + 1) in
      let adjustment = group - comma_count + 1 in
      let countdown =
        Int64.add
          (Int64.rem
             (Int64.add (Int64.rem padding modulus) (Int64.of_int adjustment))
             modulus)
          1L
        |> Int64.to_int |> ref
      in
      let remaining = ref padding in
      let failed = ref None in
      while Int64.compare !remaining 0L > 0 && Option.is_none !failed do
        decr countdown;
        if !countdown = 0 then
          match emit ',' with
          | Error error -> failed := Some error
          | Ok () -> (
              remaining := Int64.pred !remaining;
              countdown := group;
              if Int64.compare !remaining 0L > 0 then
                match emit '0' with
                | Error error -> failed := Some error
                | Ok () -> remaining := Int64.pred !remaining)
        else
          match emit '0' with
          | Error error -> failed := Some error
          | Ok () -> remaining := Int64.pred !remaining
      done;
      match !failed with
      | None -> Ok ()
      | Some error -> Error error
  in
  let number_field bits ~signed ~base ~uppercase options =
    let bytes, original_length, comma_count, group, negative =
      number_buffer bits ~signed ~base ~uppercase ~comma:options.comma
    in
    let width = nonnegative options.width in
    let sign_length = if negative then 1 else 0 in
    let length =
      if
        options.truncate
        && Int64.compare (Int64.of_int (original_length + sign_length)) width
           > 0
      then
        let available = Int64.sub width (Int64.of_int sign_length) in
        if Int64.compare available 0L <= 0 then 0
        else if Int64.compare available (Int64.of_int original_length) < 0 then
          Int64.to_int available
        else original_length
      else original_length
    in
    let occupied = Int64.of_int (length + sign_length) in
    let padding =
      if Int64.compare width occupied > 0 then Int64.sub width occupied else 0L
    in
    if options.pad_zero then
      let* () = if negative then emit '-' else Ok () in
      let* () =
        if options.comma then comma_zero_padding padding ~group ~comma_count
        else emit_repeat '0' padding
      in
      emit_reverse bytes length
    else
      let* () = emit_repeat ' ' padding in
      let* () = if negative then emit '-' else Ok () in
      emit_reverse bytes length
  in
  let argument position directive =
    if position >= Array.length arguments then
      Error
        (Invalid_argument ("Print format requires an argument for %" ^ directive))
    else Ok arguments.(position)
  in
  let star_argument position role =
    if position >= Array.length arguments then
      Error
        (Invalid_argument ("Print format " ^ role ^ " requires an argument"))
    else
      match arguments.(position) with
      | Word bits -> Ok (bits, position + 1)
      | Pointer _ ->
          Error
            (Invalid_argument
               ("Print format " ^ role ^ " requires an integer word"))
  in
  let format_required offset =
    let* byte = read format offset in
    if byte = '\000' then
      Error (Invalid_format "Print format ends after percent")
    else
      let* offset = next_offset offset in
      Ok (byte, offset)
  in
  let rec literal_digits label value byte offset =
    if digit byte then
      let* value = literal_digit label value byte in
      let* byte, offset = format_required offset in
      literal_digits label value byte offset
    else Ok (value, byte, offset)
  in
  let modifiers byte offset =
    let current = ref byte in
    let cursor = ref offset in
    let comma = ref false in
    let truncate = ref false in
    let complete = ref false in
    let failed = ref None in
    while (not !complete) && Option.is_none !failed do
      match !current with
      | (',' | 't' | 'l' | '$' | '/') as modifier -> (
          if modifier = ',' then comma := true;
          if modifier = 't' then truncate := true;
          match format_required !cursor with
          | Ok (byte, offset) ->
              current := byte;
              cursor := offset
          | Error error -> failed := Some error)
      | _ -> complete := true
    done;
    match !failed with
    | Some error -> Error error
    | None -> Ok (!comma, !truncate, !current, !cursor)
  in
  let render directive options position =
    let directive_name = String.make 1 directive in
    match directive with
    | '%' ->
        let* () = emit '%' in
        Ok position
    | 'd' | 'u' | 'x' | 'X' | 'b' | 'B' -> (
        let* value = argument position directive_name in
        match value with
        | Pointer _ ->
            Error
              (Invalid_argument
                 ("Print %" ^ directive_name ^ " requires an integer word"))
        | Word bits ->
            let signed = directive = 'd' in
            let base =
              if directive = 'd' || directive = 'u' then 10
              else if directive = 'x' || directive = 'X' then 16
              else 2
            in
            let uppercase = directive = 'X' in
            let* () = number_field bits ~signed ~base ~uppercase options in
            Ok (position + 1))
    | 's' -> (
        let* value = argument position "s" in
        match value with
        | Pointer pointer ->
            let* () = string_field pointer options in
            Ok (position + 1)
        | Word _ ->
            Error (Invalid_argument "Print %s requires an owned U8 pointer"))
    | 'c' -> (
        let* value = argument position "c" in
        match value with
        | Word bits ->
            let* () = packed_field bits options in
            Ok (position + 1)
        | Pointer _ ->
            Error (Invalid_argument "Print %c requires an integer word"))
    | _ -> Error (Invalid_format "Print format directive is not supported")
  in
  let format_spec offset position =
    let* first, offset = format_required offset in
    let* left_justify, byte, offset =
      if first = '-' then
        let* byte, offset = format_required offset in
        Ok (true, byte, offset)
      else Ok (false, first, offset)
    in
    let* pad_zero, byte, offset =
      if byte = '0' then
        let* byte, offset = format_required offset in
        Ok (true, byte, offset)
      else Ok (false, byte, offset)
    in
    let* width, byte, offset, position =
      if byte = '*' then
        let* width, position = star_argument position "width" in
        let* byte, offset = format_required offset in
        Ok (width, byte, offset, position)
      else if digit byte then
        let* width, byte, offset = literal_digits "width" 0L byte offset in
        if byte = '*' then
          let* width, position = star_argument position "width" in
          let* byte, offset = format_required offset in
          Ok (width, byte, offset, position)
        else Ok (width, byte, offset, position)
      else Ok (0L, byte, offset, position)
    in
    let* byte, offset, position =
      if byte = '.' then
        let* precision, offset = format_required offset in
        if precision = '*' then
          let* _, position = star_argument position "precision" in
          let* byte, offset = format_required offset in
          Ok (byte, offset, position)
        else if digit precision then
          let* _, byte, offset =
            literal_digits "precision" 0L precision offset
          in
          if byte = '*' then
            let* _, position = star_argument position "precision" in
            let* byte, offset = format_required offset in
            Ok (byte, offset, position)
          else Ok (byte, offset, position)
        else Ok (precision, offset, position)
      else Ok (byte, offset, position)
    in
    let* comma, truncate, directive, offset = modifiers byte offset in
    let options = { left_justify; pad_zero; width; comma; truncate } in
    let* position = render directive options position in
    Ok (offset, position)
  in
  let rec scan offset position =
    let* byte = read format offset in
    if byte = '\000' then Ok ()
    else
      let* offset = next_offset offset in
      if byte <> '%' then
        let* () = emit byte in
        scan offset position
      else
        let* offset, position = format_spec offset position in
        scan offset position
  in
  let* () = scan 0L 0 in
  Ok draft

let discard_print state ~read_byte ~format arguments =
  format_draft state ~read_byte ~format arguments |> Result.map (fun _ -> ())

let print state ~read_byte ~format arguments =
  let* draft = format_draft state ~read_byte ~format arguments in
  Buffer.add_buffer state.output draft;
  state.bytes.committed <- state.bytes.committed + Buffer.length draft;
  Ok ()

let put_chars state bits =
  let rec loop bits =
    if bits = 0L then Ok ()
    else
      let* () = charge state in
      let byte = Int64.to_int (Int64.logand bits 255L) in
      let* () =
        if byte = 0 then Ok ()
        else
          let capacity =
            Buffer.length state.output
            + (state.bytes.capacity - state.bytes.committed)
          in
          let* () = append state ~capacity state.output (Char.chr byte) in
          state.bytes.committed <- state.bytes.committed + 1;
          Ok ()
      in
      loop (Int64.shift_right_logical bits 8)
  in
  loop bits
