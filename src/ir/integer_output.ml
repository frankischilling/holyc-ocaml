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
  let rec string pointer offset =
    let* byte = read pointer offset in
    if byte = '\000' then Ok ()
    else
      let* () = emit byte in
      let* offset = next_offset offset in
      string pointer offset
  in
  let rec packed bits remaining =
    if remaining = 0 then Ok ()
    else
      let* () = charge state in
      let byte = Int64.to_int (Int64.logand bits 255L) in
      if byte = 0 then Ok ()
      else
        let* () = emit (Char.chr byte) in
        packed (Int64.shift_right_logical bits 8) (remaining - 1)
  in
  let rec decimal text index =
    if index = String.length text then Ok ()
    else
      let* () = emit text.[index] in
      decimal text (index + 1)
  in
  let argument position directive =
    if position >= Array.length arguments then
      Error
        (Invalid_argument ("Print format requires an argument for %" ^ directive))
    else Ok arguments.(position)
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
        let* directive = read format offset in
        let* offset = next_offset offset in
        match directive with
        | '%' ->
            let* () = emit '%' in
            scan offset position
        | 'd' -> (
            let* value = argument position "d" in
            match value with
            | Word bits ->
                let* () = decimal (Int64.to_string bits) 0 in
                scan offset (position + 1)
            | Pointer _ ->
                Error (Invalid_argument "Print %d requires an integer word"))
        | 's' -> (
            let* value = argument position "s" in
            match value with
            | Pointer pointer ->
                let* () = string pointer 0L in
                scan offset (position + 1)
            | Word _ ->
                Error (Invalid_argument "Print %s requires an owned U8 pointer")
            )
        | 'c' -> (
            let* value = argument position "c" in
            match value with
            | Word bits ->
                let* () = packed bits 8 in
                scan offset (position + 1)
            | Pointer _ ->
                Error (Invalid_argument "Print %c requires an integer word"))
        | '\000' -> Error (Invalid_format "Print format ends after percent")
        | '-' | '+' | '0' .. '9' | '.' | '*' | ',' | '$' ->
            Error
              (Invalid_format "Print format flags and widths are not supported")
        | _ -> Error (Invalid_format "Print format directive is not supported")
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
