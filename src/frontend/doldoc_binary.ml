module Number_map = Map.Make (Int64)

type record = {
  number : int64;
  flags : int64;
  payload : string;
  declared_size : int64;
  payload_complete : bool;
  use_count : int64;
  header_offset : int;
}

type error_kind =
  | Truncated_header
  | Payload_too_large
  | Truncated_payload
  | Duplicate_record

type error = {
  kind : error_kind;
  offset : int;
  record_number : int64 option;
  message : string;
}

type t = { text_terminator : int option; records : record Number_map.t }

let header_bytes = 16

let first_nul contents =
  let rec find offset =
    if offset = String.length contents then None
    else if Char.equal contents.[offset] '\x00' then Some offset
    else find (offset + 1)
  in
  find 0

let uint32_le contents offset =
  let byte index = Int64.of_int (Char.code contents.[offset + index]) in
  Int64.logor (byte 0)
    (Int64.logor
       (Int64.shift_left (byte 1) 8)
       (Int64.logor
          (Int64.shift_left (byte 2) 16)
          (Int64.shift_left (byte 3) 24)))

let plausible_header contents offset =
  let remaining = String.length contents - offset in
  if remaining < header_bytes then false
  else
    let flags = uint32_le contents (offset + 4) in
    let payload_size = uint32_le contents (offset + 8) in
    let use_count = uint32_le contents (offset + 12) in
    Int64.equal flags 0L
    && payload_size <= 0x0400_0000L
    && use_count > 0L && use_count <= 1_000_000L

let recovered_records contents text_terminator =
  let length = String.length contents in
  let rec decode_records offset found =
    if offset = length then
      Ok { text_terminator = Some text_terminator; records = found }
    else if length - offset < header_bytes then
      Error
        {
          kind = Truncated_header;
          offset;
          record_number = None;
          message =
            Printf.sprintf
              "the saved DolDoc binary table ends with a %d-byte partial header"
              (length - offset);
        }
    else
      let number = uint32_le contents offset in
      let flags = uint32_le contents (offset + 4) in
      let declared_size = uint32_le contents (offset + 8) in
      let use_count = uint32_le contents (offset + 12) in
      if Number_map.mem number found then
        Error
          {
            kind = Duplicate_record;
            offset;
            record_number = Some number;
            message =
              Printf.sprintf
                "the saved DolDoc binary table contains record %Ld more than \
                 once"
                number;
          }
      else
        let payload_offset = offset + header_bytes in
        let expected_stop =
          Int64.add (Int64.of_int payload_offset) declared_size
        in
        let exact_stop =
          if expected_stop > Int64.of_int Stdlib.max_int then None
          else Some (Int64.to_int expected_stop)
        in
        let next_offset =
          match exact_stop with
          | Some stop when stop = length -> Some stop
          | Some stop when stop < length && plausible_header contents stop ->
              Some stop
          | None -> None
          | Some stop when stop > length -> None
          | Some _ ->
              let upper =
                match exact_stop with
                | None -> length - header_bytes
                | Some stop -> min (stop - 1) (length - header_bytes)
              in
              let rec search candidate =
                if candidate < payload_offset then None
                else if plausible_header contents candidate then Some candidate
                else search (candidate - 1)
              in
              search upper
        in
        let payload_stop = Option.value next_offset ~default:length in
        let payload_size = payload_stop - payload_offset in
        let payload = String.sub contents payload_offset payload_size in
        let payload_complete =
          Int64.equal declared_size (Int64.of_int payload_size)
        in
        let record =
          {
            number;
            flags;
            payload;
            declared_size;
            payload_complete;
            use_count;
            header_offset = offset;
          }
        in
        let found = Number_map.add number record found in
        match next_offset with
        | None -> Ok { text_terminator = Some text_terminator; records = found }
        | Some stop -> decode_records stop found
  in
  decode_records (text_terminator + 1) Number_map.empty

let decode ?(recover_normalized = false) contents =
  match first_nul contents with
  | None -> Ok { text_terminator = None; records = Number_map.empty }
  | Some text_terminator -> (
      let length = String.length contents in
      let rec decode_records offset found =
        if offset = length then
          Ok { text_terminator = Some text_terminator; records = found }
        else
          let remaining = length - offset in
          if remaining < header_bytes then
            Error
              {
                kind = Truncated_header;
                offset;
                record_number = None;
                message =
                  Printf.sprintf
                    "the saved DolDoc binary table ends with a %d-byte partial \
                     header"
                    remaining;
              }
          else
            let number = uint32_le contents offset in
            let flags = uint32_le contents (offset + 4) in
            let payload_size = uint32_le contents (offset + 8) in
            let use_count = uint32_le contents (offset + 12) in
            if payload_size > Int64.of_int Stdlib.max_int then
              Error
                {
                  kind = Payload_too_large;
                  offset = offset + 8;
                  record_number = Some number;
                  message =
                    Printf.sprintf
                      "DolDoc binary record %Ld declares a payload too large \
                       for this host"
                      number;
                }
            else
              let payload_size = Int64.to_int payload_size in
              let available = remaining - header_bytes in
              if payload_size > available then
                Error
                  {
                    kind = Truncated_payload;
                    offset = offset + 8;
                    record_number = Some number;
                    message =
                      Printf.sprintf
                        "DolDoc binary record %Ld declares %d payload bytes, \
                         but only %d remain"
                        number payload_size available;
                  }
              else if Number_map.mem number found then
                Error
                  {
                    kind = Duplicate_record;
                    offset;
                    record_number = Some number;
                    message =
                      Printf.sprintf
                        "the saved DolDoc binary table contains record %Ld \
                         more than once"
                        number;
                  }
              else
                let payload_offset = offset + header_bytes in
                let payload = String.sub contents payload_offset payload_size in
                let record =
                  {
                    number;
                    flags;
                    payload;
                    declared_size = Int64.of_int payload_size;
                    payload_complete = true;
                    use_count;
                    header_offset = offset;
                  }
                in
                decode_records
                  (payload_offset + payload_size)
                  (Number_map.add number record found)
      in
      match decode_records (text_terminator + 1) Number_map.empty with
      | Ok _ as decoded -> decoded
      | Error _ as strict_error ->
          if recover_normalized then recovered_records contents text_terminator
          else strict_error)

let find table number = Number_map.find_opt number table.records
let records table = Number_map.bindings table.records |> List.map snd
let text_terminator table = table.text_terminator
