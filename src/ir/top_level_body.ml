module Option_ = Sema.Compiler_option

type error = {
  code : string;
  message : string;
  stream_id : int option;
  item_position : int option;
  block_id : int option;
  instruction_id : int option;
  span : Common.Span.t option;
}

let make_error ?stream_id ?item_position ?block_id ?instruction_id ?span code
    message =
  { code; message; stream_id; item_position; block_id; instruction_id; span }

module Stream_id = struct
  type t = int

  let of_int value =
    if value < 0 then
      Error
        (make_error "HCIR0034"
           (Printf.sprintf "top-level stream ID %d must be nonnegative" value))
    else Ok value

  let to_int value = value
  let compare = Int.compare
  let equal = Int.equal
end

type description = {
  stream_id : Stream_id.t;
  item_position : int;
  compiler_options : int64;
  span : Common.Span.t option;
  body : Block_graph.t;
}

type t = {
  stream_id_ : Stream_id.t;
  item_position_ : int;
  compiler_options_ : int64;
  span_ : Common.Span.t option;
  body_ : Block_graph.t;
  x87_ : X87_stack.t;
}

let reference_commit = Instruction_sequence.reference_commit
let stream_number description = Stream_id.to_int description.stream_id

let metadata_errors description =
  let errors = ref [] in
  let add ?span code message =
    errors :=
      make_error
        ~stream_id:(stream_number description)
        ~item_position:description.item_position ?span code message
      :: !errors
  in
  if description.item_position < 0 then
    add "HCIR0035"
      (Printf.sprintf "top-level stream ^t%d has negative item position %d"
         (stream_number description)
         description.item_position)
  else ();
  (match description.span with
  | Some (span : Common.Span.t) when span.start < 0 || span.stop < span.start ->
      add ~span "HCIR0036"
        (Printf.sprintf "top-level stream ^t%d has invalid source span %d..%d"
           (stream_number description)
           span.start span.stop)
  | Some _ | None -> ());
  let unknown_options =
    Int64.logand description.compiler_options (Int64.lognot Option_.known_mask)
  in
  if unknown_options <> 0L then
    add "HCIR0037"
      (Printf.sprintf
         "top-level stream ^t%d uses unsupported compiler option bits 0x%Lx"
         (stream_number description)
         unknown_options)
  else ();
  List.rev !errors

let x87_error description (error : X87_stack.error) =
  make_error
    ~stream_id:(stream_number description)
    ~item_position:description.item_position ?block_id:error.block_id
    ?instruction_id:error.instruction_id ?span:error.span error.code
    error.message

let create description =
  let metadata_errors = metadata_errors description in
  match X87_stack.verify description.body with
  | Error errors ->
      Error (metadata_errors @ List.map (x87_error description) errors)
  | Ok x87 -> (
      match metadata_errors with
      | _ :: _ -> Error metadata_errors
      | [] ->
          Ok
            {
              stream_id_ = description.stream_id;
              item_position_ = description.item_position;
              compiler_options_ = description.compiler_options;
              span_ = description.span;
              body_ = description.body;
              x87_ = x87;
            })

let stream_id body = body.stream_id_
let item_position body = body.item_position_
let compiler_options body = body.compiler_options_
let span body = body.span_
let body body = body.body_
let x87 body = body.x87_

let option_names mask =
  Option_.all
  |> List.filter (Option_.is_enabled ~mask)
  |> List.map Option_.to_string

let add_names buffer names =
  Buffer.add_char buffer '[';
  List.iteri
    (fun index name ->
      if index > 0 then Buffer.add_char buffer ',';
      Buffer.add_string buffer name)
    names;
  Buffer.add_char buffer ']'

let add_span buffer = function
  | None -> ()
  | Some (span : Common.Span.t) ->
      Printf.bprintf buffer " @source=%d:%d..%d"
        (Common.Source_id.to_int span.source)
        span.start span.stop

let human body =
  let buffer = Buffer.create 768 in
  Printf.bprintf buffer "holyc-ir-top-level-v1 reference=%s\n" reference_commit;
  Printf.bprintf buffer "stream ^t%d item-position=%d options=0x%Lx "
    (Stream_id.to_int body.stream_id_)
    body.item_position_ body.compiler_options_;
  add_names buffer (option_names body.compiler_options_);
  add_span buffer body.span_;
  Buffer.add_char buffer '\n';
  Buffer.add_string buffer "body\n";
  Buffer.add_string buffer (Block_graph.human_body body.body_);
  Buffer.add_string buffer "x87=verified\n";
  Buffer.contents buffer
