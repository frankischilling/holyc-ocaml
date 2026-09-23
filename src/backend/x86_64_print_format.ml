module E = X86_64_encoder

type argument_kind =
  | Word
  | Unsigned_byte_pointer
  | Signed_byte_pointer
  | Other_pointer

type t = {
  format_stage : int;
  arguments_stage : int;
  argument_kinds : argument_kind array;
  scratch_stage : int;
  activation_bytes : int;
}

type branch = Always | Equal | Not_equal | Below | Less | Overflow

type 'label emitter = {
  instruction : E.instruction -> unit;
  fresh : unit -> 'label;
  mark : 'label -> unit;
  branch : branch -> 'label -> unit;
  fault : int -> 'label;
  slot : int -> E.stack_slot;
}

let format_offset = 0
let argument_index = 1
let draft_length = 2
let current_pointer = 3
let current_offset = 4
let current_kind = 5
let current_word = 6
let visit_count = 7
let field_width = 8
let format_flags = 9
let current_byte = 10
let negative = 11
let payload_length = 12
let comma_count = 13
let pad_remaining = 14
let precision_value = 15
let copy_remaining = 16
let temporary_word = 17
let number_base = 18
let number_group = 19
let number_alpha = 20
let number_signed = 21
let number_buffer = 22
let number_buffer_bytes = 80
let number_buffer_slots = number_buffer_bytes / 8
let quote_phase = number_buffer + number_buffer_slots
let quote_chunk0 = quote_phase + 1
let quote_chunk1 = quote_chunk0 + 1
let quote_chunk2 = quote_chunk1 + 1
let quote_chunk3 = quote_chunk2 + 1
let quote_chunk_length = quote_chunk3 + 1
let quote_frozen = quote_chunk_length + 1
let quote_peek = quote_frozen + 1
let quote_hex_value = quote_peek + 1
let quote_hex_digits = quote_hex_value + 1
let fixed_scratch_slots = quote_hex_digits + 1
let flag_left = 1L
let flag_zero = 2L
let flag_comma = 4L
let flag_truncate = 8L
let flag_dollar = 16L
let flag_slash = 32L
let flag_uppercase = 64L

let scratch_slots count =
  if count < 0 || count > max_int - fixed_scratch_slots then
    invalid_arg "native Print argument count exceeds its scratch representation";
  fixed_scratch_slots + count

let kind_tag = function
  | Word -> 0L
  | Unsigned_byte_pointer -> 1L
  | Signed_byte_pointer -> 2L
  | Other_pointer -> 3L

let emit emitter call =
  let count = Array.length call.argument_kinds in
  let scratch_count = scratch_slots count in
  if
    call.format_stage < 0 || call.arguments_stage < 0 || call.scratch_stage < 0
    || count > (max_int / 8) - 2
    || call.format_stage > max_int - 2
    || call.arguments_stage <> call.format_stage + 2
    || call.arguments_stage > max_int - count
    || call.scratch_stage <> call.arguments_stage + count
    || call.scratch_stage > max_int - scratch_count
    || call.activation_bytes <> (count + 2) * 8
  then invalid_arg "native Print has inconsistent staged call storage";
  let out = emitter.instruction in
  let jump = emitter.branch in
  let mark = emitter.mark in
  let fresh = emitter.fresh in
  let fault = emitter.fault in
  let stage offset = emitter.slot (call.scratch_stage + offset) in
  let depth_fault = fault 4 in
  let frame_fault = fault 5 in
  let unknown_fault = fault 7 in
  let overflow_fault = fault 9 in
  let bounds_fault = fault 10 in
  let output_fault = fault 11 in
  let work_fault = fault 12 in
  let format_fault = fault 13 in
  let argument_fault = fault 14 in
  let pointer_fault = fault 15 in
  let byte_fault = fault 16 in
  (* Runtime parser state and bounded payload storage. The reverse numeric
     buffer is large enough for 64 binary digits plus fifteen group commas. *)
  let load register offset = out (E.Load_stack (register, stage offset)) in
  let store offset register = out (E.Store_stack (stage offset, register)) in
  let constant offset value =
    out (E.Mov_imm64 (E.Rax, value));
    store offset E.Rax
  in
  let zero offset = constant offset 0L in
  let copy_slot source target =
    load E.Rax source;
    store target E.Rax
  in
  let increment offset =
    load E.Rcx offset;
    out (E.Mov_imm64 (E.R8, 1L));
    out (E.Binary (E.Add, E.Rcx, E.R8));
    jump Overflow overflow_fault;
    store offset E.Rcx
  in
  let charge () =
    out (E.Load_context (E.Rcx, 96));
    out (E.Test E.Rcx);
    jump Equal work_fault;
    out (E.Dec E.Rcx);
    out (E.Store_context (96, E.Rcx))
  in
  let append () =
    (* The byte is saved before work/capacity checks clobber scratch registers. *)
    store current_byte E.Rax;
    charge ();
    load E.Rax draft_length;
    out (E.Load_context (E.Rcx, 88));
    out (E.Cmp (E.Rax, E.Rcx));
    let available = fresh () in
    jump Below available;
    jump Always output_fault;
    mark available;
    out (E.Load_context (E.Rdx, 80));
    out (E.Load_context (E.Rcx, 104));
    out (E.Binary (E.Add, E.Rdx, E.Rcx));
    out (E.Binary (E.Add, E.Rdx, E.Rax));
    load E.R8 current_byte;
    out (E.Store_indirect_narrow (E.Rdx, E.Frame8, E.R8));
    out (E.Mov_imm64 (E.Rcx, 1L));
    out (E.Binary (E.Add, E.Rax, E.Rcx));
    store draft_length E.Rax
  in
  let read_byte () =
    (* Work is consumed even when pointer kind, bounds or initialization fail. *)
    charge ();
    load E.Rax current_kind;
    out (E.Cmp_imm8 (E.Rax, 3));
    jump Equal pointer_fault;
    load E.Rdx current_pointer;
    out (E.Load_indirect (E.Rax, E.Rdx, 16));
    out (E.Test E.Rax);
    jump Less bounds_fault;
    load E.Rcx current_offset;
    out (E.Test E.Rcx);
    jump Less bounds_fault;
    out (E.Binary (E.Add, E.Rax, E.Rcx));
    jump Overflow overflow_fault;
    out (E.Load_indirect (E.R8, E.Rdx, 24));
    out (E.Cmp (E.Rax, E.R8));
    let in_bounds = fresh () in
    jump Below in_bounds;
    jump Always bounds_fault;
    mark in_bounds;
    out (E.Load_indirect (E.R8, E.Rdx, 8));
    let initialized = fresh () in
    out (E.Test E.R8);
    jump Equal initialized;
    out (E.Mov (E.Rcx, E.Rax));
    for _ = 1 to 3 do
      out (E.Binary (E.Add, E.Rcx, E.Rcx))
    done;
    out (E.Binary (E.Sub, E.R8, E.Rcx));
    out (E.Load_indirect_narrow (E.Rcx, E.R8, E.Frame8, E.Zero_extend));
    out (E.Test E.Rcx);
    jump Equal unknown_fault;
    mark initialized;
    load E.Rcx current_kind;
    out (E.Cmp_imm8 (E.Rcx, 2));
    jump Equal byte_fault;
    out (E.Load_indirect (E.Rdx, E.Rdx, 0));
    out (E.Binary (E.Add, E.Rdx, E.Rax));
    out (E.Load_indirect_narrow (E.Rax, E.Rdx, E.Frame8, E.Zero_extend))
  in
  let read_format () =
    out (E.Load_stack (E.Rax, emitter.slot call.format_stage));
    store current_pointer E.Rax;
    load E.Rax format_offset;
    store current_offset E.Rax;
    constant current_kind 1L;
    read_byte ();
    store current_byte E.Rax;
    increment format_offset;
    load E.Rax current_byte
  in
  let take_argument ~pointer =
    if count = 0 then jump Always argument_fault
    else (
      load E.Rax argument_index;
      out (E.Mov_imm64 (E.Rcx, Int64.of_int count));
      out (E.Cmp (E.Rax, E.Rcx));
      let present = fresh () in
      jump Below present;
      jump Always argument_fault;
      mark present;
      out (E.Mov (E.Rcx, E.Rax));
      for _ = 1 to 3 do
        out (E.Binary (E.Add, E.Rcx, E.Rcx))
      done;
      out (E.Address_stack (E.Rdx, stage fixed_scratch_slots));
      out (E.Binary (E.Add, E.Rdx, E.Rcx));
      out (E.Load_indirect (E.R8, E.Rdx, 0));
      out (E.Test E.R8);
      jump (if pointer then Equal else Not_equal) argument_fault;
      store current_kind E.R8;
      out (E.Address_stack (E.Rdx, emitter.slot call.arguments_stage));
      out (E.Binary (E.Add, E.Rdx, E.Rcx));
      out (E.Load_indirect (E.Rax, E.Rdx, 0));
      store (if pointer then current_pointer else current_word) E.Rax;
      increment argument_index)
  in
  let set_flag mask =
    load E.Rax format_flags;
    out (E.Mov_imm64 (E.Rcx, mask));
    out (E.Binary (E.Or, E.Rax, E.Rcx));
    store format_flags E.Rax
  in
  let test_flag mask =
    load E.Rax format_flags;
    out (E.Mov_imm64 (E.Rcx, mask));
    out (E.Binary (E.And, E.Rax, E.Rcx));
    out (E.Test E.Rax)
  in
  let uppercase_packed_byte () =
    let done_ = fresh () in
    let raw = fresh () in
    let convert = fresh () in
    store current_byte E.Rax;
    test_flag flag_uppercase;
    jump Equal raw;
    load E.Rax current_byte;
    out (E.Cmp_imm8 (E.Rax, Char.code 'a'));
    jump Below done_;
    out (E.Cmp_imm8 (E.Rax, Char.code '{'));
    jump Below convert;
    jump Always done_;
    mark convert;
    out (E.Mov_imm64 (E.Rcx, 32L));
    out (E.Binary (E.Sub, E.Rax, E.Rcx));
    jump Always done_;
    mark raw;
    load E.Rax current_byte;
    mark done_
  in
  let emit_repeat counter byte =
    let loop = fresh () in
    let done_ = fresh () in
    mark loop;
    load E.Rax counter;
    out (E.Test E.Rax);
    jump Equal done_;
    out (E.Mov_imm64 (E.Rax, Int64.of_int byte));
    append ();
    load E.Rax counter;
    out (E.Dec E.Rax);
    store counter E.Rax;
    jump Always loop;
    mark done_
  in
  let push_number_byte register =
    store current_byte register;
    load E.Rax payload_length;
    out (E.Mov_imm64 (E.Rcx, Int64.of_int number_buffer_bytes));
    out (E.Cmp (E.Rax, E.Rcx));
    let space = fresh () in
    jump Below space;
    jump Always format_fault;
    mark space;
    out (E.Address_stack (E.Rcx, stage number_buffer));
    out (E.Binary (E.Add, E.Rcx, E.Rax));
    load E.R8 current_byte;
    out (E.Store_indirect_narrow (E.Rcx, E.Frame8, E.R8));
    increment payload_length
  in
  let emit_sign () =
    let done_ = fresh () in
    load E.Rax negative;
    out (E.Test E.Rax);
    jump Equal done_;
    out (E.Mov_imm64 (E.Rax, 45L));
    append ();
    mark done_
  in
  let emit_buffer_reversed () =
    let loop = fresh () in
    let done_ = fresh () in
    mark loop;
    load E.Rcx payload_length;
    out (E.Test E.Rcx);
    jump Equal done_;
    out (E.Dec E.Rcx);
    store payload_length E.Rcx;
    out (E.Address_stack (E.Rdx, stage number_buffer));
    out (E.Binary (E.Add, E.Rdx, E.Rcx));
    out (E.Load_indirect_narrow (E.Rax, E.Rdx, E.Frame8, E.Zero_extend));
    append ();
    jump Always loop;
    mark done_
  in
  let emit_outstr_counts () =
    zero pad_remaining;
    copy_slot payload_length copy_remaining;
    let no_truncate = fresh () in
    let negative_width = fresh () in
    let shorter_than_width = fresh () in
    let done_ = fresh () in
    test_flag flag_truncate;
    jump Equal no_truncate;
    load E.Rax field_width;
    out (E.Test E.Rax);
    jump Less negative_width;
    load E.Rcx payload_length;
    out (E.Cmp (E.Rax, E.Rcx));
    jump Less shorter_than_width;
    out (E.Binary (E.Sub, E.Rax, E.Rcx));
    store pad_remaining E.Rax;
    jump Always done_;
    mark shorter_than_width;
    store copy_remaining E.Rax;
    jump Always done_;
    mark negative_width;
    zero copy_remaining;
    jump Always done_;
    mark no_truncate;
    load E.Rax field_width;
    load E.Rcx payload_length;
    out (E.Cmp (E.Rax, E.Rcx));
    jump Less done_;
    jump Equal done_;
    out (E.Binary (E.Sub, E.Rax, E.Rcx));
    store pad_remaining E.Rax;
    mark done_
  in
  let emit_string_copy () =
    zero current_offset;
    let loop = fresh () in
    let done_ = fresh () in
    mark loop;
    load E.Rax copy_remaining;
    out (E.Test E.Rax);
    jump Equal done_;
    read_byte ();
    append ();
    increment current_offset;
    load E.Rax copy_remaining;
    out (E.Dec E.Rax);
    store copy_remaining E.Rax;
    jump Always loop;
    mark done_
  in
  let emit_packed_copy () =
    copy_slot current_word temporary_word;
    let loop = fresh () in
    let done_ = fresh () in
    mark loop;
    load E.Rax copy_remaining;
    out (E.Test E.Rax);
    jump Equal done_;
    load E.Rax temporary_word;
    out (E.Mov_imm64 (E.Rcx, 255L));
    out (E.Binary (E.And, E.Rax, E.Rcx));
    uppercase_packed_byte ();
    append ();
    load E.Rax temporary_word;
    out (E.Mov_imm64 (E.Rcx, 8L));
    out (E.Shift_cl (E.Shr, E.Rax));
    store temporary_word E.Rax;
    load E.Rax copy_remaining;
    out (E.Dec E.Rax);
    store copy_remaining E.Rax;
    jump Always loop;
    mark done_
  in
  let emit_outstr_layout copy =
    emit_outstr_counts ();
    let left = fresh () in
    let done_ = fresh () in
    test_flag flag_left;
    jump Not_equal left;
    emit_repeat pad_remaining 32;
    copy ();
    jump Always done_;
    mark left;
    copy ();
    emit_repeat pad_remaining 32;
    mark done_
  in
  let emit_bare_string done_label =
    take_argument ~pointer:true;
    zero current_offset;
    let loop = fresh () in
    mark loop;
    read_byte ();
    out (E.Test E.Rax);
    jump Equal done_label;
    append ();
    increment current_offset;
    jump Always loop
  in
  let emit_formatted_string done_label =
    take_argument ~pointer:true;
    let measure = fresh () in
    let stream = fresh () in
    let scan = fresh () in
    let scanned = fresh () in
    test_flag flag_truncate;
    jump Not_equal measure;
    load E.Rax field_width;
    out (E.Test E.Rax);
    jump Equal stream;
    jump Less stream;
    jump Always measure;
    mark stream;
    zero current_offset;
    let stream_loop = fresh () in
    mark stream_loop;
    read_byte ();
    out (E.Test E.Rax);
    jump Equal done_label;
    append ();
    increment current_offset;
    jump Always stream_loop;
    mark measure;
    zero current_offset;
    mark scan;
    read_byte ();
    out (E.Test E.Rax);
    jump Equal scanned;
    increment current_offset;
    jump Always scan;
    mark scanned;
    copy_slot current_offset payload_length;
    emit_outstr_layout emit_string_copy;
    jump Always done_label
  in
  let emit_packed_stream done_label =
    zero visit_count;
    let loop = fresh () in
    mark loop;
    charge ();
    load E.Rax current_word;
    out (E.Mov_imm64 (E.R8, 255L));
    out (E.Binary (E.And, E.Rax, E.R8));
    out (E.Test E.Rax);
    jump Equal done_label;
    uppercase_packed_byte ();
    append ();
    increment visit_count;
    load E.Rax visit_count;
    out (E.Cmp_imm8 (E.Rax, 8));
    jump Equal done_label;
    load E.Rax current_word;
    out (E.Mov_imm64 (E.Rcx, 8L));
    out (E.Shift_cl (E.Shr, E.Rax));
    store current_word E.Rax;
    jump Always loop
  in
  let emit_bare_packed done_label =
    take_argument ~pointer:false;
    emit_packed_stream done_label
  in
  let emit_formatted_packed done_label =
    take_argument ~pointer:false;
    let measure = fresh () in
    let stream = fresh () in
    test_flag flag_truncate;
    jump Not_equal measure;
    load E.Rax field_width;
    out (E.Test E.Rax);
    jump Equal stream;
    jump Less stream;
    jump Always measure;
    mark stream;
    emit_packed_stream done_label;
    mark measure;
    copy_slot current_word temporary_word;
    zero payload_length;
    let loop = fresh () in
    let measured = fresh () in
    mark loop;
    charge ();
    load E.Rax temporary_word;
    out (E.Mov_imm64 (E.Rcx, 255L));
    out (E.Binary (E.And, E.Rax, E.Rcx));
    out (E.Test E.Rax);
    jump Equal measured;
    increment payload_length;
    load E.Rax payload_length;
    out (E.Cmp_imm8 (E.Rax, 8));
    jump Equal measured;
    load E.Rax temporary_word;
    out (E.Mov_imm64 (E.Rcx, 8L));
    out (E.Shift_cl (E.Shr, E.Rax));
    store temporary_word E.Rax;
    jump Always loop;
    mark measured;
    emit_outstr_layout emit_packed_copy;
    jump Always done_label
  in
  let store_quote_chunk slot register = store slot register in
  let quote_single register =
    store_quote_chunk quote_chunk0 register;
    constant quote_chunk_length 1L
  in
  let quote_pair first second =
    constant quote_chunk0 (Int64.of_int first);
    constant quote_chunk1 (Int64.of_int second);
    constant quote_chunk_length 2L
  in
  let store_upper_hex_digit slot =
    let numeric = fresh () in
    let done_ = fresh () in
    out (E.Cmp_imm8 (E.Rax, 10));
    jump Below numeric;
    out (E.Mov_imm64 (E.Rcx, 55L));
    out (E.Binary (E.Add, E.Rax, E.Rcx));
    store slot E.Rax;
    jump Always done_;
    mark numeric;
    out (E.Mov_imm64 (E.Rcx, 48L));
    out (E.Binary (E.Add, E.Rax, E.Rcx));
    store slot E.Rax;
    mark done_
  in
  let emit_quoted done_label ~decode =
    take_argument ~pointer:true;
    zero current_offset;
    zero payload_length;
    zero quote_phase;
    zero quote_frozen;
    let scan = fresh () in
    let source_done = fresh () in
    let chunk_ready = fresh () in
    let chunk_loop = fresh () in
    let chunk_first = fresh () in
    let chunk_next = fresh () in
    let chunk_done = fresh () in
    let copy_done = fresh () in
    mark scan;
    read_byte ();
    store current_byte E.Rax;
    out (E.Test E.Rax);
    jump Equal source_done;
    increment current_offset;
    (if decode then (
       (* MPrintq reads the next source byte for every nonzero current byte. *)
       read_byte ();
       store quote_peek E.Rax;
       let slash = fresh () in
       let dollar = fresh () in
       let percent = fresh () in
       let raw = fresh () in
       let simple = fresh () in
       let hex = fresh () in
       let consume_dollar = fresh () in
       let percent_raw = fresh () in
       load E.Rax current_byte;
       out (E.Cmp_imm8 (E.Rax, Char.code '\\'));
       jump Equal slash;
       out (E.Cmp_imm8 (E.Rax, Char.code '$'));
       jump Equal dollar;
       out (E.Cmp_imm8 (E.Rax, Char.code '%'));
       jump Equal percent;
       jump Always raw;
       mark slash;
       load E.Rax quote_peek;
       List.iter
         (fun (byte, decoded) ->
           let next = fresh () in
           out (E.Cmp_imm8 (E.Rax, byte));
           jump Not_equal next;
           constant quote_chunk0 (Int64.of_int decoded);
           increment current_offset;
           jump Always simple;
           mark next)
         [
           (Char.code '0', 0);
           (Char.code '\'', Char.code '\'');
           (Char.code '`', Char.code '`');
           (Char.code '"', Char.code '"');
           (Char.code '\\', Char.code '\\');
           (Char.code 'd', Char.code '$');
           (Char.code 'n', Char.code '\n');
           (Char.code 'r', Char.code '\r');
           (Char.code 't', Char.code '\t');
         ];
       out (E.Cmp_imm8 (E.Rax, Char.code 'x'));
       jump Equal hex;
       out (E.Cmp_imm8 (E.Rax, Char.code 'X'));
       jump Equal hex;
       load E.Rax current_byte;
       quote_single E.Rax;
       jump Always chunk_ready;
       mark simple;
       constant quote_chunk_length 1L;
       jump Always chunk_ready;
       mark hex;
       increment current_offset;
       zero quote_hex_value;
       zero quote_hex_digits;
       let hex_loop = fresh () in
       let hex_numeric = fresh () in
       let hex_upper = fresh () in
       let hex_lower = fresh () in
       let hex_valid = fresh () in
       let hex_finish = fresh () in
       mark hex_loop;
       read_byte ();
       store quote_peek E.Rax;
       out (E.Cmp_imm8 (E.Rax, Char.code '0'));
       jump Below hex_finish;
       out (E.Cmp_imm8 (E.Rax, Char.code ':'));
       jump Below hex_numeric;
       out (E.Cmp_imm8 (E.Rax, Char.code 'A'));
       jump Below hex_finish;
       out (E.Cmp_imm8 (E.Rax, Char.code 'G'));
       jump Below hex_upper;
       out (E.Cmp_imm8 (E.Rax, Char.code 'a'));
       jump Below hex_finish;
       out (E.Cmp_imm8 (E.Rax, Char.code 'g'));
       jump Below hex_lower;
       jump Always hex_finish;
       mark hex_numeric;
       out (E.Mov_imm64 (E.Rcx, 48L));
       out (E.Binary (E.Sub, E.Rax, E.Rcx));
       jump Always hex_valid;
       mark hex_upper;
       out (E.Mov_imm64 (E.Rcx, 55L));
       out (E.Binary (E.Sub, E.Rax, E.Rcx));
       jump Always hex_valid;
       mark hex_lower;
       out (E.Mov_imm64 (E.Rcx, 87L));
       out (E.Binary (E.Sub, E.Rax, E.Rcx));
       mark hex_valid;
       load E.Rcx quote_hex_value;
       out (E.Mov_imm64 (E.R8, 16L));
       out (E.Binary (E.Imul, E.Rcx, E.R8));
       out (E.Binary (E.Add, E.Rcx, E.Rax));
       store quote_hex_value E.Rcx;
       increment current_offset;
       increment quote_hex_digits;
       load E.Rax quote_hex_digits;
       out (E.Cmp_imm8 (E.Rax, 2));
       jump Not_equal hex_loop;
       mark hex_finish;
       load E.Rax quote_hex_value;
       quote_single E.Rax;
       jump Always chunk_ready;
       mark dollar;
       load E.Rax quote_peek;
       out (E.Cmp_imm8 (E.Rax, Char.code '$'));
       jump Equal consume_dollar;
       out (E.Mov_imm64 (E.Rax, Int64.of_int (Char.code '$')));
       quote_single E.Rax;
       jump Always chunk_ready;
       mark consume_dollar;
       increment current_offset;
       out (E.Mov_imm64 (E.Rax, Int64.of_int (Char.code '$')));
       quote_single E.Rax;
       jump Always chunk_ready;
       mark percent;
       test_flag flag_slash;
       jump Equal percent_raw;
       load E.Rax quote_peek;
       out (E.Cmp_imm8 (E.Rax, Char.code '%'));
       jump Not_equal percent_raw;
       increment current_offset;
       mark percent_raw;
       out (E.Mov_imm64 (E.Rax, Int64.of_int (Char.code '%')));
       quote_single E.Rax;
       jump Always chunk_ready;
       mark raw;
       load E.Rax current_byte;
       quote_single E.Rax;
       jump Always chunk_ready)
     else
       let dollar = fresh () in
       let percent = fresh () in
       let line_feed = fresh () in
       let carriage_return = fresh () in
       let tab = fresh () in
       let quoted = fresh () in
       let raw = fresh () in
       let control = fresh () in
       let dollar_plain = fresh () in
       let percent_plain = fresh () in
       load E.Rax current_byte;
       out (E.Cmp_imm8 (E.Rax, Char.code '$'));
       jump Equal dollar;
       out (E.Cmp_imm8 (E.Rax, Char.code '%'));
       jump Equal percent;
       out (E.Cmp_imm8 (E.Rax, Char.code '\n'));
       jump Equal line_feed;
       out (E.Cmp_imm8 (E.Rax, Char.code '\r'));
       jump Equal carriage_return;
       out (E.Cmp_imm8 (E.Rax, Char.code '\t'));
       jump Equal tab;
       out (E.Cmp_imm8 (E.Rax, Char.code '"'));
       jump Equal quoted;
       out (E.Cmp_imm8 (E.Rax, Char.code '\\'));
       jump Equal quoted;
       out (E.Cmp_imm8 (E.Rax, 0x1f));
       jump Below control;
       out (E.Cmp_imm8 (E.Rax, 0x7f));
       jump Equal control;
       jump Always raw;
       mark dollar;
       test_flag flag_dollar;
       jump Equal dollar_plain;
       quote_pair (Char.code '\\') (Char.code 'd');
       jump Always chunk_ready;
       mark dollar_plain;
       quote_pair (Char.code '$') (Char.code '$');
       jump Always chunk_ready;
       mark percent;
       test_flag flag_slash;
       jump Equal percent_plain;
       quote_pair (Char.code '%') (Char.code '%');
       jump Always chunk_ready;
       mark percent_plain;
       out (E.Mov_imm64 (E.Rax, Int64.of_int (Char.code '%')));
       quote_single E.Rax;
       jump Always chunk_ready;
       mark line_feed;
       quote_pair (Char.code '\\') (Char.code 'n');
       jump Always chunk_ready;
       mark carriage_return;
       quote_pair (Char.code '\\') (Char.code 'r');
       jump Always chunk_ready;
       mark tab;
       quote_pair (Char.code '\\') (Char.code 't');
       jump Always chunk_ready;
       mark quoted;
       constant quote_chunk0 (Int64.of_int (Char.code '\\'));
       load E.Rax current_byte;
       store quote_chunk1 E.Rax;
       constant quote_chunk_length 2L;
       jump Always chunk_ready;
       mark raw;
       load E.Rax current_byte;
       quote_single E.Rax;
       jump Always chunk_ready;
       mark control;
       constant quote_chunk0 (Int64.of_int (Char.code '\\'));
       constant quote_chunk1 (Int64.of_int (Char.code 'x'));
       load E.Rax current_byte;
       out (E.Mov_imm64 (E.Rcx, 4L));
       out (E.Shift_cl (E.Shr, E.Rax));
       out (E.Mov_imm64 (E.Rcx, 15L));
       out (E.Binary (E.And, E.Rax, E.Rcx));
       store_upper_hex_digit quote_chunk2;
       load E.Rax current_byte;
       out (E.Mov_imm64 (E.Rcx, 15L));
       out (E.Binary (E.And, E.Rax, E.Rcx));
       store_upper_hex_digit quote_chunk3;
       constant quote_chunk_length 4L;
       jump Always chunk_ready);
    mark chunk_ready;
    zero visit_count;
    mark chunk_loop;
    load E.Rax visit_count;
    load E.Rcx quote_chunk_length;
    out (E.Cmp (E.Rax, E.Rcx));
    jump Equal chunk_done;
    out (E.Mov (E.Rcx, E.Rax));
    for _ = 1 to 3 do
      out (E.Binary (E.Add, E.Rcx, E.Rcx))
    done;
    out (E.Address_stack (E.Rdx, stage quote_chunk0));
    out (E.Binary (E.Add, E.Rdx, E.Rcx));
    out (E.Load_indirect (E.Rax, E.Rdx, 0));
    load E.Rcx quote_phase;
    out (E.Test E.Rcx);
    jump Equal chunk_first;
    load E.Rcx copy_remaining;
    out (E.Test E.Rcx);
    jump Equal copy_done;
    out (E.Test E.Rax);
    jump Equal copy_done;
    append ();
    load E.Rax copy_remaining;
    out (E.Dec E.Rax);
    store copy_remaining E.Rax;
    out (E.Test E.Rax);
    jump Equal copy_done;
    jump Always chunk_next;
    mark chunk_first;
    load E.Rcx quote_frozen;
    out (E.Test E.Rcx);
    jump Not_equal chunk_next;
    out (E.Test E.Rax);
    let visible = fresh () in
    jump Not_equal visible;
    constant quote_frozen 1L;
    jump Always chunk_next;
    mark visible;
    increment payload_length;
    mark chunk_next;
    increment visit_count;
    jump Always chunk_loop;
    mark chunk_done;
    jump Always scan;
    mark source_done;
    load E.Rax quote_phase;
    out (E.Test E.Rax);
    jump Not_equal copy_done;
    emit_outstr_counts ();
    test_flag flag_left;
    let left = fresh () in
    jump Not_equal left;
    emit_repeat pad_remaining 32;
    mark left;
    load E.Rax copy_remaining;
    out (E.Test E.Rax);
    jump Equal copy_done;
    constant quote_phase 1L;
    zero current_offset;
    zero quote_frozen;
    jump Always scan;
    mark copy_done;
    test_flag flag_left;
    let complete = fresh () in
    jump Equal complete;
    emit_repeat pad_remaining 32;
    mark complete;
    jump Always done_label
  in
  let emit_grouped_zero_padding () =
    let done_ = fresh () in
    load E.Rax pad_remaining;
    out (E.Test E.Rax);
    jump Equal done_;
    out E.Zero_edx;
    load E.Rcx number_group;
    out (E.Mov_imm64 (E.R8, 1L));
    out (E.Binary (E.Add, E.Rcx, E.R8));
    out E.Div_rcx;
    out (E.Mov (E.R8, E.Rcx));
    load E.Rcx comma_count;
    out (E.Binary (E.Sub, E.R8, E.Rcx));
    out (E.Binary (E.Add, E.Rdx, E.R8));
    load E.Rcx number_group;
    out (E.Mov_imm64 (E.R8, 1L));
    out (E.Binary (E.Add, E.Rcx, E.R8));
    out (E.Cmp (E.Rdx, E.Rcx));
    let reduced = fresh () in
    jump Below reduced;
    out (E.Binary (E.Sub, E.Rdx, E.Rcx));
    mark reduced;
    out (E.Mov_imm64 (E.Rcx, 1L));
    out (E.Binary (E.Add, E.Rdx, E.Rcx));
    store comma_count E.Rdx;
    let loop = fresh () in
    let zero = fresh () in
    mark loop;
    load E.Rax pad_remaining;
    out (E.Test E.Rax);
    jump Equal done_;
    load E.Rax comma_count;
    out (E.Dec E.Rax);
    store comma_count E.Rax;
    out (E.Test E.Rax);
    jump Not_equal zero;
    out (E.Mov_imm64 (E.Rax, 44L));
    append ();
    copy_slot number_group comma_count;
    load E.Rax pad_remaining;
    out (E.Dec E.Rax);
    store pad_remaining E.Rax;
    out (E.Test E.Rax);
    jump Equal done_;
    mark zero;
    out (E.Mov_imm64 (E.Rax, 48L));
    append ();
    load E.Rax pad_remaining;
    out (E.Dec E.Rax);
    store pad_remaining E.Rax;
    jump Always loop;
    mark done_
  in
  let emit_number done_label =
    take_argument ~pointer:false;
    zero payload_length;
    zero negative;
    copy_slot number_group comma_count;
    let magnitude = fresh () in
    let digits_entry = fresh () in
    load E.Rax number_signed;
    out (E.Test E.Rax);
    jump Equal digits_entry;
    load E.Rax current_word;
    out (E.Test E.Rax);
    jump Less magnitude;
    jump Always digits_entry;
    mark magnitude;
    constant negative 1L;
    load E.Rax current_word;
    out (E.Unary (E.Neg, E.Rax));
    store current_word E.Rax;
    mark digits_entry;
    let digits = fresh () in
    let digits_done = fresh () in
    mark digits;
    load E.Rax current_word;
    out E.Zero_edx;
    load E.Rcx number_base;
    out E.Div_rcx;
    store current_word E.Rax;
    out (E.Mov (E.Rax, E.Rdx));
    out (E.Cmp_imm8 (E.Rax, 10));
    let numeric = fresh () in
    let encoded = fresh () in
    jump Below numeric;
    load E.Rcx number_alpha;
    out (E.Binary (E.Add, E.Rax, E.Rcx));
    jump Always encoded;
    mark numeric;
    out (E.Mov_imm64 (E.Rcx, 48L));
    out (E.Binary (E.Add, E.Rax, E.Rcx));
    mark encoded;
    push_number_byte E.Rax;
    load E.Rax current_word;
    out (E.Test E.Rax);
    jump Equal digits_done;
    test_flag flag_comma;
    let no_group = fresh () in
    jump Equal no_group;
    load E.Rax comma_count;
    out (E.Dec E.Rax);
    store comma_count E.Rax;
    out (E.Test E.Rax);
    jump Not_equal no_group;
    out (E.Mov_imm64 (E.Rax, 44L));
    push_number_byte E.Rax;
    copy_slot number_group comma_count;
    mark no_group;
    jump Always digits;
    mark digits_done;
    let width_nonnegative = fresh () in
    let negative_width = fresh () in
    load E.Rax field_width;
    out (E.Test E.Rax);
    jump Less negative_width;
    jump Always width_nonnegative;
    mark negative_width;
    zero field_width;
    mark width_nonnegative;
    let truncate_done = fresh () in
    test_flag flag_truncate;
    jump Equal truncate_done;
    load E.Rax payload_length;
    load E.Rcx negative;
    out (E.Binary (E.Add, E.Rax, E.Rcx));
    load E.Rdx field_width;
    out (E.Cmp (E.Rdx, E.Rax));
    let do_truncate = fresh () in
    jump Less do_truncate;
    jump Always truncate_done;
    mark do_truncate;
    load E.Rax field_width;
    load E.Rcx negative;
    out (E.Binary (E.Sub, E.Rax, E.Rcx));
    out (E.Test E.Rax);
    let store_truncated = fresh () in
    jump Less store_truncated;
    store payload_length E.Rax;
    jump Always truncate_done;
    mark store_truncated;
    zero payload_length;
    mark truncate_done;
    zero pad_remaining;
    load E.Rax payload_length;
    load E.Rcx negative;
    out (E.Binary (E.Add, E.Rax, E.Rcx));
    load E.Rdx field_width;
    out (E.Cmp (E.Rdx, E.Rax));
    let no_padding = fresh () in
    jump Less no_padding;
    jump Equal no_padding;
    out (E.Binary (E.Sub, E.Rdx, E.Rax));
    store pad_remaining E.Rdx;
    mark no_padding;
    let zero_padding = fresh () in
    let digits_output = fresh () in
    test_flag flag_zero;
    jump Not_equal zero_padding;
    emit_repeat pad_remaining 32;
    emit_sign ();
    jump Always digits_output;
    mark zero_padding;
    emit_sign ();
    test_flag flag_comma;
    let simple_zero_padding = fresh () in
    jump Equal simple_zero_padding;
    emit_grouped_zero_padding ();
    jump Always digits_output;
    mark simple_zero_padding;
    emit_repeat pad_remaining 48;
    mark digits_output;
    emit_buffer_reversed ();
    jump Always done_label
  in
  let parse_decimal_field slot =
    let loop = fresh () in
    let digit = fresh () in
    let done_ = fresh () in
    mark loop;
    out (E.Cmp_imm8 (E.Rax, 48));
    jump Below done_;
    out (E.Cmp_imm8 (E.Rax, 58));
    jump Below digit;
    jump Always done_;
    mark digit;
    load E.Rax slot;
    out (E.Mov_imm64 (E.Rcx, 10L));
    out (E.Binary (E.Imul, E.Rax, E.Rcx));
    jump Overflow format_fault;
    load E.Rcx current_byte;
    out (E.Mov_imm64 (E.R8, 48L));
    out (E.Binary (E.Sub, E.Rcx, E.R8));
    out (E.Binary (E.Add, E.Rax, E.Rcx));
    jump Overflow format_fault;
    store slot E.Rax;
    read_format ();
    jump Always loop;
    mark done_
  in
  let parse_modifiers () =
    let loop = fresh () in
    let comma = fresh () in
    let truncate = fresh () in
    let dollar = fresh () in
    let slash = fresh () in
    let ignored = fresh () in
    let done_ = fresh () in
    mark loop;
    out (E.Cmp_imm8 (E.Rax, 44));
    jump Equal comma;
    out (E.Cmp_imm8 (E.Rax, 116));
    jump Equal truncate;
    out (E.Cmp_imm8 (E.Rax, Char.code '$'));
    jump Equal dollar;
    out (E.Cmp_imm8 (E.Rax, Char.code '/'));
    jump Equal slash;
    out (E.Cmp_imm8 (E.Rax, Char.code 'l'));
    jump Equal ignored;
    jump Always done_;
    mark comma;
    set_flag flag_comma;
    read_format ();
    jump Always loop;
    mark truncate;
    set_flag flag_truncate;
    read_format ();
    jump Always loop;
    mark dollar;
    set_flag flag_dollar;
    read_format ();
    jump Always loop;
    mark slash;
    set_flag flag_slash;
    read_format ();
    jump Always loop;
    mark ignored;
    read_format ();
    jump Always loop;
    mark done_
  in
  out (E.Load_context (E.Rcx, 56));
  out (E.Test E.Rcx);
  jump Equal depth_fault;
  out (E.Load_context (E.Rcx, 48));
  out (E.Mov_imm64 (E.Rax, Int64.of_int call.activation_bytes));
  out (E.Cmp (E.Rcx, E.Rax));
  jump Below frame_fault;
  constant format_offset 0L;
  constant argument_index 0L;
  constant draft_length 0L;
  Array.iteri
    (fun index kind -> constant (fixed_scratch_slots + index) (kind_tag kind))
    call.argument_kinds;
  let format_loop = fresh () in
  let format_conversion = fresh () in
  let bare_decimal = fresh () in
  let bare_string = fresh () in
  let bare_packed = fresh () in
  let bare_u = fresh () in
  let bare_x = fresh () in
  let bare_X = fresh () in
  let bare_b = fresh () in
  let bare_B = fresh () in
  let percent = fresh () in
  let parse_spec = fresh () in
  let number_d = fresh () in
  let number_u = fresh () in
  let number_x = fresh () in
  let number_X = fresh () in
  let number_b = fresh () in
  let number_B = fresh () in
  let number_body = fresh () in
  let formatted_string = fresh () in
  let formatted_packed = fresh () in
  let formatted_upper_packed = fresh () in
  let quoted_Q = fresh () in
  let quoted_q = fresh () in
  let complete = fresh () in
  mark format_loop;
  read_format ();
  out (E.Test E.Rax);
  jump Equal complete;
  out (E.Cmp_imm8 (E.Rax, Char.code '%'));
  jump Equal format_conversion;
  append ();
  jump Always format_loop;
  mark format_conversion;
  read_format ();
  List.iter
    (fun (byte, label) ->
      out (E.Cmp_imm8 (E.Rax, Char.code byte));
      jump Equal label)
    [
      ('%', percent); ('d', bare_decimal); ('s', bare_string); ('c', bare_packed);
    ];
  List.iter
    (fun (byte, label) ->
      out (E.Cmp_imm8 (E.Rax, Char.code byte));
      jump Equal label)
    [
      ('u', bare_u); ('x', bare_x); ('X', bare_X); ('b', bare_b); ('B', bare_B);
    ];
  jump Always parse_spec;
  mark percent;
  out (E.Mov_imm64 (E.Rax, 37L));
  append ();
  jump Always format_loop;
  mark bare_string;
  emit_bare_string format_loop;
  mark bare_packed;
  constant format_flags 0L;
  emit_bare_packed format_loop;
  mark bare_decimal;
  constant field_width 0L;
  constant format_flags 0L;
  (* Preserve the established bare decimal path by using the same generic
     conversion with an empty field specification. Its work is still only the
     format reads plus attempted output appends. *)
  jump Always number_d;
  let emit_bare_number label target =
    mark label;
    constant field_width 0L;
    constant format_flags 0L;
    jump Always target
  in
  emit_bare_number bare_u number_u;
  emit_bare_number bare_x number_x;
  emit_bare_number bare_X number_X;
  emit_bare_number bare_b number_b;
  emit_bare_number bare_B number_B;
  mark parse_spec;
  constant field_width 0L;
  constant format_flags 0L;
  constant precision_value 0L;
  load E.Rax current_byte;
  let after_minus = fresh () in
  out (E.Cmp_imm8 (E.Rax, 45));
  jump Not_equal after_minus;
  set_flag flag_left;
  read_format ();
  mark after_minus;
  let after_zero = fresh () in
  out (E.Cmp_imm8 (E.Rax, 48));
  jump Not_equal after_zero;
  set_flag flag_zero;
  read_format ();
  mark after_zero;
  parse_decimal_field field_width;
  let after_width_star = fresh () in
  out (E.Cmp_imm8 (E.Rax, 42));
  jump Not_equal after_width_star;
  take_argument ~pointer:false;
  copy_slot current_word field_width;
  read_format ();
  mark after_width_star;
  let after_precision = fresh () in
  out (E.Cmp_imm8 (E.Rax, 46));
  jump Not_equal after_precision;
  constant precision_value 0L;
  read_format ();
  parse_decimal_field precision_value;
  let after_precision_star = fresh () in
  out (E.Cmp_imm8 (E.Rax, 42));
  jump Not_equal after_precision_star;
  take_argument ~pointer:false;
  copy_slot current_word precision_value;
  read_format ();
  mark after_precision_star;
  mark after_precision;
  parse_modifiers ();
  List.iter
    (fun (byte, label) ->
      out (E.Cmp_imm8 (E.Rax, Char.code byte));
      jump Equal label)
    [
      ('%', percent);
      ('d', number_d);
      ('u', number_u);
      ('x', number_x);
      ('X', number_X);
      ('b', number_b);
      ('B', number_B);
      ('s', formatted_string);
      ('c', formatted_packed);
      ('C', formatted_upper_packed);
      ('Q', quoted_Q);
      ('q', quoted_q);
    ];
  jump Always format_fault;
  let emit_number_setup label ~base ~group ~alpha ~signed =
    mark label;
    constant number_base (Int64.of_int base);
    constant number_group (Int64.of_int group);
    constant number_alpha (Int64.of_int alpha);
    constant number_signed (if signed then 1L else 0L);
    jump Always number_body
  in
  emit_number_setup number_d ~base:10 ~group:3 ~alpha:48 ~signed:true;
  emit_number_setup number_u ~base:10 ~group:3 ~alpha:48 ~signed:false;
  emit_number_setup number_x ~base:16 ~group:4
    ~alpha:(Char.code 'a' - 10)
    ~signed:false;
  emit_number_setup number_X ~base:16 ~group:4
    ~alpha:(Char.code 'A' - 10)
    ~signed:false;
  emit_number_setup number_b ~base:2 ~group:4 ~alpha:48 ~signed:false;
  emit_number_setup number_B ~base:2 ~group:4 ~alpha:48 ~signed:false;
  mark number_body;
  emit_number format_loop;
  mark formatted_string;
  emit_formatted_string format_loop;
  mark formatted_packed;
  emit_formatted_packed format_loop;
  mark formatted_upper_packed;
  set_flag flag_uppercase;
  emit_formatted_packed format_loop;
  mark quoted_Q;
  emit_quoted format_loop ~decode:false;
  mark quoted_q;
  emit_quoted format_loop ~decode:true;
  mark complete;
  load E.Rax draft_length;
  out (E.Load_context (E.Rcx, 88));
  out (E.Binary (E.Sub, E.Rcx, E.Rax));
  out (E.Store_context (88, E.Rcx));
  out (E.Load_context (E.Rcx, 104));
  out (E.Binary (E.Add, E.Rcx, E.Rax));
  out (E.Store_context (104, E.Rcx))
