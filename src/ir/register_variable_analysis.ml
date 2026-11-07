module Frame = Sema.Function_frame_layout
module Request = Sema.Register_request
module Seq = Instruction_sequence
module Values = Map.Make (Seq.Value_id)
module O = Opcode

type selection = Register_candidate | Stack | Unknown

type explicit_conflict = {
  register : int;
  previous : Sema.Symbol.t;
  replacement : Sema.Symbol.t;
  origin : Sema.Symbol.origin;
}

type snapshot = Seq.Block_id.t * Seq.description list * Seq.Block_id.t list

type payload_stamp =
  | No_payload
  | Integer of int64
  | Float_bits of int64
  | Block of int
  | Symbol of Sema.Symbol.t

type instruction_stamp = {
  id : int;
  opcode : O.t;
  operands : int list;
  result : int option;
  target : (Sema.Type.primitive_form * Sema.Primitive_type.t * int) option;
  payload : payload_stamp;
  flags : int64;
  span : (int * int * int) option;
}

let equal_stamp a b =
  a.id = b.id && a.opcode = b.opcode && a.operands = b.operands
  && a.result = b.result && a.target = b.target
  && (match (a.payload, b.payload) with
    | Symbol left, Symbol right -> left == right
    | No_payload, No_payload -> true
    | Integer left, Integer right -> left = right
    | Float_bits left, Float_bits right -> left = right
    | Block left, Block right -> left = right
    | _ -> false)
  && a.flags = b.flags && a.span = b.span

let instruction_stamp (item : Seq.description) =
  let target =
    match item.target_type with
    | None -> Some None
    | Some type_ -> (
        match Sema.Type.base type_ with
        | Sema.Type.Primitive (form, primitive) ->
            Some (Some (form, primitive, Sema.Type.pointer_depth type_))
        | Sema.Type.Aggregate _ -> None)
  in
  let payload =
    match item.payload with
    | None -> Some No_payload
    | Some (Seq.Integer value) -> Some (Integer value)
    | Some (Seq.Float_bits bits) -> Some (Float_bits bits)
    | Some (Seq.Block block) -> Some (Block (Seq.Block_id.to_int block))
    | Some (Seq.Symbol symbol) -> Some (Symbol symbol)
    | _ -> None
  in
  Option.bind target (fun target ->
      Option.map
        (fun payload ->
          {
            id = Seq.Instruction_id.to_int item.instruction_id;
            opcode = item.opcode;
            operands = List.map Seq.Value_id.to_int item.operands;
            result =
              Option.map
                (fun (result : Seq.value_definition) ->
                  Seq.Value_id.to_int result.value_id)
                item.result;
            target;
            payload;
            flags = item.flags;
            span =
              Option.map
                (fun (span : Common.Span.t) ->
                  (Common.Source_id.to_int span.source, span.start, span.stop))
                item.span;
          })
        payload)

type span_stamp = int * int * int

type origin_stamp =
  | Pinned_origin of string * int
  | Source_origin of
      span_stamp * span_stamp list * span_stamp option * span_stamp option
  | Synthesized_origin of string

type request_stamp =
  | Unspecified_request
  | Allocatable_request
  | Disabled_request
  | Explicit_request of string * int * origin_stamp

let copy_string value = Bytes.to_string (Bytes.of_string value)

let span_stamp (span : Common.Span.t) =
  (Common.Source_id.to_int span.source, span.start, span.stop)

let origin_stamp = function
  | Sema.Symbol.Pinned_source { path; line } ->
      Pinned_origin (copy_string path, line)
  | Source_location source ->
      Source_origin
        ( span_stamp source.span,
          List.map span_stamp source.source_segments,
          Option.map span_stamp source.generated_from,
          Option.map span_stamp source.defined_at )
  | Synthesized description -> Synthesized_origin (copy_string description)

(* Do not retain the explicit-register record as the request snapshot. It is
   shared by the frame and its semantic source, so an in-place change would
   otherwise rewrite both the request and the witness's expected value. *)
let request_stamp = function
  | Request.Unspecified -> Unspecified_request
  | Allocatable -> Allocatable_request
  | Disabled -> Disabled_request
  | Explicit register ->
      Explicit_request
        ( copy_string (Request.explicit_register_spelling register),
          Request.explicit_register_number register,
          origin_stamp (Request.explicit_register_origin register) )

(* Source requests have identities even when their effective selection is the
   immediate [Allocatable] or [Disabled] constructor. Seal those owners and
   primitive contents separately; retaining the mutable semantic objects alone
   would let a changed reservation or forced request rewrite its own witness. *)
type source_request_stamp = {
  owner : Request.t;
  kind : Request.kind;
  position : Request.position;
  spelling : string;
  origin : origin_stamp;
  explicit_owner : Request.explicit_register option;
  selection : request_stamp option;
}

type source_owner =
  | Parameter_owner of Sema.Function_type_resolution.parameter
  | Local_owner of Sema.Local_type_resolution.local

type source_requests_stamp = {
  owner : source_owner;
  requests : source_request_stamp list;
}

let source_request_stamp owner =
  {
    owner;
    kind = Request.kind owner;
    position = Request.position owner;
    spelling = copy_string (Request.spelling owner);
    origin = origin_stamp (Request.origin owner);
    explicit_owner = Request.explicit_register owner;
    (* Do not call [Request.effective]: malformed shared requests must revoke a
       witness, not raise its assertion for [Disable, Some _]. *)
    selection =
      (match (Request.kind owner, Request.explicit_register owner) with
      | Request.Allocate, None -> Some Allocatable_request
      | Disable, None -> Some Disabled_request
      | Allocate, Some register ->
          Some (request_stamp (Request.Explicit register))
      | Disable, Some _ -> None);
  }

let source_requests ~frame location =
  let module Headers = Sema.Function_type_resolution in
  let module Bindings = Sema.Function_binding_index in
  let module Locals = Sema.Local_type_resolution in
  let symbol = Frame.location_symbol location in
  let binding = Frame.location_binding location in
  if Bindings.binding_symbol binding != symbol then None
  else
    match Frame.location_kind location with
    | Frame.Named_parameter ->
        Option.bind (Bindings.binding_parameter_index binding) (fun index ->
            let header = Frame.function_header frame in
            header |> Headers.function_signature |> Headers.signature_parameters
            |> List.find_opt (fun parameter ->
                Headers.parameter_index parameter = index
                && Option.is_some (Headers.parameter_source parameter)
                && List.exists
                     (fun original ->
                       Headers.parameter_binding_index original = index
                       && Headers.parameter_binding_symbol original == symbol
                       && Option.equal ( == )
                            (Headers.parameter_binding_source original)
                            (Headers.parameter_source parameter))
                     (Headers.function_parameter_bindings header))
            |> Option.map (fun parameter ->
                {
                  owner = Parameter_owner parameter;
                  requests =
                    List.map source_request_stamp
                      (Headers.parameter_register_requests parameter);
                }))
    | Automatic_local ->
        Option.bind (Frame.location_local_source location) (fun local ->
            if
              Locals.local_symbol local != symbol
              || Bindings.binding_local_declaration_index binding
                 <> Some (Locals.local_declaration_index local)
              || Bindings.binding_local_declarator_index binding
                 <> Some (Locals.local_declarator_index local)
            then None
            else
              Some
                {
                  owner = Local_owner local;
                  requests =
                    List.map source_request_stamp
                      (Locals.local_register_requests local);
                })
    | Variadic_argc | Variadic_argv | Static_local -> None

let equal_source_requests a b =
  (match (a.owner, b.owner) with
    | Parameter_owner left, Parameter_owner right -> left == right
    | Local_owner left, Local_owner right -> left == right
    | _ -> false)
  && List.equal
       (fun (a : source_request_stamp) (b : source_request_stamp) ->
         a.owner == b.owner && a.kind = b.kind && a.position = b.position
         && a.spelling = b.spelling && a.origin = b.origin
         && Option.equal ( == ) a.explicit_owner b.explicit_owner
         && a.selection = b.selection)
       a.requests b.requests

type location_stamp = {
  location : Frame.location;
  symbol : Sema.Symbol.t;
  binding : Sema.Function_binding_index.binding;
  kind : Frame.location_kind;
  request : request_stamp;
  request_owner : Request.explicit_register option;
  source_requests : source_requests_stamp option;
  value_shape : Frame.value_shape;
  declarator_shape : Frame.declarator_shape;
  dimensions : (Frame.dimension_kind * int64) list;
  source_dimensions_checked : bool;
  name : string;
  element_size : int64;
  alignment : int;
  checked_type : Sema.Type.primitive_form * Sema.Primitive_type.t * int;
  slot : (Frame.frame_slot * int64 * int64) option;
  allocated_size : int64;
}

let location_stamp ~frame location =
  match Sema.Type.base (Frame.location_checked_type location) with
  | Sema.Type.Aggregate _ -> None
  | Sema.Type.Primitive (form, primitive) ->
      Some
        {
          location;
          symbol = Frame.location_symbol location;
          binding = Frame.location_binding location;
          kind = Frame.location_kind location;
          request = request_stamp (Frame.location_register_selection location);
          request_owner =
            (match Frame.location_register_selection location with
            | Request.Explicit register -> Some register
            | Unspecified | Allocatable | Disabled -> None);
          source_requests = source_requests ~frame location;
          value_shape = Frame.location_value_shape location;
          declarator_shape = Frame.location_declarator_shape location;
          dimensions =
            List.map
              (fun dimension ->
                (Frame.dimension_kind dimension, Frame.dimension_value dimension))
              (Frame.location_dimensions location);
          source_dimensions_checked =
            Frame.location_source_dimensions_checked location;
          name = copy_string (Sema.Symbol.name (Frame.location_symbol location));
          element_size = Frame.location_element_size location;
          alignment = Frame.location_alignment location;
          checked_type =
            ( form,
              primitive,
              Sema.Type.pointer_depth (Frame.location_checked_type location) );
          slot =
            Option.map
              (fun slot ->
                ( slot,
                  Frame.frame_slot_displacement slot,
                  Frame.frame_slot_size slot ))
              (Frame.location_frame_slot location);
          allocated_size = Frame.location_allocated_size location;
        }

let equal_location a b =
  a.location == b.location && a.symbol == b.symbol && a.binding == b.binding
  && a.kind = b.kind && a.request = b.request
  && Option.equal ( == ) a.request_owner b.request_owner
  && Option.equal equal_source_requests a.source_requests b.source_requests
  && a.value_shape = b.value_shape
  && a.declarator_shape = b.declarator_shape
  && a.dimensions = b.dimensions
  && a.source_dimensions_checked = b.source_dimensions_checked
  && a.name = b.name
  && a.element_size = b.element_size
  && a.alignment = b.alignment
  && a.checked_type = b.checked_type
  && a.allocated_size = b.allocated_size
  && Option.equal
       (fun (slot, offset, size) (other_slot, other_offset, other_size) ->
         slot == other_slot && offset = other_offset && size = other_size)
       a.slot b.slot

let signature_parameters frame =
  Frame.function_header frame
  |> Sema.Function_type_resolution.function_signature
  |> Sema.Function_type_resolution.signature_parameters

let source_parameters frame =
  Frame.function_header frame
  |> Sema.Function_type_resolution.function_source_parameters

type t = {
  frame_ : Frame.function_layout;
  parameters_ : Sema.Function_type_resolution.parameter list;
  parameter_sources_ : Frontend.Ast.function_parameter list option;
  body_ : Function_body.t;
  definition_ : Sema.Function_resolution.resolved_declaration option;
  source_ : Sema.Function_call_expression_result.resolved_function option;
  options_ : int64;
  flags_ : int64;
  locations_ : location_stamp list;
  blocks_ : snapshot list;
  stamps_ : (Seq.description * instruction_stamp) list;
  selections_ : (Sema.Symbol.t * selection) list;
  exact_registers_ : (Sema.Symbol.t * int) list option;
  shadowed_explicit_ : (Sema.Symbol.t * int) list;
  explicit_conflicts_ : explicit_conflict list;
  exact_source_ : Exact_register_scores.source_stamp option;
  runtime_ : Runtime_call_context.t option;
}

let descriptions block =
  Block_graph.instructions block |> Seq.instructions |> List.map Seq.description

let snapshot body =
  Function_body.body body |> Block_graph.blocks
  |> List.map (fun block ->
      ( Block_graph.block_id block,
        descriptions block,
        List.map Fun.id (Block_graph.successors block) ))

let same_list equal a b =
  List.length a = List.length b && List.for_all2 equal a b

let matches_blocks expected actual =
  same_list
    (fun (id, instructions, successors)
         (other_id, other_instructions, other_successors) ->
      Seq.Block_id.equal id other_id
      && same_list ( == ) instructions other_instructions
      && same_list Seq.Block_id.equal successors other_successors)
    expected actual

(* KernelA.HH defines REGG_LOCAL_VARS (RSI, RDI, R10, R11, R14, R15)
   and REGG_LOCAL_NON_PTR_VARS (R12, R13). These are variable registers,
   distinct from fixed instruction, stack temporary and frame registers. *)
let general_variable_registers = [ 6; 7; 10; 11; 14; 15 ]
let nonpointer_variable_registers = [ 12; 13 ]

let variable_registers =
  general_variable_registers @ nonpointer_variable_registers

let supported_register_requests locations =
  List.for_all
    (fun location ->
      match Frame.location_register_selection location with
      | Request.Explicit register ->
          let number = Request.explicit_register_number register in
          List.mem number variable_registers
          && Request.canonical_u64_register_number
               (Request.explicit_register_spelling register)
             = Some number
      | Unspecified | Allocatable | Disabled -> true)
    locations

let safe_register_requests locations =
  let rec check used = function
    | [] -> true
    | location :: rest -> (
        match Frame.location_register_selection location with
        | Request.Explicit register ->
            let number = Request.explicit_register_number register in
            List.mem number variable_registers
            && Request.canonical_u64_register_number
                 (Request.explicit_register_spelling register)
               = Some number
            && (not (List.mem number used))
            && check (number :: used) rest
        | Unspecified | Allocatable | Disabled -> check used rest)
  in
  check [] locations

let registers_disabled body =
  Sema.Compiler_option.is_enabled
    ~mask:(Function_body.compiler_options body)
    Sema.Compiler_option.No_reg_var

let supported_options body =
  let module Options = Sema.Compiler_option in
  let allowed_changes =
    List.fold_left
      (fun mask option -> Int64.logor mask (Options.mask option))
      0L
      [
        Options.Warn_unused_var;
        Warn_paren;
        Warn_dup_types;
        Warn_header_mismatch;
        No_reg_var;
        (* Lex/trace output and symbol retention do not alter OptPass3's
           candidate membership, scores or banks. Keep their original snapshots. *)
        Echo;
        Trace;
        Keep_private;
      ]
  in
  let actual = Function_body.compiler_options body in
  Int64.logand
    (Int64.logxor actual Options.initial_mask)
    (Int64.lognot allowed_changes)
  = 0L
  && Option.fold ~none:false ~some:(Int64.equal actual)
       (Function_body.definition_compiler_options body)

let f64_type type_ =
  Sema.Type.pointer_depth type_ = 0
  &&
  match Sema.Type.base type_ with
  | Sema.Type.Primitive (_, Sema.Primitive_type.F64) -> true
  | _ -> false

let automatic_candidate location =
  match Frame.location_register_selection location with
  | Request.Allocatable -> true
  | Unspecified -> not (f64_type (Frame.location_checked_type location))
  | Disabled | Explicit _ -> false

let stack_selected ~body location =
  match Frame.location_register_selection location with
  | Request.Disabled -> true
  | Request.Explicit _ -> false
  | Request.Unspecified ->
      f64_type (Frame.location_checked_type location) || registers_disabled body
  | Request.Allocatable -> registers_disabled body

let analyze_parameter ~frame ~body =
  let locations = Frame.function_locations frame in
  let ordinary =
    Option.is_some (Function_body.definition_declaration body)
    && Function_body.definition_matches_frame body frame
    && Int64.logand
         (Function_body.stored_flags body)
         (Int64.lognot Function_body.ordinary_calling_flag_mask)
       = 0L
    && supported_options body
    && List.length locations >= 2
    && List.length locations <= 6
    && safe_register_requests locations
    && List.for_all
         (fun location ->
           Frame.location_kind location = Frame.Named_parameter
           && Frame.location_value_shape location = Frame.Scalar
           && Option.is_some
                (Integer_scalar_storage.of_type
                   (Frame.location_checked_type location))
           && Option.is_some (Frame.location_frame_slot location))
         locations
  in
  if not ordinary then None
  else
    let blocks = snapshot body in
    let code =
      List.concat_map (fun (_, instructions, _) -> instructions) blocks
    in
    let allowed (item : Seq.description) =
      let flags =
        if item.opcode = O.Ic_end_exp then item.flags = 0x200L
        else item.flags = 0L
      in
      flags
      && Option.is_some (instruction_stamp item)
      &&
      match item.opcode with
      | O.Ic_rbp
      | Ic_imm_i64
      | Ic_add
      | Ic_deref
      | Ic_shl_equ
      | Ic_shr_equ
      | Ic_add_equ
      | Ic_sub_equ
      | Ic_mul_equ
      | Ic_div_equ
      | Ic_mod_equ
      | Ic_and_equ
      | Ic_or_equ
      | Ic_xor_equ
      | Ic_end_exp
      | Ic_return_val
      | Ic_jmp
      | Ic_ret -> true
      | _ -> false
    in
    if List.length code > 96 || not (List.for_all allowed code) then None
    else
      let produced =
        List.fold_left
          (fun map (item : Seq.description) ->
            match item.result with
            | None -> map
            | Some result -> Values.add result.value_id item map)
          Values.empty code
      in
      let users id =
        List.filter
          (fun (item : Seq.description) ->
            List.exists (Seq.Value_id.equal id) item.operands)
          code
      in
      let location_at offset =
        List.find_opt
          (fun location ->
            Option.fold ~none:false
              ~some:(fun slot -> Frame.frame_slot_displacement slot = offset)
              (Frame.location_frame_slot location))
          locations
      in
      let frame_address (item : Seq.description) =
        match (item.opcode, item.operands, item.result) with
        | O.Ic_add, [ left; right ], Some result ->
            let is_rbp id =
              Option.fold ~none:false
                ~some:(fun (d : Seq.description) -> d.opcode = O.Ic_rbp)
                (Values.find_opt id produced)
            in
            let offset id =
              Option.bind (Values.find_opt id produced)
                (fun (d : Seq.description) ->
                  match (d.opcode, d.payload) with
                  | O.Ic_imm_i64, Some (Seq.Integer n) -> Some n
                  | _ -> None)
            in
            let value =
              if is_rbp left then offset right
              else if is_rbp right then offset left
              else None
            in
            Option.bind value (fun n ->
                Option.bind (location_at n) (fun location ->
                    Option.bind item.target_type (fun pointer ->
                        match Sema.Type.dereference pointer with
                        | Ok actual
                          when Sema.Type.equal actual
                                 (Frame.location_checked_type location) ->
                            Some (result.value_id, location)
                        | _ -> None)))
        | _ -> None
      in
      let additions =
        List.filter (fun (d : Seq.description) -> d.opcode = O.Ic_add) code
      in
      let addresses = List.filter_map frame_address additions in
      let arithmetic_additions =
        List.filter (fun item -> Option.is_none (frame_address item)) additions
      in
      let safe_use id location (item : Seq.description) =
        let same_type =
          Option.fold ~none:false
            ~some:(Sema.Type.equal (Frame.location_checked_type location))
            item.target_type
        in
        same_type
        &&
        match (item.opcode, item.operands) with
        | O.Ic_deref, [ target ] -> Seq.Value_id.equal target id
        | ( ( Ic_shl_equ
            | Ic_shr_equ
            | Ic_add_equ
            | Ic_sub_equ
            | Ic_mul_equ
            | Ic_div_equ
            | Ic_mod_equ
            | Ic_and_equ
            | Ic_or_equ
            | Ic_xor_equ ),
            [ target; _ ] ) -> Seq.Value_id.equal target id
        | _ -> false
      in
      let addresses_safe =
        List.for_all
          (fun (id, location) ->
            let consumers = users id in
            consumers <> [] && List.for_all (safe_use id location) consumers)
          addresses
      in
      let rbp_safe =
        List.for_all
          (fun (item : Seq.description) ->
            if item.opcode <> O.Ic_rbp then true
            else
              match item.result with
              | None -> false
              | Some result ->
                  let consumers = users result.value_id in
                  consumers <> []
                  && List.for_all
                       (fun (d : Seq.description) ->
                         d.opcode = O.Ic_add && Option.is_some (frame_address d))
                       consumers)
          code
      in
      let dereferences_safe =
        List.for_all
          (fun (item : Seq.description) ->
            match (item.opcode, item.operands) with
            | O.Ic_deref, [ target ]
            | ( ( Ic_shl_equ
                | Ic_shr_equ
                | Ic_add_equ
                | Ic_sub_equ
                | Ic_mul_equ
                | Ic_div_equ
                | Ic_mod_equ
                | Ic_and_equ
                | Ic_or_equ
                | Ic_xor_equ ),
                [ target; _ ] ) ->
                List.exists
                  (fun (id, _) -> Seq.Value_id.equal id target)
                  addresses
            | _ -> true)
          code
      in
      (* The checked pattern lives wholly in the entry block. A later or
         unreachable update must not be mistaken for the source optimizer's
         live register-variable use. *)
      let entry_id =
        Block_graph.block_id (Block_graph.entry (Function_body.body body))
      in
      let simple_flow =
        List.for_all
          (fun (id, instructions, _) ->
            Seq.Block_id.equal id entry_id
            || List.for_all
                 (fun (item : Seq.description) ->
                   item.opcode = O.Ic_jmp || item.opcode = O.Ic_ret)
                 instructions)
          blocks
      in
      let matching_target =
        let address_location id =
          List.find_map
            (fun (original, location) ->
              if Seq.Value_id.equal original id then Some location else None)
            addresses
        in
        let loaded_location id =
          Option.bind (Values.find_opt id produced)
            (fun (item : Seq.description) ->
              match (item.opcode, item.operands) with
              | O.Ic_deref, [ address ] -> address_location address
              | _ -> None)
        in
        let updates =
          List.filter
            (fun (item : Seq.description) ->
              item.opcode = O.Ic_shl_equ || item.opcode = O.Ic_shr_equ
              || item.opcode = O.Ic_add_equ || item.opcode = O.Ic_sub_equ
              || item.opcode = O.Ic_mul_equ || item.opcode = O.Ic_div_equ
              || item.opcode = O.Ic_mod_equ || item.opcode = O.Ic_and_equ
              || item.opcode = O.Ic_or_equ || item.opcode = O.Ic_xor_equ)
            code
        in
        let returns =
          List.filter
            (fun (item : Seq.description) -> item.opcode = O.Ic_return_val)
            code
        in
        match (updates, returns) with
        | [ update ], [ returned ] -> (
            match (update.operands, returned.operands) with
            | [ target; count ], [ result ] -> (
                match
                  ( address_location target,
                    loaded_location count,
                    loaded_location result )
                with
                | ( Some target_location,
                    Some count_location,
                    Some return_location )
                  when target_location == return_location
                       && target_location != count_location
                       && Seq.Instruction_id.compare update.instruction_id
                            returned.instruction_id
                          < 0 -> (
                    match
                      Sema.Type.base
                        (Frame.location_checked_type target_location)
                    with
                    | Sema.Type.Primitive
                        ( _,
                          (Sema.Primitive_type.I8 | U8 | I16 | U16 | I32 | U32)
                        ) -> Some (target_location, count_location)
                    | _ -> None)
                | _ -> None)
            | _ -> None)
        | _ -> None
      in
      (* For two updates, authenticate the only non-address addition as the
         return sum and tie each operand to its own frame location. *)
      let matching_pair =
        let integer_shape type_ =
          if Sema.Type.pointer_depth type_ <> 0 then None
          else
            match Sema.Type.base type_ with
            | Sema.Type.Primitive (form, (Sema.Primitive_type.I8 | U8)) ->
                Some (form, 1)
            | Sema.Type.Primitive (form, (Sema.Primitive_type.I16 | U16)) ->
                Some (form, 2)
            | Sema.Type.Primitive (form, (Sema.Primitive_type.I32 | U32)) ->
                Some (form, 4)
            | Sema.Type.Primitive (form, (Sema.Primitive_type.I64 | U64)) ->
                Some (form, 8)
            | _ -> None
        in
        let compatible_sum first second result =
          match
            (integer_shape first, integer_shape second, integer_shape result)
          with
          | ( Some (first_form, first_width),
              Some (second_form, second_width),
              Some (result_form, result_width) ) ->
              first_form = second_form && first_form = result_form
              && min first_width second_width < 8
              && result_width = max first_width second_width
          | _ -> false
        in
        let supported_update = function
          | O.Ic_add_equ
          | O.Ic_sub_equ
          | O.Ic_mul_equ
          | O.Ic_div_equ
          | O.Ic_mod_equ
          | O.Ic_shl_equ
          | O.Ic_shr_equ
          | O.Ic_and_equ
          | O.Ic_or_equ
          | O.Ic_xor_equ -> true
          | _ -> false
        in
        let address_location id =
          List.find_map
            (fun (original, location) ->
              if Seq.Value_id.equal original id then Some location else None)
            addresses
        in
        let loaded_location id =
          Option.bind (Values.find_opt id produced)
            (fun (item : Seq.description) ->
              match (item.opcode, item.operands) with
              | O.Ic_deref, [ address ] -> address_location address
              | _ -> None)
        in
        let updates =
          List.filter
            (fun (item : Seq.description) ->
              item.opcode = O.Ic_add_equ || item.opcode = O.Ic_sub_equ
              || item.opcode = O.Ic_mul_equ || item.opcode = O.Ic_div_equ
              || item.opcode = O.Ic_mod_equ || item.opcode = O.Ic_shl_equ
              || item.opcode = O.Ic_shr_equ || item.opcode = O.Ic_and_equ
              || item.opcode = O.Ic_or_equ || item.opcode = O.Ic_xor_equ)
            code
        in
        let returns =
          List.filter
            (fun (item : Seq.description) -> item.opcode = O.Ic_return_val)
            code
        in
        match (updates, returns, arithmetic_additions) with
        | [ first; second ], [ returned ], [ sum ]
          when supported_update first.opcode
               && supported_update second.opcode
               && first.payload = None && second.payload = None
               && sum.payload = None && returned.payload = None
               && Seq.Instruction_id.compare first.instruction_id
                    second.instruction_id
                  < 0
               && Seq.Instruction_id.compare second.instruction_id
                    sum.instruction_id
                  < 0
               && Seq.Instruction_id.compare sum.instruction_id
                    returned.instruction_id
                  < 0 -> (
            match
              ( first.operands,
                second.operands,
                sum.operands,
                sum.result,
                returned.operands )
            with
            | ( [ first_address; first_count ],
                [ second_address; second_count ],
                [ left; right ],
                Some sum_result,
                [ returned_value ] )
              when Seq.Value_id.equal sum_result.value_id returned_value
                   && List.for_all
                        (fun value ->
                          Option.fold ~none:false
                            ~some:(fun (item : Seq.description) ->
                              Seq.Instruction_id.compare second.instruction_id
                                item.instruction_id
                              < 0)
                            (Values.find_opt value produced))
                        [ left; right ] -> (
                match
                  ( address_location first_address,
                    address_location second_address,
                    loaded_location first_count,
                    loaded_location second_count,
                    loaded_location left,
                    loaded_location right,
                    sum.target_type )
                with
                | ( Some first_target,
                    Some second_target,
                    Some count,
                    Some other_count,
                    Some left_target,
                    Some right_target,
                    Some sum_type )
                  when first_target != second_target
                       && first_target != count && second_target != count
                       && first_target != other_count
                       && second_target != other_count
                       && left_target == first_target
                       && right_target == second_target
                       && compatible_sum
                            (Frame.location_checked_type first_target)
                            (Frame.location_checked_type second_target)
                            sum_type ->
                    Some
                      ( first_target,
                        second_target,
                        count,
                        other_count,
                        sum_type,
                        first.opcode,
                        second.opcode )
                | _ -> None)
            | _ -> None)
        | _ -> None
      in
      let source_matches (target, count_location) =
        let module Typed = Sema.Function_call_expression_result in
        let module Calls = Sema.Function_call_resolution in
        let module Bind = Sema.Module_expression_binding in
        let binding_of value =
          match Calls.argument_expression_kind (Typed.result_source value) with
          | Calls.Bound_identifier_expression bound -> (
              match
                Bind.occurrence_resolution
                  (Calls.bound_identifier_occurrence bound)
              with
              | Bind.Local_binding binding -> Some binding
              | _ -> None)
          | _ -> None
        in
        Option.fold ~none:false
          ~some:(fun source ->
            match
              ( Typed.function_expression_statements source,
                Typed.function_returns source )
            with
            | [ statement ], [ returned ] -> (
                let value = Typed.expression_statement_value statement in
                match
                  ( Calls.argument_expression_source (Typed.result_source value),
                    Typed.result_binary_operands value,
                    Typed.return_value returned,
                    List.filter
                      (fun (item : Seq.description) ->
                        item.opcode = O.Ic_shl_equ || item.opcode = O.Ic_shr_equ
                        || item.opcode = O.Ic_add_equ
                        || item.opcode = O.Ic_sub_equ
                        || item.opcode = O.Ic_mul_equ
                        || item.opcode = O.Ic_div_equ
                        || item.opcode = O.Ic_mod_equ
                        || item.opcode = O.Ic_and_equ
                        || item.opcode = O.Ic_or_equ
                        || item.opcode = O.Ic_xor_equ)
                      code )
                with
                | ( Some (Frontend.Ast.Binary_expression binary),
                    Some (left, right),
                    Some return_value,
                    [ update ] ) ->
                    let opcode_matches =
                      match
                        (binary.binary_operator_spec.spelling, update.opcode)
                      with
                      | "<<=", O.Ic_shl_equ
                      | ">>=", Ic_shr_equ
                      | "+=", Ic_add_equ
                      | "-=", Ic_sub_equ
                      | "*=", Ic_mul_equ
                      | "/=", Ic_div_equ
                      | "%=", Ic_mod_equ
                      | "&=", Ic_and_equ
                      | "|=", Ic_or_equ
                      | "^=", Ic_xor_equ -> true
                      | _ -> false
                    in
                    let target_binding = Frame.location_binding target in
                    opcode_matches
                    && Option.fold ~none:false ~some:(( == ) target_binding)
                         (binding_of left)
                    && Option.fold ~none:false ~some:(( == ) target_binding)
                         (binding_of return_value)
                    && Option.fold ~none:false
                         ~some:(( == ) (Frame.location_binding count_location))
                         (binding_of right)
                | _ -> false)
            | _ -> false)
          (Function_body.definition_source body)
      in
      let pair_source_matches
          ( first_target,
            second_target,
            first_count,
            second_count,
            sum_type,
            first_opcode,
            second_opcode ) =
        let module Typed = Sema.Function_call_expression_result in
        let module Calls = Sema.Function_call_resolution in
        let module Bind = Sema.Module_expression_binding in
        let binding_of value =
          match Calls.argument_expression_kind (Typed.result_source value) with
          | Calls.Bound_identifier_expression bound -> (
              match
                Bind.occurrence_resolution
                  (Calls.bound_identifier_occurrence bound)
              with
              | Bind.Local_binding binding -> Some binding
              | _ -> None)
          | _ -> None
        in
        let matches_binding location value =
          Option.fold ~none:false
            ~some:(( == ) (Frame.location_binding location))
            (binding_of value)
        in
        let update_matches target count opcode statement =
          let value = Typed.expression_statement_value statement in
          match
            ( Calls.argument_expression_source (Typed.result_source value),
              Typed.result_binary_operands value )
          with
          | Some (Frontend.Ast.Binary_expression binary), Some (left, right) ->
              (match (binary.binary_operator_spec.spelling, opcode) with
                | "+=", O.Ic_add_equ
                | "-=", O.Ic_sub_equ
                | "*=", O.Ic_mul_equ
                | "/=", O.Ic_div_equ
                | "%=", O.Ic_mod_equ
                | "<<=", O.Ic_shl_equ
                | ">>=", O.Ic_shr_equ
                | "&=", O.Ic_and_equ
                | "|=", O.Ic_or_equ
                | "^=", O.Ic_xor_equ -> true
                | _ -> false)
              && matches_binding target left
              && matches_binding count right
          | _ -> false
        in
        Option.fold ~none:false
          ~some:(fun source ->
            match
              ( Typed.function_expression_statements source,
                Typed.function_returns source )
            with
            | [ first; second ], [ returned ] -> (
                match Typed.return_value returned with
                | Some value -> (
                    match
                      ( Calls.argument_expression_source
                          (Typed.result_source value),
                        Typed.result_binary_operands value )
                    with
                    | ( Some (Frontend.Ast.Binary_expression binary),
                        Some (left, right) ) ->
                        binary.binary_operator_spec.spelling = "+"
                        && Option.fold ~none:false
                             ~some:(Sema.Type.equal sum_type)
                             (Typed.result_type value)
                        && update_matches first_target first_count first_opcode
                             first
                        && update_matches second_target second_count
                             second_opcode second
                        && matches_binding first_target left
                        && matches_binding second_target right
                    | _ -> false)
                | None -> false)
            | _ -> false)
          (Function_body.definition_source body)
      in
      (* Match the original ordered updates, including repeated destinations.
         Operands must remain unchanged and returns must read updated members. *)
      let matching_sequence =
        let address_location id =
          List.find_map
            (fun (original, location) ->
              if Seq.Value_id.equal original id then Some location else None)
            addresses
        in
        let loaded_location id =
          Option.bind (Values.find_opt id produced)
            (fun (item : Seq.description) ->
              match (item.opcode, item.operands) with
              | O.Ic_deref, [ address ] -> address_location address
              | _ -> None)
        in
        let updates =
          List.filter
            (fun (item : Seq.description) ->
              match item.opcode with
              | O.Ic_shl_equ
              | Ic_shr_equ
              | Ic_add_equ
              | Ic_sub_equ
              | Ic_mul_equ
              | Ic_div_equ
              | Ic_mod_equ
              | Ic_and_equ
              | Ic_or_equ
              | Ic_xor_equ -> true
              | _ -> false)
            code
        in
        let returns =
          List.filter
            (fun (item : Seq.description) -> item.opcode = O.Ic_return_val)
            code
        in
        let count = List.length updates in
        let rec ordered = function
          | first :: (second :: _ as rest) ->
              Seq.Instruction_id.compare first.Seq.instruction_id
                second.Seq.instruction_id
              < 0
              && ordered rest
          | _ -> true
        in
        match (updates, returns) with
        | (_ :: _ as updates), [ returned ]
          when count <= 5
               && (count >= 3 || (count = 2 && arithmetic_additions = []))
               && List.length arithmetic_additions <= 5
               && List.for_all
                    (fun (item : Seq.description) -> item.payload = None)
                    updates
               && returned.payload = None && ordered updates -> (
            let last = List.hd (List.rev updates) in
            let mapped =
              List.map
                (fun (item : Seq.description) ->
                  match item.operands with
                  | [ address; operand ] ->
                      Option.bind (address_location address) (fun target ->
                          Option.map
                            (fun operand -> (target, operand))
                            (loaded_location operand))
                  | _ -> None)
                updates
            in
            if List.exists Option.is_none mapped then None
            else
              let pairs = List.map Option.get mapped in
              let targets = List.map fst pairs in
              let operands = List.map snd pairs in
              let integer_shape type_ =
                if Sema.Type.pointer_depth type_ <> 0 then None
                else
                  match Sema.Type.base type_ with
                  | Sema.Type.Primitive (form, (Sema.Primitive_type.I8 | U8)) ->
                      Some (form, 1)
                  | Sema.Type.Primitive (form, (Sema.Primitive_type.I16 | U16))
                    -> Some (form, 2)
                  | Sema.Type.Primitive (form, (Sema.Primitive_type.I32 | U32))
                    -> Some (form, 4)
                  | Sema.Type.Primitive (form, (Sema.Primitive_type.I64 | U64))
                    -> Some (form, 8)
                  | _ -> None
              in
              let compatible_sum left right result =
                match
                  (integer_shape left, integer_shape right, integer_shape result)
                with
                | Some (lf, lw), Some (rf, rw), Some (sf, sw) ->
                    lf = rf && lf = sf && sw = max lw rw
                | _ -> false
              in
              let supported_targets =
                List.for_all
                  (fun target ->
                    Option.is_some
                      (integer_shape (Frame.location_checked_type target)))
                  targets
                && List.exists
                     (fun target ->
                       Option.fold ~none:false
                         ~some:(fun (_, width) -> width < 8)
                         (integer_shape (Frame.location_checked_type target)))
                     targets
              in
              if
                (not supported_targets)
                || List.exists
                     (fun target -> List.exists (( == ) target) operands)
                     targets
                || Seq.Instruction_id.compare last.instruction_id
                     returned.instruction_id
                   >= 0
              then None
              else
                let rec collect seen value =
                  if List.exists (Seq.Value_id.equal value) seen then None
                  else
                    Option.bind (Values.find_opt value produced)
                      (fun (item : Seq.description) ->
                        if
                          Seq.Instruction_id.compare last.instruction_id
                            item.instruction_id
                          >= 0
                          || Seq.Instruction_id.compare item.instruction_id
                               returned.instruction_id
                             >= 0
                        then None
                        else
                          match (item.opcode, item.operands) with
                          | O.Ic_deref, [ address ] ->
                              Option.map
                                (fun location ->
                                  ( [ location ],
                                    [],
                                    Frame.location_checked_type location ))
                                (address_location address)
                          | O.Ic_add, [ left; right ] when item.payload = None
                            ->
                              Option.bind
                                (collect (value :: seen) left)
                                (fun (left_locations, left_sums, left_type) ->
                                  Option.bind
                                    (collect (value :: seen) right)
                                    (fun ( right_locations,
                                           right_sums,
                                           right_type )
                                       ->
                                      Option.bind item.target_type
                                        (fun result_type ->
                                          if
                                            compatible_sum left_type right_type
                                              result_type
                                          then
                                            Some
                                              ( left_locations @ right_locations,
                                                item :: (left_sums @ right_sums),
                                                result_type )
                                          else None)))
                          | _ -> None)
                in
                match returned.operands with
                | [ root ] -> (
                    match collect [] root with
                    | Some (leaves, sums, _)
                      when List.length leaves >= 1
                           && List.length leaves <= 6
                           && List.for_all
                                (fun leaf -> List.exists (( == ) leaf) targets)
                                leaves
                           && List.length sums
                              = List.length arithmetic_additions
                           && List.length
                                (List.sort_uniq Int.compare
                                   (List.map
                                      (fun (item : Seq.description) ->
                                        Seq.Instruction_id.to_int
                                          item.instruction_id)
                                      sums))
                              = List.length sums
                           && List.for_all
                                (fun sum ->
                                  List.exists (( == ) sum) arithmetic_additions)
                                sums ->
                        Some
                          ( targets,
                            operands,
                            root,
                            List.map
                              (fun (item : Seq.description) -> item.opcode)
                              updates )
                    | _ -> None)
                | _ -> None)
        | _ -> None
      in
      let sequence_source_matches (targets, operands, root, opcodes) =
        let module Typed = Sema.Function_call_expression_result in
        let module Calls = Sema.Function_call_resolution in
        let module Bind = Sema.Module_expression_binding in
        let binding_of value =
          match Calls.argument_expression_kind (Typed.result_source value) with
          | Calls.Bound_identifier_expression bound -> (
              match
                Bind.occurrence_resolution
                  (Calls.bound_identifier_occurrence bound)
              with
              | Bind.Local_binding binding -> Some binding
              | _ -> None)
          | _ -> None
        in
        let matches_binding location value =
          Option.fold ~none:false
            ~some:(( == ) (Frame.location_binding location))
            (binding_of value)
        in
        let update_matches (target, (operand, opcode)) statement =
          let value = Typed.expression_statement_value statement in
          match
            ( Calls.argument_expression_source (Typed.result_source value),
              Typed.result_binary_operands value )
          with
          | Some (Frontend.Ast.Binary_expression binary), Some (left, right) ->
              (match (binary.binary_operator_spec.spelling, opcode) with
                | "+=", O.Ic_add_equ
                | "-=", O.Ic_sub_equ
                | "*=", O.Ic_mul_equ
                | "/=", O.Ic_div_equ
                | "%=", O.Ic_mod_equ
                | "<<=", O.Ic_shl_equ
                | ">>=", O.Ic_shr_equ
                | "&=", O.Ic_and_equ
                | "|=", O.Ic_or_equ
                | "^=", O.Ic_xor_equ -> true
                | _ -> false)
              && matches_binding target left
              && matches_binding operand right
          | _ -> false
        in
        let rec result_matches typed value =
          match Calls.argument_expression_kind (Typed.result_source typed) with
          | Calls.Parenthesized_expression source -> (
              match Typed.result_operand typed with
              | Some operand
                when Typed.result_source operand == source
                     && Typed.result_intrinsic_conversion typed
                        = Typed.No_intrinsic_conversion
                     && Option.equal Sema.Type.equal (Typed.result_type typed)
                          (Typed.result_type operand) ->
                  result_matches operand value
              | _ -> false)
          | _ -> (
              match Values.find_opt value produced with
              | None -> false
              | Some (item : Seq.description) -> (
                  match (item.opcode, item.operands) with
                  | O.Ic_deref, [ address ] ->
                      Option.fold ~none:false
                        ~some:(fun location -> matches_binding location typed)
                        (List.find_map
                           (fun (id, location) ->
                             if Seq.Value_id.equal id address then Some location
                             else None)
                           addresses)
                  | O.Ic_add, [ left; right ] -> (
                      match
                        ( Calls.argument_expression_source
                            (Typed.result_source typed),
                          Typed.result_binary_operands typed )
                      with
                      | ( Some (Frontend.Ast.Binary_expression binary),
                          Some (typed_left, typed_right) ) ->
                          binary.binary_operator_spec.spelling = "+"
                          && Option.equal Sema.Type.equal
                               (Typed.result_type typed) item.target_type
                          && result_matches typed_left left
                          && result_matches typed_right right
                      | _ -> false)
                  | _ -> false))
        in
        Option.fold ~none:false
          ~some:(fun source ->
            match
              ( Typed.function_expression_statements source,
                Typed.function_returns source )
            with
            | statements, [ returned ]
              when List.length statements = List.length targets ->
                List.for_all2 update_matches
                  (List.combine targets (List.combine operands opcodes))
                  statements
                && Option.fold ~none:false
                     ~some:(fun value -> result_matches value root)
                     (Typed.return_value returned)
            | _ -> false)
          (Function_body.definition_source body)
      in
      let selected_targets =
        let sequence_selection () =
          Option.bind matching_sequence
            (fun (targets, operands, root, opcodes) ->
              if sequence_source_matches (targets, operands, root, opcodes) then
                Some targets
              else None)
        in
        if arithmetic_additions = [] then
          match
            Option.bind matching_target (fun (target, count) ->
                if source_matches (target, count) then Some [ target ] else None)
          with
          | Some _ as selection -> selection
          | None -> sequence_selection ()
        else
          match matching_pair with
          | Some
              ( first,
                second,
                first_count,
                second_count,
                sum_type,
                first_opcode,
                second_opcode )
            when pair_source_matches
                   ( first,
                     second,
                     first_count,
                     second_count,
                     sum_type,
                     first_opcode,
                     second_opcode ) -> Some [ first; second ]
          | _ -> sequence_selection ()
      in
      if
        (not (addresses_safe && rbp_safe && dereferences_safe && simple_flow))
        || Option.is_none selected_targets
      then None
      else
        let targets = Option.get selected_targets in
        let selections =
          List.map
            (fun location ->
              let selection =
                if not (List.exists (( == ) location) targets) then Unknown
                else if stack_selected ~body location then Stack
                else Register_candidate
              in
              (Frame.location_symbol location, selection))
            locations
        in
        Some
          {
            frame_ = frame;
            parameters_ = List.map Fun.id (signature_parameters frame);
            parameter_sources_ =
              Option.map (List.map Fun.id) (source_parameters frame);
            body_ = body;
            definition_ = Function_body.definition_declaration body;
            source_ = Function_body.definition_source body;
            options_ = Function_body.compiler_options body;
            flags_ = Function_body.stored_flags body;
            locations_ =
              List.map
                (fun location -> Option.get (location_stamp ~frame location))
                locations;
            blocks_ = blocks;
            stamps_ =
              List.map
                (fun item -> (item, Option.get (instruction_stamp item)))
                code;
            selections_ = selections;
            exact_registers_ = None;
            shadowed_explicit_ = [];
            explicit_conflicts_ = [];
            exact_source_ = None;
            runtime_ = None;
          }

let analyze_local ~frame ~body =
  let module Typed = Sema.Function_call_expression_result in
  let module Calls = Sema.Function_call_resolution in
  let module Bind = Sema.Module_expression_binding in
  let locations = Frame.function_locations frame in
  let supported location =
    Frame.location_kind location = Frame.Automatic_local
    && Frame.location_value_shape location = Frame.Scalar
    && Sema.Type.pointer_depth (Frame.location_checked_type location) = 0
    && (match Sema.Type.base (Frame.location_checked_type location) with
      | Sema.Type.Primitive
          (_, (Sema.Primitive_type.I8 | U8 | I16 | U16 | I32 | U32)) -> true
      | _ -> false)
    && Option.is_some (Frame.location_frame_slot location)
  in
  let unary_parameter location =
    Frame.location_kind location = Frame.Named_parameter
    && Frame.location_value_shape location = Frame.Scalar
    && Sema.Type.pointer_depth (Frame.location_checked_type location) = 0
    && (match Sema.Type.base (Frame.location_checked_type location) with
      | Sema.Type.Primitive
          (_, (Sema.Primitive_type.I8 | U8 | I16 | U16 | I32 | U32)) -> true
      | _ -> false)
    && Option.is_some (Frame.location_frame_slot location)
  in
  let ordinary =
    Option.is_some (Function_body.definition_declaration body)
    && Function_body.definition_matches_frame body frame
    && supported_options body
    && safe_register_requests locations
    && Int64.logand
         (Function_body.stored_flags body)
         (Int64.lognot Function_body.ordinary_calling_flag_mask)
       = 0L
  in
  let parameter location =
    Frame.location_kind location = Frame.Named_parameter
    && Frame.location_value_shape location = Frame.Scalar
    && Sema.Type.pointer_depth (Frame.location_checked_type location) = 0
    && (match Sema.Type.base (Frame.location_checked_type location) with
      | Sema.Type.Primitive
          (_, (Sema.Primitive_type.I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64))
        -> true
      | _ -> false)
    && Option.is_some (Frame.location_frame_slot location)
  in
  let count_parameter location =
    Frame.location_kind location = Frame.Named_parameter
    && Frame.location_value_shape location = Frame.Scalar
    && Sema.Type.pointer_depth (Frame.location_checked_type location) = 0
    && (match Sema.Type.base (Frame.location_checked_type location) with
      | Sema.Type.Primitive
          (_, (Sema.Primitive_type.I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64))
        -> true
      | _ -> false)
    && Option.is_some (Frame.location_frame_slot location)
  in
  let shape =
    match locations with
    | [ location ] when supported location || unary_parameter location ->
        Some (None, None, location)
    | [ argument; location ] when parameter argument && supported location ->
        Some (Some argument, None, location)
    | [ argument; count; location ]
      when parameter argument && count_parameter count && supported location ->
        Some (Some argument, Some count, location)
    | _ -> None
  in
  match shape with
  | Some (argument, count_argument, location) when ordinary ->
      let blocks = snapshot body in
      let code =
        List.concat_map (fun (_, instructions, _) -> instructions) blocks
      in
      let entry =
        Block_graph.block_id (Block_graph.entry (Function_body.body body))
      in
      let valid_terminator (item : Seq.description) successors =
        item.operands = [] && item.result = None
        &&
        match (item.opcode, item.payload, successors) with
        | O.Ic_jmp, Some (Seq.Block target), [ successor ] ->
            Seq.Block_id.equal target successor
        | O.Ic_ret, None, [] -> true
        | _ -> false
      in
      let valid_block (id, instructions, successors) =
        if Seq.Block_id.equal id entry then true
        else
          match instructions with
          | [ item ] -> valid_terminator item successors
          | _ -> false
      in
      let core =
        List.filter
          (fun (item : Seq.description) ->
            item.opcode <> O.Ic_jmp && item.opcode <> O.Ic_ret)
          code
      in
      let entry_sequence =
        List.find_map
          (fun (id, instructions, successors) ->
            if Seq.Block_id.equal id entry then Some (instructions, successors)
            else None)
          blocks
      in
      let exact_entry =
        Option.fold ~none:false
          ~some:(fun (instructions, successors) ->
            match List.rev instructions with
            | jump :: reversed when valid_terminator jump successors ->
                same_list ( == ) core (List.rev reversed)
            | _ -> false)
          entry_sequence
      in
      let unique values =
        List.length values = List.length (List.sort_uniq Int.compare values)
      in
      let unique_ids =
        unique
          (List.map
             (fun (item : Seq.description) ->
               Seq.Instruction_id.to_int item.instruction_id)
             code)
        && unique
             (List.filter_map
                (fun (item : Seq.description) ->
                  Option.map
                    (fun (result : Seq.value_definition) ->
                      Seq.Value_id.to_int result.value_id)
                    item.result)
                code)
      in
      let id (item : Seq.description) =
        Option.map (fun (v : Seq.value_definition) -> v.value_id) item.result
      in
      let is_one item operand = item.Seq.operands = [ operand ] in
      let integer (item : Seq.description) =
        match (item.opcode, item.payload, item.operands) with
        | O.Ic_imm_i64, Some (Seq.Integer value), [] -> Some value
        | _ -> None
      in
      let address location rbp offset add =
        rbp.Seq.opcode = O.Ic_rbp && rbp.operands = [] && rbp.payload = None
        && offset.Seq.opcode = O.Ic_imm_i64
        && Option.fold ~none:false
             ~some:(fun slot ->
               integer offset = Some (Frame.frame_slot_displacement slot))
             (Frame.location_frame_slot location)
        && add.Seq.opcode = O.Ic_add && add.payload = None
        && (match (id rbp, id offset) with
          | Some r, Some o -> add.operands = [ r; o ] || add.operands = [ o; r ]
          | _ -> false)
        && Option.fold ~none:false
             ~some:(fun pointer ->
               Option.fold ~none:false ~some:(Sema.Type.equal pointer)
                 rbp.target_type
               && Option.fold ~none:false ~some:(Sema.Type.equal pointer)
                    offset.target_type
               &&
               match Sema.Type.dereference pointer with
               | Ok actual ->
                   Sema.Type.equal actual (Frame.location_checked_type location)
               | Error _ -> false)
             add.target_type
      in
      let scalar_literal (item : Seq.description) =
        Option.fold ~none:false
          ~some:(fun type_ ->
            Sema.Type.pointer_depth type_ = 0
            &&
            match Sema.Type.base type_ with
            | Sema.Type.Primitive (_, (Sema.Primitive_type.I64 | U64)) -> true
            | _ -> false)
          item.target_type
      in
      let same_type (item : Seq.description) =
        Option.fold ~none:false
          ~some:(Sema.Type.equal (Frame.location_checked_type location))
          item.target_type
      in
      let binding_of value =
        match Calls.argument_expression_kind (Typed.result_source value) with
        | Calls.Bound_identifier_expression bound -> (
            match
              Bind.occurrence_resolution
                (Calls.bound_identifier_occurrence bound)
            with
            | Bind.Local_binding binding -> Some binding
            | _ -> None)
        | _ -> None
      in
      let literal value =
        match Calls.argument_expression_kind (Typed.result_source value) with
        | Calls.Integer_literal bits -> Some bits
        | _ -> None
      in
      let source_matches initial count update =
        Option.fold ~none:false
          ~some:(fun source ->
            match
              ( Typed.function_initializers source,
                Typed.function_expression_statements source,
                Typed.function_returns source )
            with
            | [ initialization ], [ statement ], [ returned ] -> (
                let value = Typed.expression_statement_value statement in
                match
                  ( Calls.argument_expression_source (Typed.result_source value),
                    Typed.result_binary_operands value,
                    Typed.return_value returned )
                with
                | ( Some (Frontend.Ast.Binary_expression binary),
                    Some (left, right),
                    Some returned ) ->
                    let operator =
                      match
                        (binary.binary_operator_spec.spelling, update.Seq.opcode)
                      with
                      | "<<=", O.Ic_shl_equ
                      | ">>=", Ic_shr_equ
                      | "+=", Ic_add_equ
                      | "-=", Ic_sub_equ
                      | "*=", Ic_mul_equ
                      | "/=", Ic_div_equ
                      | "%=", Ic_mod_equ
                      | "&=", Ic_and_equ
                      | "|=", Ic_or_equ
                      | "^=", Ic_xor_equ -> true
                      | _ -> false
                    in
                    operator
                    && Typed.initializer_target_symbol initialization
                       == Frame.location_symbol location
                    && (match (argument, initial) with
                      | _, Some bits ->
                          literal (Typed.initializer_value initialization)
                          = Some bits
                      | Some argument, None ->
                          Option.fold ~none:false
                            ~some:(( == ) (Frame.location_binding argument))
                            (binding_of
                               (Typed.initializer_value initialization))
                      | _ -> false)
                    && (match (count_argument, count) with
                      | None, Some bits -> literal right = Some bits
                      | Some argument, None ->
                          Option.fold ~none:false
                            ~some:(( == ) (Frame.location_binding argument))
                            (binding_of right)
                      | _ -> false)
                    && Option.fold ~none:false
                         ~some:(( == ) (Frame.location_binding location))
                         (binding_of left)
                    && Option.fold ~none:false
                         ~some:(( == ) (Frame.location_binding location))
                         (binding_of returned)
                | _ -> false)
            | _ -> false)
          (Function_body.definition_source body)
      in
      let source_matches_direct initial =
        Option.fold ~none:false
          ~some:(fun source ->
            match
              ( Typed.function_initializers source,
                Typed.function_expression_statements source,
                Typed.function_returns source )
            with
            | [ initialization ], [], [ returned ] ->
                Typed.initializer_target_symbol initialization
                == Frame.location_symbol location
                && literal (Typed.initializer_value initialization)
                   = Some initial
                && Option.fold ~none:false
                     ~some:(fun returned ->
                       Option.fold ~none:false
                         ~some:(( == ) (Frame.location_binding location))
                         (binding_of returned))
                     (Typed.return_value returned)
            | _ -> false)
          (Function_body.definition_source body)
      in
      let source_matches_parameter argument =
        Option.fold ~none:false
          ~some:(fun source ->
            match
              ( Typed.function_initializers source,
                Typed.function_expression_statements source,
                Typed.function_returns source )
            with
            | [ initialization ], [], [ returned ] ->
                Typed.initializer_target_symbol initialization
                == Frame.location_symbol location
                && Option.fold ~none:false
                     ~some:(( == ) (Frame.location_binding argument))
                     (binding_of (Typed.initializer_value initialization))
                && Option.fold ~none:false
                     ~some:(fun value ->
                       Option.fold ~none:false
                         ~some:(( == ) (Frame.location_binding location))
                         (binding_of value))
                     (Typed.return_value returned)
            | _ -> false)
          (Function_body.definition_source body)
      in
      let source_matches_literal_parameter initial argument update =
        Option.fold ~none:false
          ~some:(fun source ->
            match
              ( Typed.function_initializers source,
                Typed.function_expression_statements source,
                Typed.function_returns source )
            with
            | [ initialization ], [ statement ], [ returned ] -> (
                let value = Typed.expression_statement_value statement in
                match
                  ( Calls.argument_expression_source (Typed.result_source value),
                    Typed.result_binary_operands value,
                    Typed.return_value returned )
                with
                | ( Some (Frontend.Ast.Binary_expression binary),
                    Some (left, right),
                    Some returned ) ->
                    (match
                       (binary.binary_operator_spec.spelling, update.Seq.opcode)
                     with
                      | "+=", O.Ic_add_equ
                      | "-=", Ic_sub_equ
                      | "*=", Ic_mul_equ
                      | "/=", Ic_div_equ
                      | "%=", Ic_mod_equ
                      | "<<=", Ic_shl_equ
                      | ">>=", Ic_shr_equ
                      | "&=", Ic_and_equ
                      | "|=", Ic_or_equ
                      | "^=", Ic_xor_equ -> true
                      | _ -> false)
                    && Typed.initializer_target_symbol initialization
                       == Frame.location_symbol location
                    && literal (Typed.initializer_value initialization)
                       = Some initial
                    && Option.fold ~none:false
                         ~some:(( == ) (Frame.location_binding location))
                         (binding_of left)
                    && Option.fold ~none:false
                         ~some:(( == ) (Frame.location_binding argument))
                         (binding_of right)
                    && Option.fold ~none:false
                         ~some:(( == ) (Frame.location_binding location))
                         (binding_of returned)
                | _ -> false)
            | _ -> false)
          (Function_body.definition_source body)
      in
      let source_matches_unary initial update =
        Option.fold ~none:false
          ~some:(fun source ->
            match
              ( Typed.function_initializers source,
                Typed.function_expression_statements source,
                Typed.function_returns source )
            with
            | initializers, [ statement ], [ returned ] ->
                let initial_matches =
                  match (initial, initializers) with
                  | None, [] -> unary_parameter location
                  | Some bits, [ initialization ] ->
                      supported location
                      && Typed.initializer_target_symbol initialization
                         == Frame.location_symbol location
                      && literal (Typed.initializer_value initialization)
                         = Some bits
                  | _ -> false
                in
                let value = Typed.expression_statement_value statement in
                let operator_matches =
                  match
                    Calls.argument_expression_source (Typed.result_source value)
                  with
                  | Some (Frontend.Ast.Prefix_expression prefix) -> (
                      match
                        (prefix.prefix_operator_kind, update.Seq.opcode)
                      with
                      | Frontend.Ast.Pre_increment, O.Ic_pp_
                      | Frontend.Ast.Pre_decrement, Ic_mm_ -> true
                      | _ -> false)
                  | Some (Frontend.Ast.Postfix_expression postfix) -> (
                      match
                        (postfix.postfix_operator_kind, update.Seq.opcode)
                      with
                      | Frontend.Ast.Post_increment, O.Ic__pp
                      | Frontend.Ast.Post_decrement, Ic__mm -> true
                      | _ -> false)
                  | _ -> false
                in
                initial_matches && operator_matches
                && Option.fold ~none:false
                     ~some:(fun operand ->
                       Option.fold ~none:false
                         ~some:(( == ) (Frame.location_binding location))
                         (binding_of operand))
                     (Typed.result_operand value)
                && Option.fold ~none:false
                     ~some:(fun value ->
                       Option.fold ~none:false
                         ~some:(( == ) (Frame.location_binding location))
                         (binding_of value))
                     (Typed.return_value returned)
            | _ -> false)
          (Function_body.definition_source body)
      in
      let source_matches_unary_return initial update =
        Option.fold ~none:false
          ~some:(fun source ->
            match
              ( Typed.function_initializers source,
                Typed.function_expression_statements source,
                Typed.function_returns source )
            with
            | initializers, [], [ returned ] ->
                let initial_matches =
                  match (initial, initializers) with
                  | None, [] -> unary_parameter location
                  | Some bits, [ initialization ] ->
                      supported location
                      && Typed.initializer_target_symbol initialization
                         == Frame.location_symbol location
                      && literal (Typed.initializer_value initialization)
                         = Some bits
                  | _ -> false
                in
                initial_matches
                && Option.fold ~none:false
                     ~some:(fun value ->
                       let operator_matches =
                         match
                           Calls.argument_expression_source
                             (Typed.result_source value)
                         with
                         | Some (Frontend.Ast.Prefix_expression prefix) -> (
                             match
                               (prefix.prefix_operator_kind, update.Seq.opcode)
                             with
                             | Frontend.Ast.Pre_increment, O.Ic_pp_
                             | Frontend.Ast.Pre_decrement, Ic_mm_ -> true
                             | _ -> false)
                         | Some (Frontend.Ast.Postfix_expression postfix) -> (
                             match
                               (postfix.postfix_operator_kind, update.Seq.opcode)
                             with
                             | Frontend.Ast.Post_increment, O.Ic__pp
                             | Frontend.Ast.Post_decrement, Ic__mm -> true
                             | _ -> false)
                         | _ -> false
                       in
                       operator_matches
                       && Option.fold ~none:false
                            ~some:(fun operand ->
                              Option.fold ~none:false
                                ~some:(( == ) (Frame.location_binding location))
                                (binding_of operand))
                            (Typed.result_operand value))
                     (Typed.return_value returned)
            | _ -> false)
          (Function_body.definition_source body)
      in
      let source_matches_compound_return initial count update =
        Option.fold ~none:false
          ~some:(fun source ->
            match
              ( Typed.function_initializers source,
                Typed.function_expression_statements source,
                Typed.function_returns source )
            with
            | initializers, [], [ returned ] -> (
                let initial_matches =
                  match (initial, initializers) with
                  | None, [] -> unary_parameter location
                  | Some bits, [ initialization ] ->
                      supported location
                      && Typed.initializer_target_symbol initialization
                         == Frame.location_symbol location
                      && literal (Typed.initializer_value initialization)
                         = Some bits
                  | _ -> false
                in
                match Typed.return_value returned with
                | Some value -> (
                    match
                      ( Calls.argument_expression_source
                          (Typed.result_source value),
                        Typed.result_binary_operands value )
                    with
                    | ( Some (Frontend.Ast.Binary_expression binary),
                        Some (left, right) ) ->
                        (match
                           ( binary.binary_operator_spec.spelling,
                             update.Seq.opcode )
                         with
                          | "+=", O.Ic_add_equ
                          | "-=", Ic_sub_equ
                          | "*=", Ic_mul_equ
                          | "<<=", Ic_shl_equ
                          | ">>=", Ic_shr_equ -> true
                          | _ -> false)
                        && initial_matches
                        && literal right = Some count
                        && Option.fold ~none:false
                             ~some:(( == ) (Frame.location_binding location))
                             (binding_of left)
                    | _ -> false)
                | None -> false)
            | _ -> false)
          (Function_body.definition_source body)
      in
      let matched =
        match core with
        | [ rbp; offset; address_; count; update; returned ]
          when update.Seq.opcode = O.Ic_add_equ
               || update.opcode = O.Ic_sub_equ
               || update.opcode = O.Ic_mul_equ
               || update.opcode = O.Ic_shl_equ
               || update.opcode = O.Ic_shr_equ -> (
            match (id address_, id count, integer count, id update) with
            | Some addressid, Some countid, Some countbits, Some updateid ->
                Option.is_none argument
                && Option.is_none count_argument
                && unary_parameter location
                && address location rbp offset address_
                && scalar_literal count
                && (update.opcode <> O.Ic_add_equ
                    && update.opcode <> O.Ic_sub_equ
                   || countbits >= 0L
                      && countbits <= Int64.of_int32 Int32.max_int)
                && update.payload = None && same_type update
                && update.operands = [ addressid; countid ]
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned updateid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && item.flags = 0L)
                     code
                && source_matches_compound_return None countbits update
            | _ -> false)
        | [
         r1; o1; a1; initial; store; end1; r2; o2; a2; count; update; returned;
        ]
          when update.Seq.opcode = O.Ic_add_equ
               || update.opcode = O.Ic_sub_equ
               || update.opcode = O.Ic_mul_equ
               || update.opcode = O.Ic_shl_equ
               || update.opcode = O.Ic_shr_equ -> (
            match
              ( id a1,
                id initial,
                integer initial,
                id store,
                id a2,
                id count,
                integer count,
                id update )
            with
            | ( Some a1id,
                Some initialid,
                Some initialbits,
                Some storeid,
                Some a2id,
                Some countid,
                Some countbits,
                Some updateid ) ->
                Option.is_none argument
                && Option.is_none count_argument
                && supported location && address location r1 o1 a1
                && scalar_literal initial && address location r2 o2 a2
                && scalar_literal count
                && store.Seq.opcode = O.Ic_assign
                && store.payload = None && same_type store
                && store.operands = [ a1id; initialid ]
                && end1.Seq.opcode = O.Ic_end_exp
                && end1.payload = None && end1.result = None
                && end1.flags = 0x200L && is_one end1 storeid
                && (update.opcode <> O.Ic_add_equ
                    && update.opcode <> O.Ic_sub_equ
                   || countbits >= 0L
                      && countbits <= Int64.of_int32 Int32.max_int)
                && update.payload = None && same_type update
                && update.operands = [ a2id; countid ]
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned updateid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && (item.opcode = O.Ic_end_exp || item.flags = 0L))
                     code
                && source_matches_compound_return (Some initialbits) countbits
                     update
            | _ -> false)
        | [ r1; o1; a1; update; returned ] -> (
            match (id a1, id update) with
            | Some a1id, Some updateid ->
                Option.is_none argument
                && Option.is_none count_argument
                && address location r1 o1 a1
                && (update.Seq.opcode = O.Ic_pp_
                   || update.opcode = O.Ic_mm_ || update.opcode = O.Ic__pp
                   || update.opcode = O.Ic__mm)
                && update.payload = None && same_type update
                && is_one update a1id
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned updateid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && item.flags = 0L)
                     code
                && source_matches_unary_return None update
            | _ -> false)
        | [ r1; o1; a1; initial; store; end1; r2; o2; a2; update; returned ]
          when update.Seq.opcode = O.Ic_pp_
               || update.opcode = O.Ic_mm_ || update.opcode = O.Ic__pp
               || update.opcode = O.Ic__mm -> (
            match
              (id a1, id initial, integer initial, id store, id a2, id update)
            with
            | ( Some a1id,
                Some initialid,
                Some initialbits,
                Some storeid,
                Some a2id,
                Some updateid ) ->
                Option.is_none argument
                && Option.is_none count_argument
                && address location r1 o1 a1 && scalar_literal initial
                && address location r2 o2 a2
                && store.Seq.opcode = O.Ic_assign
                && store.payload = None && same_type store
                && store.operands = [ a1id; initialid ]
                && end1.Seq.opcode = O.Ic_end_exp
                && end1.payload = None && end1.result = None
                && end1.flags = 0x200L && is_one end1 storeid
                && (update.Seq.opcode = O.Ic_pp_
                   || update.opcode = O.Ic_mm_ || update.opcode = O.Ic__pp
                   || update.opcode = O.Ic__mm)
                && update.payload = None && same_type update
                && is_one update a2id
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned updateid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && (item.opcode = O.Ic_end_exp || item.flags = 0L))
                     code
                && source_matches_unary_return (Some initialbits) update
            | _ -> false)
        | [ r1; o1; a1; update; end1; r2; o2; a2; load; returned ] -> (
            match (id a1, id update, id a2, id load) with
            | Some a1id, Some updateid, Some a2id, Some loadid ->
                Option.is_none argument
                && Option.is_none count_argument
                && address location r1 o1 a1 && address location r2 o2 a2
                && (update.Seq.opcode = O.Ic_pp_
                   || update.opcode = O.Ic_mm_ || update.opcode = O.Ic__pp
                   || update.opcode = O.Ic__mm)
                && update.payload = None && same_type update
                && is_one update a1id
                && end1.Seq.opcode = O.Ic_end_exp
                && end1.payload = None && end1.result = None
                && end1.flags = 0x200L && is_one end1 updateid
                && load.Seq.opcode = O.Ic_deref
                && load.payload = None && same_type load && is_one load a2id
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned loadid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && (item.opcode = O.Ic_end_exp || item.flags = 0L))
                     code
                && source_matches_unary None update
            | _ -> false)
        | [
         r1;
         o1;
         a1;
         initial;
         store;
         end1;
         r2;
         o2;
         a2;
         update;
         end2;
         r3;
         o3;
         a3;
         load;
         returned;
        ] -> (
            match
              ( id a1,
                id initial,
                integer initial,
                id store,
                id a2,
                id update,
                id a3,
                id load )
            with
            | ( Some a1id,
                Some initialid,
                Some initialbits,
                Some storeid,
                Some a2id,
                Some updateid,
                Some a3id,
                Some loadid ) ->
                Option.is_none argument
                && Option.is_none count_argument
                && address location r1 o1 a1 && scalar_literal initial
                && address location r2 o2 a2 && address location r3 o3 a3
                && store.Seq.opcode = O.Ic_assign
                && store.payload = None && same_type store
                && store.operands = [ a1id; initialid ]
                && end1.Seq.opcode = O.Ic_end_exp
                && end1.payload = None && end1.result = None
                && end1.flags = 0x200L && is_one end1 storeid
                && (update.Seq.opcode = O.Ic_pp_
                   || update.opcode = O.Ic_mm_ || update.opcode = O.Ic__pp
                   || update.opcode = O.Ic__mm)
                && update.payload = None && same_type update
                && is_one update a2id
                && end2.Seq.opcode = O.Ic_end_exp
                && end2.payload = None && end2.result = None
                && end2.flags = 0x200L && is_one end2 updateid
                && load.Seq.opcode = O.Ic_deref
                && load.payload = None && same_type load && is_one load a3id
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned loadid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && (item.opcode = O.Ic_end_exp || item.flags = 0L))
                     code
                && source_matches_unary (Some initialbits) update
            | _ -> false)
        | [
         r1; o1; a1; rp; op; ap; input; store; end1; r2; o2; a2; load; returned;
        ] -> (
            match
              (argument, id a1, id ap, id input, id store, id a2, id load)
            with
            | ( Some argument,
                Some a1id,
                Some apid,
                Some inputid,
                Some storeid,
                Some a2id,
                Some loadid ) ->
                address location r1 o1 a1 && address argument rp op ap
                && address location r2 o2 a2
                && input.Seq.opcode = O.Ic_deref
                && input.payload = None
                && Option.fold ~none:false
                     ~some:
                       (Sema.Type.equal (Frame.location_checked_type argument))
                     input.target_type
                && is_one input apid
                && store.Seq.opcode = O.Ic_assign
                && store.payload = None && same_type store
                && store.operands = [ a1id; inputid ]
                && end1.Seq.opcode = O.Ic_end_exp
                && end1.payload = None && end1.result = None
                && end1.flags = 0x200L && is_one end1 storeid
                && load.Seq.opcode = O.Ic_deref
                && load.payload = None && same_type load && is_one load a2id
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned loadid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && (item.opcode = O.Ic_end_exp || item.flags = 0L))
                     code
                && Option.is_none count_argument
                && source_matches_parameter argument
            | _ -> false)
        | [
         r1;
         o1;
         a1;
         rp;
         op;
         ap;
         input;
         store;
         end1;
         r2;
         o2;
         a2;
         count;
         update;
         end2;
         r3;
         o3;
         a3;
         load;
         returned;
        ]
          when rp.Seq.opcode = O.Ic_rbp -> (
            match
              ( argument,
                id a1,
                id ap,
                id input,
                id store,
                id a2,
                id count,
                integer count,
                id update,
                id a3,
                id load )
            with
            | ( Some argument,
                Some a1id,
                Some apid,
                Some inputid,
                Some storeid,
                Some a2id,
                Some countid,
                Some countbits,
                Some updateid,
                Some a3id,
                Some loadid ) ->
                address location r1 o1 a1 && address argument rp op ap
                && address location r2 o2 a2 && address location r3 o3 a3
                && input.Seq.opcode = O.Ic_deref
                && input.payload = None
                && Option.fold ~none:false
                     ~some:
                       (Sema.Type.equal (Frame.location_checked_type argument))
                     input.target_type
                && is_one input apid && scalar_literal count
                && store.Seq.opcode = O.Ic_assign
                && store.payload = None && same_type store
                && store.operands = [ a1id; inputid ]
                && end1.Seq.opcode = O.Ic_end_exp
                && end1.payload = None && end1.result = None
                && end1.flags = 0x200L && is_one end1 storeid
                && (update.Seq.opcode = O.Ic_shl_equ
                   || update.opcode = O.Ic_shr_equ
                   || update.opcode = O.Ic_mul_equ
                   || (update.opcode = O.Ic_add_equ
                      || update.opcode = O.Ic_sub_equ)
                      && countbits >= 0L
                      && countbits <= Int64.of_int32 Int32.max_int)
                && update.payload = None && same_type update
                && update.operands = [ a2id; countid ]
                && end2.Seq.opcode = O.Ic_end_exp
                && end2.payload = None && end2.result = None
                && end2.flags = 0x200L && is_one end2 updateid
                && load.Seq.opcode = O.Ic_deref
                && load.payload = None && same_type load && is_one load a3id
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned loadid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && (item.opcode = O.Ic_end_exp || item.flags = 0L))
                     code
                && source_matches None (Some countbits) update
            | _ -> false)
        | [
         r1;
         o1;
         a1;
         rp;
         op;
         ap;
         input;
         store;
         end1;
         r2;
         o2;
         a2;
         rc;
         oc;
         ac;
         count;
         update;
         end2;
         r3;
         o3;
         a3;
         load;
         returned;
        ] -> (
            match
              ( argument,
                count_argument,
                id a1,
                id ap,
                id input,
                id store,
                id a2,
                id ac,
                id count,
                id update,
                id a3,
                id load )
            with
            | ( Some argument,
                Some count_argument,
                Some a1id,
                Some apid,
                Some inputid,
                Some storeid,
                Some a2id,
                Some acid,
                Some countid,
                Some updateid,
                Some a3id,
                Some loadid ) ->
                address location r1 o1 a1 && address argument rp op ap
                && address location r2 o2 a2
                && address count_argument rc oc ac
                && address location r3 o3 a3
                && input.Seq.opcode = O.Ic_deref
                && input.payload = None
                && Option.fold ~none:false
                     ~some:
                       (Sema.Type.equal (Frame.location_checked_type argument))
                     input.target_type
                && is_one input apid
                && store.Seq.opcode = O.Ic_assign
                && store.payload = None && same_type store
                && store.operands = [ a1id; inputid ]
                && end1.Seq.opcode = O.Ic_end_exp
                && end1.payload = None && end1.result = None
                && end1.flags = 0x200L && is_one end1 storeid
                && count.Seq.opcode = O.Ic_deref
                && count.payload = None
                && Option.fold ~none:false
                     ~some:
                       (Sema.Type.equal
                          (Frame.location_checked_type count_argument))
                     count.target_type
                && is_one count acid
                && (update.Seq.opcode = O.Ic_shl_equ
                   || update.opcode = O.Ic_shr_equ
                   || update.opcode = O.Ic_add_equ
                   || update.opcode = O.Ic_sub_equ
                   || update.opcode = O.Ic_mul_equ
                   || update.opcode = O.Ic_div_equ
                   || update.opcode = O.Ic_mod_equ
                   || update.opcode = O.Ic_and_equ
                   || update.opcode = O.Ic_or_equ
                   || update.opcode = O.Ic_xor_equ)
                && update.payload = None && same_type update
                && update.operands = [ a2id; countid ]
                && end2.Seq.opcode = O.Ic_end_exp
                && end2.payload = None && end2.result = None
                && end2.flags = 0x200L && is_one end2 updateid
                && load.Seq.opcode = O.Ic_deref
                && load.payload = None && same_type load && is_one load a3id
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned loadid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && (item.opcode = O.Ic_end_exp || item.flags = 0L))
                     code
                && source_matches None None update
            | _ -> false)
        | [
         r1;
         o1;
         a1;
         initial;
         store;
         end1;
         r2;
         o2;
         a2;
         rp;
         op;
         ap;
         count;
         update;
         end2;
         r3;
         o3;
         a3;
         load;
         returned;
        ] -> (
            match
              ( argument,
                count_argument,
                id a1,
                id initial,
                integer initial,
                id store,
                id a2,
                id ap,
                id count,
                id update,
                id a3,
                id load )
            with
            | ( Some argument,
                None,
                Some a1id,
                Some initialid,
                Some initialbits,
                Some storeid,
                Some a2id,
                Some apid,
                Some countid,
                Some updateid,
                Some a3id,
                Some loadid ) ->
                (match
                   Sema.Type.base (Frame.location_checked_type argument)
                 with
                  | Sema.Type.Primitive (_, (Sema.Primitive_type.I64 | U64)) ->
                      true
                  | _ -> false)
                && address location r1 o1 a1 && scalar_literal initial
                && address location r2 o2 a2 && address argument rp op ap
                && address location r3 o3 a3
                && store.Seq.opcode = O.Ic_assign
                && store.payload = None && same_type store
                && store.operands = [ a1id; initialid ]
                && end1.Seq.opcode = O.Ic_end_exp
                && end1.payload = None && end1.result = None
                && end1.flags = 0x200L && is_one end1 storeid
                && count.Seq.opcode = O.Ic_deref
                && count.payload = None
                && Option.fold ~none:false
                     ~some:
                       (Sema.Type.equal (Frame.location_checked_type argument))
                     count.target_type
                && is_one count apid
                && (update.Seq.opcode = O.Ic_add_equ
                   || update.opcode = O.Ic_sub_equ
                   || update.opcode = O.Ic_mul_equ
                   || update.opcode = O.Ic_div_equ
                   || update.opcode = O.Ic_mod_equ
                   || update.opcode = O.Ic_shl_equ
                   || update.opcode = O.Ic_shr_equ
                   || update.opcode = O.Ic_and_equ
                   || update.opcode = O.Ic_or_equ
                   || update.opcode = O.Ic_xor_equ)
                && update.payload = None && same_type update
                && update.operands = [ a2id; countid ]
                && end2.Seq.opcode = O.Ic_end_exp
                && end2.payload = None && end2.result = None
                && end2.flags = 0x200L && is_one end2 updateid
                && load.Seq.opcode = O.Ic_deref
                && load.payload = None && same_type load && is_one load a3id
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned loadid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && (item.opcode = O.Ic_end_exp || item.flags = 0L))
                     code
                && source_matches_literal_parameter initialbits argument update
            | _ -> false)
        | [ r1; o1; a1; initial; store; end1; r2; o2; a2; load; returned ] -> (
            match
              (id a1, id initial, id store, id a2, id load, integer initial)
            with
            | ( Some a1id,
                Some initialid,
                Some storeid,
                Some a2id,
                Some loadid,
                Some bits ) ->
                address location r1 o1 a1 && address location r2 o2 a2
                && scalar_literal initial
                && store.Seq.opcode = O.Ic_assign
                && store.payload = None && same_type store
                && store.operands = [ a1id; initialid ]
                && end1.Seq.opcode = O.Ic_end_exp
                && end1.payload = None && end1.result = None
                && end1.flags = 0x200L && is_one end1 storeid
                && load.Seq.opcode = O.Ic_deref
                && load.payload = None && same_type load && is_one load a2id
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned loadid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && (item.opcode = O.Ic_end_exp || item.flags = 0L))
                     code
                && Option.is_none count_argument
                && source_matches_direct bits
            | _ -> false)
        | [
         r1;
         o1;
         a1;
         initial;
         store;
         end1;
         r2;
         o2;
         a2;
         count;
         update;
         end2;
         r3;
         o3;
         a3;
         load;
         returned;
        ] -> (
            match
              ( id a1,
                id initial,
                id store,
                id a2,
                id count,
                id update,
                id a3,
                id load,
                integer initial,
                integer count )
            with
            | ( Some a1id,
                Some initialid,
                Some storeid,
                Some a2id,
                Some countid,
                Some shiftid,
                Some a3id,
                Some loadid,
                Some initialbits,
                Some countbits ) ->
                address location r1 o1 a1 && address location r2 o2 a2
                && address location r3 o3 a3 && scalar_literal initial
                && scalar_literal count
                && store.Seq.opcode = O.Ic_assign
                && store.payload = None && same_type store
                && store.operands = [ a1id; initialid ]
                && end1.Seq.opcode = O.Ic_end_exp
                && end1.payload = None && end1.result = None
                && end1.flags = 0x200L && is_one end1 storeid
                && (update.Seq.opcode = O.Ic_shl_equ
                   || update.opcode = O.Ic_shr_equ
                   || update.opcode = O.Ic_add_equ
                   || update.opcode = O.Ic_sub_equ
                   || update.opcode = O.Ic_mul_equ)
                && update.payload = None && same_type update
                && (update.opcode <> O.Ic_add_equ
                    && update.opcode <> O.Ic_sub_equ
                   || countbits >= 0L
                      && countbits <= Int64.of_int32 Int32.max_int)
                && update.operands = [ a2id; countid ]
                && end2.Seq.opcode = O.Ic_end_exp
                && end2.payload = None && end2.result = None
                && end2.flags = 0x200L && is_one end2 shiftid
                && load.Seq.opcode = O.Ic_deref
                && load.payload = None && same_type load && is_one load a3id
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned loadid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && (item.opcode = O.Ic_end_exp || item.flags = 0L))
                     code
                && Option.is_none count_argument
                && source_matches (Some initialbits) (Some countbits) update
            | _ -> false)
        | _ -> false
      in
      if
        List.length code > 64
        || (not (List.for_all valid_block blocks))
        || (not exact_entry) || (not unique_ids) || not matched
      then None
      else
        Some
          {
            frame_ = frame;
            parameters_ = List.map Fun.id (signature_parameters frame);
            parameter_sources_ =
              Option.map (List.map Fun.id) (source_parameters frame);
            body_ = body;
            definition_ = Function_body.definition_declaration body;
            source_ = Function_body.definition_source body;
            options_ = Function_body.compiler_options body;
            flags_ = Function_body.stored_flags body;
            locations_ =
              List.map
                (fun item -> Option.get (location_stamp ~frame item))
                locations;
            blocks_ = blocks;
            stamps_ =
              List.map
                (fun item -> (item, Option.get (instruction_stamp item)))
                code;
            exact_registers_ = None;
            shadowed_explicit_ = [];
            explicit_conflicts_ = [];
            exact_source_ = None;
            runtime_ = None;
            selections_ =
              [
                ( Frame.location_symbol location,
                  if stack_selected ~body location then Stack
                  else Register_candidate );
              ];
          }
  | _ -> None

(* Canonical scalar graphs authenticate complete source ownership and control
   flow, not TempleOS optimizer scores. Retain the original statement tree so
   Exact_register_scores can separately prove optimizer-phase access survival.
   Unsupported phase shapes keep the existing capacity proof. *)
module Mixed_scalar_source = struct
  module Typed = Sema.Function_call_expression_result
  module Source = Sema.Function_call_resolution
  module Policy = Sema.Function_call_conversion_policy
  module Target = Sema.Function_call_target_classification
  module Ast = Frontend.Ast
  module Headers = Sema.Function_type_resolution
  module Local = Sema.Local_type_resolution
  module Lower = Integer_program_lowering

  exception Unproved

  let require value = if not value then raise Unproved

  let get = function
    | Some value -> value
    | None -> raise Unproved

  let scalar type_ =
    Sema.Type.pointer_depth type_ = 0
    &&
    match Sema.Type.base type_ with
    | Sema.Type.Primitive
        ( _,
          (Sema.Primitive_type.I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64 | F64)
        ) -> true
    | _ -> false

  let owned_scalar type_ =
    scalar type_
    || Sema.Type.pointer_depth type_ <= 2
       &&
       match Sema.Type.base type_ with
       | Sema.Type.Primitive (_, primitive) ->
           Option.is_some (Sema.Primitive_type.integer_storage_info primitive)
       | _ -> false

  let scalar_or_void_call ~owned value =
    let type_ = get (Typed.result_type value) in
    (if owned then owned_scalar type_ else scalar type_)
    || Sema.Type.pointer_depth type_ = 0
       && (match Sema.Type.base type_ with
         | Sema.Type.Primitive (_, (Sema.Primitive_type.I0 | U0)) -> true
         | _ -> false)
       &&
       match Source.argument_expression_kind (Typed.result_source value) with
       | Source.Unresolved_expression Source.Call_expression -> true
       | _ -> false

  let direct_resolution call = Typed.direct_source call |> Policy.direct_source

  let reconstruct ?runtime ?(owned = false) ~frame ~body () =
    try
      let source = get (Function_body.definition_source body) in
      let declaration = get (Function_body.definition_declaration body) in
      require
        (Function_body.definition_matches_frame body frame
        && Frame.function_header frame
           == Sema.Function_resolution.declaration_site_function
                (Sema.Function_resolution.resolved_declaration_site declaration)
        );
      let definition = get (Typed.function_source_body source) in
      require
        (definition.variadic = None
        && definition.empty_parameter_entries = []
        && definition.name.spelling
           = Sema.Symbol.name (Function_body.symbol body)
        && Some definition.location.span = Function_body.span body
        && Typed.function_selectors source = []
        && Typed.function_switch_cases source = []
        && Typed.function_implicit_outputs source = []
        && Typed.function_outer_callback_calls source = []
        && List.length (Typed.function_all_results source) <= 512);
      let calls =
        List.map
          (function
            | Typed.Direct_call_result call -> call
            | _ -> raise Unproved)
          (Typed.function_calls source)
      in
      let targets =
        match runtime with
        | Some runtime ->
            get
              (Runtime_call_context.function_mixed_scalar_call_targets runtime
                 ~body)
        | None ->
            require (calls = []);
            []
      in
      require
        (List.length targets = List.length calls
        && List.for_all
             (fun call ->
               List.length
                 (List.filter
                    (fun target -> Target.source target == call)
                    targets)
               = 1)
             calls);
      let seen = ref [] and seen_calls = ref [] in
      let rec expression depth value =
        require (depth > 0 && not (List.exists (( == ) value) !seen));
        require
          (List.exists (( == ) value) (Typed.function_all_results source)
          && (Typed.result_array_rank value = 0 || owned)
          && scalar_or_void_call ~owned value);
        seen := value :: !seen;
        let original = Typed.result_source value in
        let ast = get (Source.argument_expression_source original) in
        require
          (Typed.result_origin value
          = Sema.Initializer_source.origin_of_location
              (Ast.expression_location ast));
        let child expected value =
          require
            (Option.equal ( == ) (Some expected)
               (Source.argument_expression_source (Typed.result_source value)));
          expression (depth - 1) value
        in
        let floating = f64_type (get (Typed.result_type value)) in
        match (Source.argument_expression_kind original, ast) with
        | Source.Integer_literal _, Ast.Integer_literal _ ->
            require (not floating);
            []
        | Float_literal _, Ast.Float_literal _ ->
            require floating;
            []
        | Bound_identifier_expression _, Ast.Identifier_expression _ -> (
            match Frame_address_lowering.prepare ~frame value with
            | Ok (Some _) -> []
            | _ -> raise Unproved)
        | Parenthesized_expression _, Ast.Parenthesized_expression group ->
            child group.grouped_expression (get (Typed.result_operand value))
        | Prefix_expression _, Ast.Prefix_expression prefix ->
            require
              ((not floating)
              || List.mem prefix.prefix_operator_kind
                   [ Ast.Unary_plus; Unary_minus ]);
            child prefix.prefix_operand (get (Typed.result_operand value))
        | Postfix_expression _, Ast.Postfix_expression postfix ->
            require (not floating);
            child postfix.postfix_operand (get (Typed.result_operand value))
        | Binary_expression _, Ast.Binary_expression binary ->
            let left, right = get (Typed.result_binary_operands value) in
            let left_float = f64_type (get (Typed.result_type left))
            and right_float = f64_type (get (Typed.result_type right)) in
            require
              ((floating = left_float && floating = right_float)
              || floating
                 && (left_float || right_float)
                 && List.mem binary.binary_operator_spec.spelling
                      [ "+"; "-"; "*"; "/" ]
              || binary.binary_operator_spec.spelling = "="
                 && floating = left_float);
            require
              ((not floating)
              || List.mem binary.binary_operator_spec.spelling
                   [ "+"; "-"; "*"; "/"; "=" ]);
            let left_calls = child binary.binary_left left in
            left_calls @ child binary.binary_right right
        | Source.Index_expression _, Ast.Index_expression index when owned ->
            let base, offset = get (Typed.result_index_operands value) in
            let first = child index.index_base base in
            first @ child index.index_value offset
        | Unresolved_expression Source.Call_expression, Ast.Call_expression ast
          ->
            let resolved =
              match Typed.result_call_resolution value with
              | Some (Source.Direct_call direct) -> direct
              | _ -> raise Unproved
            in
            let call =
              match
                List.filter
                  (fun call -> direct_resolution call == resolved)
                  calls
              with
              | [ call ] -> call
              | _ -> raise Unproved
            in
            require (not (List.exists (( == ) call) !seen_calls));
            seen_calls := call :: !seen_calls;
            require
              (Typed.direct_variadic_results call = []
              && Option.is_none
                   (Headers.function_variadic_bindings
                      (Source.direct_active_header resolved)));
            let arguments =
              List.map
                (fun fixed ->
                  match Typed.fixed_path fixed with
                  | Typed.Provided_result value -> value
                  | Declared_default_result _ -> raise Unproved)
                (Typed.direct_fixed_results call)
            in
            require (List.length arguments = List.length ast.call_arguments);
            let nested =
              List.map2
                (fun original argument ->
                  match original.Ast.call_argument_value with
                  | Ast.Provided_call_argument expected ->
                      child expected argument
                  | Omitted_call_argument -> raise Unproved)
                ast.call_arguments arguments
              |> List.concat
            in
            (Source.direct_source resolved, original) :: nested
        | _ -> raise Unproved
      in
      let validate value =
        let pairs = expression 32 value in
        let pairs =
          List.sort
            (fun (a, _) (b, _) ->
              Int.compare (Source.call_index a) (Source.call_index b))
            pairs
        in
        require
          (Result.is_ok
             (Source.validate_source_expression
                ~source:
                  (get
                     (Source.argument_expression_source
                        (Typed.result_source value)))
                ~expression:(Typed.result_source value)
                ~calls:(List.map fst pairs) ~call_expressions:pairs ()))
      in
      let initializers = ref (Typed.function_initializers source)
      and expressions = ref (Typed.function_expression_statements source)
      and conditions = ref (Typed.function_conditions source)
      and returns = ref (Typed.function_returns source) in
      let take remaining matches =
        let found =
          match List.filter matches !remaining with
          | [ found ] -> found
          | _ -> raise Unproved
        in
        remaining := List.filter (fun other -> other != found) !remaining;
        found
      in
      let matches value ast =
        Option.equal ( == )
          (Source.argument_expression_source (Typed.result_source value))
          (Some ast)
      in
      let condition role ast =
        let root =
          take conditions (fun root ->
              Source.condition_role (Typed.condition_source root) = role
              && matches (Typed.condition_value root) ast)
        in
        let value = Typed.condition_value root in
        require (not (f64_type (get (Typed.result_type value))));
        validate value;
        value
      in
      let declarations = ref 0 and statement_budget = ref 1024 in
      let rec statement depth node =
        decr statement_budget;
        require (depth > 0 && !statement_budget >= 0);
        match node with
        | Ast.Empty_statement empty ->
            Lower.Empty
              (Ast.statement_location (Ast.Empty_statement empty)).span
        | Block_statement block ->
            Lower.Block
              (List.map (statement (depth - 1)) block.block_statements)
        | Sequence_statement sequence ->
            require (sequence.sequence_leading_commas = []);
            Lower.Block
              (List.map
                 (fun element ->
                   require (element.Ast.sequence_following_commas = []);
                   statement (depth - 1) element.sequence_statement)
                 sequence.sequence_elements)
        | Expression_statement ast ->
            let root =
              take expressions (fun root ->
                  matches
                    (Typed.expression_statement_value root)
                    ast.expression_statement_expression)
            in
            let value = Typed.expression_statement_value root in
            validate value;
            Lower.Expression value
        | Return_statement ast ->
            let root =
              take returns (fun root ->
                  (match (Typed.return_value root, ast.return_value) with
                    | Some value, Some original -> matches value original
                    | None, None -> true
                    | _ -> false)
                  && Source.return_origin (Typed.return_source root)
                     = Sema.Initializer_source.origin_of_location
                         ast.return_location)
            in
            (match Typed.return_value root with
            | Some value ->
                require (Typed.return_presence root = Typed.Matching_value);
                validate value
            | None ->
                require (Typed.return_presence root = Typed.Matching_no_value));
            Lower.Return root
        | Local_declaration_statement declaration ->
            require (declaration.local_storage = Ast.Automatic_local);
            let lowered =
              List.mapi
                (fun index (declarator : Ast.local_declarator) ->
                  require
                    ((owned || declarator.local_pointer_layers = [])
                    && (owned || declarator.local_array_dimensions = [])
                    && declarator.local_function_pointer = None);
                  match declarator.local_initializer with
                  | None when owned -> None
                  | None -> raise Unproved
                  | Some initial ->
                      require (declarator.local_array_dimensions = []);
                      let ast =
                        match initial.local_initializer_value with
                        | Ast.Scalar_initializer ast -> ast
                        | _ -> raise Unproved
                      in
                      let root =
                        take initializers (fun root ->
                            let local =
                              Typed.initializer_source root
                              |> Source.initializer_local
                            in
                            Local.local_declaration_index local = !declarations
                            && Local.local_declarator_index local = index
                            && Sema.Symbol.origin (Local.local_symbol local)
                               = Sema.Initializer_source.origin_of_location
                                   declarator.local_name.location
                            && Local.local_declaration_origin local
                               = Sema.Initializer_source.origin_of_location
                                   declaration.local_declaration_location
                            && List.exists
                                 (fun location ->
                                   Option.equal ( == )
                                     (Frame.location_local_source location)
                                     (Some local))
                                 (Frame.function_locations frame)
                            && matches (Typed.initializer_value root) ast)
                      in
                      validate (Typed.initializer_value root);
                      Some (Lower.Initialize root))
                declaration.local_declarators
              |> List.filter_map Fun.id
            in
            incr declarations;
            Lower.Block lowered
        | If_statement ast ->
            let value = condition Source.If_condition ast.if_condition in
            let yes = statement (depth - 1) ast.if_then_branch in
            let no =
              Option.map
                (fun clause -> statement (depth - 1) clause.Ast.else_branch)
                ast.if_else_clause
            in
            Lower.If (value, yes, no)
        | While_statement ast ->
            let value = condition Source.While_condition ast.while_condition in
            let body = statement (depth - 1) ast.while_body in
            Lower.While (value, body)
        | Do_while_statement ast ->
            let body = statement (depth - 1) ast.do_body in
            let value =
              condition Source.Do_while_condition ast.do_while_condition
            in
            Lower.Do_while (body, value)
        | For_statement ast ->
            let initial = statement (depth - 1) ast.for_initializer in
            let value = condition Source.For_condition ast.for_condition in
            let update = Option.map (statement (depth - 1)) ast.for_update in
            let body = statement (depth - 1) ast.for_body in
            Lower.For (initial, value, update, body)
        | Break_statement ast -> Lower.Break ast.break_location.span
        | _ -> raise Unproved
      in
      let statements =
        List.map (statement 32) (Option.to_list definition.body)
      in
      require
        (!initializers = [] && !expressions = [] && !conditions = []
       && !returns = []
        && List.length !seen = List.length (Typed.function_all_results source)
        && List.length !seen_calls = List.length calls);
      let canonical =
        match
          Lower.lower ~frame ~function_calls:targets
            ~span:definition.location.span statements
        with
        | Ok graph -> X87_stack.graph graph
        | Error _ -> raise Unproved
      in
      let actual = Function_body.body body in
      require
        (Seq.Block_id.equal
           (Block_graph.block_id (Block_graph.entry canonical))
           (Block_graph.block_id (Block_graph.entry actual)));
      require
        (same_list
           (fun expected actual ->
             Seq.Block_id.equal
               (Block_graph.block_id expected)
               (Block_graph.block_id actual)
             && same_list Seq.Block_id.equal
                  (Block_graph.successors expected)
                  (Block_graph.successors actual)
             && same_list
                  (fun expected actual ->
                    equal_stamp
                      (get (instruction_stamp expected))
                      (get (instruction_stamp actual)))
                  (descriptions expected) (descriptions actual))
           (Block_graph.blocks canonical)
           (Block_graph.blocks actual));
      Some statements
    with Unproved -> None

  let authenticate ?runtime ~frame ~body () =
    Option.is_some (reconstruct ?runtime ~frame ~body ())
end

let authenticates_mixed_scalar_source ~runtime ~frame ~body =
  Mixed_scalar_source.authenticate ~runtime ~frame ~body ()

(* OptPass3 scores member accesses across the whole function, not per basic
   block. Check that all automatic candidates fit the remaining variable banks,
   without relying on score tie ordering. Keep updates statement-only. Dynamic operands, bounded add/subtract/multiply
   and shift literals, and discarded increment/decrement have source-backed
   storage behavior; other immediate rewrites and compound-expression results
   remain outside the proof. Named calls retain their original typed targets;
   OptPass789A saves and restores selected variable registers around each ordinary
   activation. IC_CALL_INDIRECT2 is admitted only for a named definition's JIT
   address slot, not for a callback. Locals require a single source-owned initializer in the entry block, with
   local dependencies initialized before each read. *)
let analyze_integer_function ?exact ?runtime ?(mixed = false) ~frame ~body () =
  let locations = Frame.function_locations frame in
  let integer_member location =
    (Frame.location_kind location = Frame.Named_parameter
    || Frame.location_kind location = Frame.Automatic_local)
    && Frame.location_value_shape location = Frame.Scalar
    && Frame.location_declarator_shape location = Frame.Object
    && Frame.location_dimensions location = []
    && Sema.Type.pointer_depth (Frame.location_checked_type location) = 0
    && (match Sema.Type.base (Frame.location_checked_type location) with
      | Sema.Type.Primitive
          (_, (Sema.Primitive_type.I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64))
        -> true
      | Sema.Type.Primitive (_, Sema.Primitive_type.F64) -> mixed
      | _ -> false)
    && Option.is_some (Frame.location_frame_slot location)
  in
  let blocks = snapshot body in
  let code =
    List.concat_map (fun (_, instructions, _) -> instructions) blocks
  in
  let update = function
    | O.Ic_shl_equ
    | Ic_shr_equ
    | Ic_add_equ
    | Ic_sub_equ
    | Ic_mul_equ
    | Ic_div_equ
    | Ic_mod_equ
    | Ic_and_equ
    | Ic_or_equ
    | Ic_xor_equ -> true
    | _ -> false
  in
  let unary_update = function
    | O.Ic_pp_ | Ic_mm_ | Ic__pp | Ic__mm -> true
    | _ -> false
  in
  let branch = function
    | O.Ic_br_zero | Ic_br_not_zero -> true
    | _ -> false
  in
  let module Typed = Sema.Function_call_expression_result in
  let module Calls = Sema.Function_call_resolution in
  let module Policy = Sema.Function_call_conversion_policy in
  let module Resolution = Sema.Function_resolution in
  let source_calls =
    Option.fold ~none:[] ~some:Typed.function_calls
      (Function_body.definition_source body)
  in
  let numeric_target call =
    mixed
    && Option.fold ~none:false
         ~some:(fun runtime ->
           Option.fold ~none:false
             ~some:
               (List.exists (fun target ->
                    Sema.Function_call_target_classification.source target
                    == call
                    && Option.fold ~none:false ~some:Numeric_intrinsic.supports
                         (Runtime_call_context.intrinsic_opcode_of_source
                            (Runtime_call_context.Function_call target))))
             (Runtime_call_context.function_mixed_scalar_call_targets runtime
                ~body))
         runtime
  in
  let direct_target = function
    | Typed.Direct_call_result call ->
        let source = Typed.direct_source call |> Policy.direct_source in
        let declaration = Typed.direct_declaration call in
        if
          (Resolution.declaration_site_kind
             (Resolution.resolved_declaration_site declaration)
           = Resolution.Definition
          || numeric_target call)
          && Option.is_none
               (Sema.Function_type_resolution.function_variadic_bindings
                  (Calls.direct_active_header source))
          && List.for_all
               (fun argument ->
                 match Typed.fixed_path argument with
                 | Typed.Provided_result _ -> true
                 | Declared_default_result _ -> false)
               (Typed.direct_fixed_results call)
        then Some (Calls.direct_target_symbol source)
        else None
    | _ -> None
  in
  let targets = List.filter_map direct_target source_calls in
  let direct_call = function
    | O.Ic_call_start | Ic_call | Ic_call_indirect2 | Ic_call_end -> true
    | _ -> false
  in
  let scalar_result (item : Seq.description) =
    Option.fold ~none:false
      ~some:(fun type_ ->
        Sema.Type.pointer_depth type_ = 0
        &&
        match Sema.Type.base type_ with
        | Sema.Type.Primitive
            ( _,
              (Sema.Primitive_type.I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64)
            ) -> true
        | Sema.Type.Primitive (_, Sema.Primitive_type.F64) -> mixed
        | _ -> false)
      item.target_type
  in
  let call_result item =
    scalar_result item
    || Option.fold ~none:false
         ~some:(fun type_ ->
           Sema.Type.pointer_depth type_ = 0
           &&
           match Sema.Type.base type_ with
           | Sema.Type.Primitive (_, (Sema.Primitive_type.I0 | U0)) -> true
           | _ -> false)
         item.Seq.target_type
  in
  let allowed (item : Seq.description) =
    (if item.opcode = O.Ic_end_exp then item.flags = 0x200L
     else
       let flags =
         if mixed then Int64.logand item.flags (Int64.lognot 3L) else item.flags
       in
       flags = 0L || (flags = 0x2000L && scalar_result item))
    && (if direct_call item.opcode then
          (match item.payload with
            | Some (Seq.Symbol symbol) -> List.exists (( == ) symbol) targets
            | _ -> false)
          && (item.opcode = O.Ic_call_start || call_result item)
        else true)
    && Option.is_some (instruction_stamp item)
    && (update item.opcode || unary_update item.opcode || branch item.opcode
       ||
       match item.opcode with
       | O.Ic_call_start
       | Ic_call
       | Ic_call_indirect2
       | Ic_call_end
       | Ic_add_rsp
       | Ic_add_rsp1
       | O.Ic_rbp
       | Ic_imm_i64
       | Ic_add
       | Ic_sub
       | Ic_mul
       | Ic_div
       | Ic_mod
       | Ic_and
       | Ic_or
       | Ic_xor
       | Ic_shl
       | Ic_shr
       | Ic_shl_const
       | Ic_shr_const
       | Ic_com
       | Ic_not
       | Ic_unary_minus
       | Ic_equ_equ
       | Ic_not_equ
       | Ic_less
       | Ic_less_equ
       | Ic_greater
       | Ic_greater_equ
       | Ic_deref
       | Ic_assign
       | Ic_end_exp
       | Ic_return_val
       | Ic_jmp
       | Ic_ret -> true
       | Ic_imm_f64 -> mixed
       | Ic_to_i64 | Ic_to_f64 ->
           mixed
           && Option.fold ~none:false
                ~some:(fun runtime ->
                  Option.fold ~none:false
                    ~some:(fun receipt ->
                      Runtime_call_context.intrinsic_opcode receipt
                      = item.opcode)
                    (Runtime_call_context.find_intrinsic_instruction runtime
                       ~owner:(Runtime_call_context.Function body)
                       item.instruction_id))
                runtime
       | _ -> false)
  in
  if
    Option.is_none (Function_body.definition_declaration body)
    || (not (Function_body.definition_matches_frame body frame))
    || (not (supported_options body))
    || Int64.logand
         (Function_body.stored_flags body)
         (Int64.lognot Function_body.ordinary_calling_flag_mask)
       <> 0L
    || locations = []
    || List.length locations > 32
    || (not (List.for_all integer_member locations))
    || (not (safe_register_requests locations))
    || List.length blocks > 64
    || List.length code > 512
    || (mixed && not (Mixed_scalar_source.authenticate ?runtime ~frame ~body ()))
    || List.length targets <> List.length source_calls
    || not (List.for_all allowed code)
  then None
  else
    let produced =
      List.fold_left
        (fun map (item : Seq.description) ->
          match item.result with
          | None -> map
          | Some result -> Values.add result.value_id item map)
        Values.empty code
    in
    let users id =
      List.filter
        (fun (item : Seq.description) ->
          List.exists (Seq.Value_id.equal id) item.operands)
        code
    in
    let location_at offset =
      List.find_opt
        (fun location ->
          Frame.frame_slot_displacement
            (Option.get (Frame.location_frame_slot location))
          = offset)
        locations
    in
    let frame_address (item : Seq.description) =
      match (item.opcode, item.operands, item.result) with
      | O.Ic_add, [ left; right ], Some result ->
          let rbp id =
            Option.fold ~none:false
              ~some:(fun (d : Seq.description) -> d.opcode = O.Ic_rbp)
              (Values.find_opt id produced)
          in
          let offset id =
            Option.bind (Values.find_opt id produced)
              (fun (d : Seq.description) ->
                match (d.opcode, d.payload) with
                | O.Ic_imm_i64, Some (Seq.Integer n) -> Some n
                | _ -> None)
          in
          let offset =
            if rbp left then offset right
            else if rbp right then offset left
            else None
          in
          Option.bind offset (fun n ->
              Option.bind (location_at n) (fun location ->
                  Option.bind item.target_type (fun pointer ->
                      match Sema.Type.dereference pointer with
                      | Ok actual
                        when Sema.Type.equal actual
                               (Frame.location_checked_type location) ->
                          Some (result.value_id, location)
                      | _ -> None)))
      | _ -> None
    in
    let addresses = List.filter_map frame_address code in
    let used location =
      List.exists (fun (_, original) -> original == location) addresses
    in
    let register_capacity_fits, forced_capacity_fits =
      let explicit =
        List.filter_map
          (fun location ->
            match Frame.location_register_selection location with
            | Request.Explicit register ->
                Some (Request.explicit_register_number register)
            | _ -> None)
          locations
      in
      let available registers =
        List.length
          (List.filter
             (fun register -> not (List.mem register explicit))
             registers)
      in
      let candidates =
        List.fold_left
          (fun count location ->
            match Frame.location_register_selection location with
            | Request.Allocatable -> count + 1
            | Request.Unspecified
              when used location && automatic_candidate location -> count + 1
            | _ -> count)
          0 locations
      in
      let general = available general_variable_registers in
      (* OptPass3 opens the nonpointer bank only when there are more than six
         scored candidates, not when explicit requests exhaust the general
         bank. Require capacity even if optimization removes some accesses. *)
      let nonpointer = available nonpointer_variable_registers in
      let forced =
        List.fold_left
          (fun count location ->
            if Frame.location_register_selection location = Request.Allocatable
            then count + 1
            else count)
          0 locations
      in
      ( registers_disabled body || candidates <= general
        || (general = 6 && candidates <= general + nonpointer),
        forced <= general || (forced > 6 && forced <= general + nonpointer) )
    in
    (* REG_ALLOC starts at I64_MAX/2 in OptPass3, so these bounded functions
       cannot score an ordinary member ahead of a plain reg member. When the
       ordinary members overflow the bank, retain their unknown selection:
       only 64-bit ordinary values are independent of that choice. *)
    let pressure_safe =
      (not mixed) && forced_capacity_fits
      && List.for_all
           (fun location ->
             Frame.location_register_selection location <> Request.Unspecified
             || (not (used location))
             ||
             match Sema.Type.base (Frame.location_checked_type location) with
             | Sema.Type.Primitive (_, (Sema.Primitive_type.I64 | U64)) -> true
             | _ -> false)
           locations
    in
    let address id =
      List.find_opt (fun (other, _) -> Seq.Value_id.equal id other) addresses
    in
    let load id =
      Option.bind (Values.find_opt id produced) (fun (item : Seq.description) ->
          match (item.opcode, item.operands) with
          | O.Ic_deref, [ target ] -> address target
          | _ -> None)
    in
    let safe_address (id, location) =
      let consumers = users id in
      consumers <> []
      && List.for_all
           (fun (item : Seq.description) ->
             Option.fold ~none:false
               ~some:(Sema.Type.equal (Frame.location_checked_type location))
               item.target_type
             &&
             match (item.opcode, item.operands) with
             | O.Ic_deref, [ target ] -> Seq.Value_id.equal target id
             | opcode, [ target ] when unary_update opcode ->
                 Seq.Value_id.equal target id
             | opcode, [ target; _ ] when update opcode || opcode = O.Ic_assign
               -> Seq.Value_id.equal target id
             | _ -> false)
           consumers
    in
    let local_initializers =
      Option.fold ~none:[] ~some:Typed.function_initializers
        (Function_body.definition_source body)
    in
    let entry_code =
      Function_body.body body |> Block_graph.entry |> descriptions
    in
    let binding_matches value location =
      match Calls.argument_expression_kind (Typed.result_source value) with
      | Calls.Bound_identifier_expression bound -> (
          match
            Sema.Module_expression_binding.occurrence_resolution
              (Calls.bound_identifier_occurrence bound)
          with
          | Sema.Module_expression_binding.Local_binding binding ->
              binding == Frame.location_binding location
          | _ -> false)
      | _ -> false
    in
    let matches_operand ?(normalize_shift = false) value operand =
      match load operand with
      | Some (_, location) -> binding_matches value location
      | None -> (
          match
            ( Calls.argument_expression_kind (Typed.result_source value),
              Values.find_opt operand produced )
          with
          | ( Calls.Integer_literal bits,
              Some
                {
                  opcode = O.Ic_imm_i64;
                  payload = Some (Seq.Integer actual);
                  _;
                } ) ->
              actual = if normalize_shift then Int64.logand bits 63L else bits
          | ( Calls.Float_literal bits,
              Some
                {
                  opcode = O.Ic_imm_f64;
                  payload = Some (Seq.Float_bits actual);
                  _;
                } ) -> mixed && actual = bits
          | _ -> false)
    in
    let rec matches_expression_operand ?(allowed_load = fun _ _ -> true) depth
        value operand =
      if depth = 0 then false
      else if
        Option.fold ~none:false
          ~some:(fun (_, location) -> not (allowed_load operand location))
          (load operand)
      then false
      else if matches_operand value operand then true
      else if
        match Calls.argument_expression_kind (Typed.result_source value) with
        | Calls.Parenthesized_expression _ -> true
        | Calls.Prefix_expression prefix ->
            Calls.prefix_operator prefix = Calls.Unary_plus
        | _ -> false
      then
        Option.fold ~none:false
          ~some:(fun inner ->
            matches_expression_operand ~allowed_load (depth - 1) inner operand)
          (Typed.result_operand value)
      else
        match
          ( Calls.argument_expression_source (Typed.result_source value),
            Typed.result_binary_operands value,
            Values.find_opt operand produced )
        with
        | ( Some (Frontend.Ast.Binary_expression binary),
            Some (left, right),
            Some expression )
          when expression.opcode = O.Ic_shl_const
               || expression.opcode = O.Ic_shr_const -> (
            expression.span = Some binary.binary_operator.operator_location.span
            && (match
                  (binary.binary_operator_spec.spelling, expression.opcode)
                with
              | "<<", O.Ic_shl_const | ">>", Ic_shr_const -> true
              | _ -> false)
            &&
            match
              ( expression.operands,
                expression.payload,
                Calls.argument_expression_kind (Typed.result_source right) )
            with
            | [ input ], Some (Seq.Integer count), Calls.Integer_literal bits ->
                count = bits
                && matches_expression_operand ~allowed_load (depth - 1) left
                     input
            | _ -> false)
        | ( Some (Frontend.Ast.Binary_expression binary),
            Some (left, right),
            Some expression ) -> (
            expression.payload = None
            && expression.span
               = Some binary.binary_operator.operator_location.span
            && (match
                  (binary.binary_operator_spec.spelling, expression.opcode)
                with
              | "+", O.Ic_add
              | "-", Ic_sub
              | "*", Ic_mul
              | "/", Ic_div
              | "%", Ic_mod
              | "<<", Ic_shl
              | ">>", Ic_shr
              | "==", Ic_equ_equ
              | "!=", Ic_not_equ
              | "<", Ic_less
              | "<=", Ic_less_equ
              | ">", Ic_greater
              | ">=", Ic_greater_equ
              | "&", Ic_and
              | "|", Ic_or
              | "^", Ic_xor -> true
              | _ -> false)
            &&
            match expression.operands with
            | [ left_operand; right_operand ] ->
                matches_expression_operand ~allowed_load (depth - 1) left
                  left_operand
                && matches_expression_operand ~allowed_load (depth - 1) right
                     right_operand
            | _ -> false)
        | Some (Frontend.Ast.Prefix_expression prefix), None, Some expression
          -> (
            expression.payload = None
            && expression.span
               = Some prefix.prefix_operator.operator_location.span
            && (match (prefix.prefix_operator_kind, expression.opcode) with
              | Frontend.Ast.Unary_minus, O.Ic_unary_minus
              | Frontend.Ast.Bitwise_not, Ic_com
              | Frontend.Ast.Logical_not, Ic_not -> true
              | _ -> false)
            &&
            match (Typed.result_operand value, expression.operands) with
            | Some inner, [ input ] ->
                matches_expression_operand ~allowed_load (depth - 1) inner input
            | _ -> false)
        | Some (Frontend.Ast.Call_expression call), _, Some expression
          when mixed || Option.is_some exact ->
            expression.opcode = O.Ic_call_end
            && expression.span = Some call.call_location.span
            && Option.equal Sema.Type.equal (Typed.result_type value)
                 expression.target_type
            && (match
                  (Typed.result_call_resolution value, expression.payload)
                with
              | Some (Calls.Direct_call direct), Some (Seq.Symbol symbol) ->
                  Calls.direct_target_symbol direct == symbol
              | _ -> false)
            && Option.fold ~none:false
                 ~some:(fun runtime ->
                   let call =
                     List.find_map
                       (fun (start : Seq.description) ->
                         if start.opcode <> O.Ic_call_start then None
                         else
                           Option.bind
                             (Runtime_call_context.find_start runtime
                                ~owner:(Runtime_call_context.Function body)
                                start.instruction_id) (fun call ->
                               if
                                 Seq.Instruction_id.equal
                                   (Runtime_call_context.last call)
                                   expression.instruction_id
                               then Some call
                               else None))
                       code
                   in
                   Option.fold ~none:false
                     ~some:(fun call ->
                       let first =
                         Seq.Instruction_id.to_int
                           (Runtime_call_context.first call)
                       and last =
                         Seq.Instruction_id.to_int
                           (Runtime_call_context.last call)
                       in
                       (* Canonical call ownership does not bypass initializer
                      ordering: check every transitive original argument read. *)
                       List.for_all
                         (fun (item : Seq.description) ->
                           let id =
                             Seq.Instruction_id.to_int item.instruction_id
                           in
                           if
                             id < first || id > last
                             || item.opcode <> O.Ic_deref
                           then true
                           else
                             Option.fold ~none:false
                               ~some:(fun result ->
                                 Option.fold ~none:false
                                   ~some:(fun (_, location) ->
                                     allowed_load result.Seq.value_id location)
                                   (load result.Seq.value_id))
                               item.result)
                         code)
                     call)
                 runtime
        | _ -> false
    in
    let original_initializer_location (item : Seq.description) =
      match (item.opcode, item.operands, item.result) with
      | O.Ic_assign, [ target; _ ], Some result when item.payload = None -> (
          match address target with
          | Some (_, location)
            when List.exists (( == ) item) entry_code
                 &&
                 match users result.value_id with
                 | [ consumer ] -> consumer.opcode = O.Ic_end_exp
                 | _ -> false -> (
              match Values.find_opt target produced with
              | Some address
                when List.exists
                       (fun initialization ->
                         Typed.initializer_target_symbol initialization
                         == Frame.location_symbol location
                         &&
                         match
                           Sema.Symbol.origin
                             (Sema.Local_type_resolution.local_symbol
                                (Calls.initializer_local
                                   (Typed.initializer_source initialization)))
                         with
                         | Sema.Symbol.Source_location { span; _ } ->
                             address.span = Some span
                         | _ -> false)
                       local_initializers -> Some location
              | _ -> None)
          | _ -> None)
      | _ -> None
    in
    let entry_position item =
      let rec find index = function
        | [] -> None
        | candidate :: rest ->
            if candidate == item then Some index else find (index + 1) rest
      in
      find 0 entry_code
    in
    let precedes left right =
      match (entry_position left, entry_position right) with
      | Some left, Some right -> left < right
      | _ -> false
    in
    let initializer_matches item location operand =
      (* PrsVar parses automatic initialization as an ordinary expression.
         Earlier initialized locals are available, but a declaration address
         alone does not prove that its store preceded this read. Every local's
         initializer is independently authenticated by initialized_once below. *)
      let allowed_load operand dependency =
        Frame.location_kind dependency = Frame.Named_parameter
        || Option.fold ~none:false
             ~some:(fun read ->
               precedes read item
               && List.exists
                    (fun store ->
                      Option.fold ~none:false ~some:(( == ) dependency)
                        (original_initializer_location store)
                      && precedes store read)
                    entry_code)
             (Values.find_opt operand produced)
      in
      List.exists
        (fun initialization ->
          Typed.initializer_target_symbol initialization
          == Frame.location_symbol location
          &&
          let source = Typed.initializer_value initialization in
          matches_expression_operand ~allowed_load 32 source operand)
        local_initializers
    in
    let initialization (item : Seq.description) =
      match (original_initializer_location item, item.operands) with
      | Some location, [ _; operand ] ->
          initializer_matches item location operand
      | _ -> false
    in
    let initialized_once location =
      if Frame.location_kind location = Frame.Named_parameter then true
      else
        let stores =
          List.filter
            (fun (item : Seq.description) ->
              match (item.opcode, item.operands) with
              | O.Ic_assign, target :: _ -> (
                  match address target with
                  | Some (_, other) -> other == location
                  | None -> false)
              | _ -> false)
            code
        in
        match List.filter initialization stores with
        | [ _ ] -> true
        | _ -> false
    in
    let source_matches_update (item : Seq.description) =
      let span_matches (operator : Frontend.Ast.expression_operator) =
        item.span = Some operator.operator_location.span
      in
      let matches value =
        match
          ( Calls.argument_expression_source (Typed.result_source value),
            item.operands )
        with
        | Some (Frontend.Ast.Binary_expression binary), [ target; operand ] -> (
            match (address target, Typed.result_binary_operands value) with
            | Some (_, location), Some (left, right) -> (
                span_matches binary.binary_operator
                && binding_matches left location
                && (matches_operand
                      ~normalize_shift:
                        (item.opcode = O.Ic_shl_equ
                       || item.opcode = O.Ic_shr_equ)
                      right operand
                   || matches_expression_operand 32 right operand)
                &&
                match (binary.binary_operator_spec.spelling, item.opcode) with
                | "=", O.Ic_assign
                | "+=", O.Ic_add_equ
                | "-=", Ic_sub_equ
                | "*=", Ic_mul_equ
                | "/=", Ic_div_equ
                | "%=", Ic_mod_equ
                | "<<=", Ic_shl_equ
                | ">>=", Ic_shr_equ
                | "&=", Ic_and_equ
                | "|=", Ic_or_equ
                | "^=", Ic_xor_equ -> true
                | _ -> false)
            | _ -> false)
        | Some (Frontend.Ast.Prefix_expression prefix), [ target ] -> (
            match (address target, Typed.result_operand value) with
            | Some (_, location), Some operand -> (
                span_matches prefix.prefix_operator
                && binding_matches operand location
                &&
                match (prefix.prefix_operator_kind, item.opcode) with
                | Frontend.Ast.Pre_increment, O.Ic_pp_
                | Frontend.Ast.Pre_decrement, Ic_mm_ -> true
                | _ -> false)
            | _ -> false)
        | Some (Frontend.Ast.Postfix_expression postfix), [ target ] -> (
            match (address target, Typed.result_operand value) with
            | Some (_, location), Some operand -> (
                span_matches postfix.postfix_operator
                && binding_matches operand location
                &&
                match (postfix.postfix_operator_kind, item.opcode) with
                | Frontend.Ast.Post_increment, O.Ic__pp
                | Frontend.Ast.Post_decrement, Ic__mm -> true
                | _ -> false)
            | _ -> false)
        | _ -> false
      in
      item.payload = None
      && Option.fold ~none:false
           ~some:(fun source ->
             List.exists matches (Typed.function_all_results source))
           (Function_body.definition_source body)
    in
    let literal_operand opcode operand =
      match Values.find_opt operand produced with
      | Some { opcode = O.Ic_imm_i64; payload = Some (Seq.Integer n); _ } -> (
          match opcode with
          | O.Ic_add_equ | Ic_sub_equ | Ic_mul_equ ->
              n >= 0L && n <= Int64.of_int32 Int32.max_int
          | O.Ic_shl_equ | Ic_shr_equ -> n >= 0L && n < 64L
          | _ -> false)
      | _ -> false
    in
    let discarded result =
      match users result.Seq.value_id with
      | [ consumer ] -> consumer.opcode = O.Ic_end_exp
      | _ -> false
    in
    let safe_instruction (item : Seq.description) =
      match (item.opcode, item.operands, item.result) with
      | O.Ic_rbp, [], Some result ->
          let consumers = users result.value_id in
          consumers <> []
          && List.for_all
               (fun item -> Option.is_some (frame_address item))
               consumers
      | O.Ic_assign, [ target; _ ], Some result ->
          initialization item
          || Option.fold ~none:false
               ~some:(fun (_, location) ->
                 source_matches_update item
                 (* OptPass4 makes these direct frame assignments by-value.
                    BackC reloads the destination: selected registers preserve
                    the full RHS, proven stack slots reload at their width.
                    Unknown pressure must not acquire either result rule. *)
                 && (discarded result
                    || stack_selected ~body location
                    || register_capacity_fits
                    || Frame.location_register_selection location
                       <> Request.Unspecified))
               (address target)
      | O.Ic_assign, _, _ -> false
      | O.Ic_deref, [ target ], _ -> Option.is_some (address target)
      | opcode, [ target; operand ], Some result when update opcode ->
          Option.is_some (address target)
          && (Option.is_some (load operand)
             || literal_operand opcode operand
             || Option.fold ~none:false
                  ~some:(fun (expression : Seq.description) ->
                    match expression.opcode with
                    | O.Ic_add
                    | Ic_sub
                    | Ic_mul
                    | Ic_div
                    | Ic_mod
                    | Ic_shl
                    | Ic_shr
                    | Ic_shl_const
                    | Ic_shr_const
                    | Ic_and
                    | Ic_or
                    | Ic_xor
                    | Ic_equ_equ
                    | Ic_not_equ
                    | Ic_less
                    | Ic_less_equ
                    | Ic_greater
                    | Ic_greater_equ
                    | Ic_unary_minus
                    | Ic_com
                    | Ic_not -> true
                    | _ -> false)
                  (Values.find_opt operand produced))
          && source_matches_update item && discarded result
      | opcode, [ target ], Some result when unary_update opcode ->
          Option.is_some (address target)
          && source_matches_update item && discarded result
      | opcode, _, _ when update opcode || unary_update opcode -> false
      | _ -> true
    in
    if
      not
        ((Option.is_some exact || register_capacity_fits || pressure_safe)
        && List.for_all safe_address addresses
        && List.for_all initialized_once locations
        && List.for_all safe_instruction code)
    then None
    else
      let exact_registers =
        Option.map
          (fun members ->
            let explicit =
              List.filter_map
                (fun (location, _) ->
                  match Frame.location_register_selection location with
                  | Request.Explicit register ->
                      Some
                        ( Frame.location_symbol location,
                          Request.explicit_register_number register )
                  | _ -> None)
                members
            in
            if registers_disabled body then explicit
            else
              let candidates =
                List.filter_map
                  (fun (index, (location, occurrences)) ->
                    let seed =
                      match Frame.location_register_selection location with
                      (* OptPass3.HC:29-42 excludes ordinary F64 members before
                         QSort. Including one at score zero would still change
                         the unstable order of tied integer/plain-reg members.
                         Ordinary zero-score integers must remain candidates. *)
                      | Request.Unspecified
                        when not
                               (f64_type (Frame.location_checked_type location))
                        -> Some 0L
                      | Allocatable -> Some (Int64.div Int64.max_int 2L)
                      | Unspecified | Disabled | Explicit _ -> None
                    in
                    Option.map
                      (fun seed ->
                        {
                          Scored_register_allocation.member = index;
                          (* PrsExp.HC:800-802 emits one frame address for each
                             original member occurrence. OptPass3.HC:187-198
                             adds one score/LEA, then :119-135,149-163,223-244
                             adds one score and cancels that LEA for its sole
                             authenticated read/write consumer. Initializers
                             count as writes. The 32-member/512-instruction
                             bound cannot overflow the REG_ALLOC seed. *)
                          score =
                            Int64.add seed (Int64.of_int (2 * occurrences));
                          offset =
                            Frame.frame_slot_displacement
                              (Option.get (Frame.location_frame_slot location));
                          lea_balance = 0L;
                          pointer_depth = 0;
                          has_dimensions = false;
                          name =
                            Sema.Symbol.name (Frame.location_symbol location);
                        })
                      seed)
                  (List.mapi (fun index member -> (index, member)) members)
              in
              explicit
              @ List.map
                  (fun (index, number) ->
                    ( Frame.location_symbol (fst (List.nth members index)),
                      number ))
                  (Scored_register_allocation.allocate
                     ~reserved:(List.map snd explicit) candidates))
          exact
      in
      Some
        {
          frame_ = frame;
          parameters_ = List.map Fun.id (signature_parameters frame);
          parameter_sources_ =
            Option.map (List.map Fun.id) (source_parameters frame);
          body_ = body;
          definition_ = Function_body.definition_declaration body;
          source_ = Function_body.definition_source body;
          options_ = Function_body.compiler_options body;
          flags_ = Function_body.stored_flags body;
          locations_ =
            List.map
              (fun location -> Option.get (location_stamp ~frame location))
              locations;
          blocks_ = blocks;
          stamps_ =
            List.map
              (fun item -> (item, Option.get (instruction_stamp item)))
              code;
          selections_ =
            List.map
              (fun location ->
                ( Frame.location_symbol location,
                  if mixed && stack_selected ~body location then Stack
                  else if
                    (not (used location))
                    && not
                         (mixed
                         &&
                         match Frame.location_register_selection location with
                         | Request.Allocatable | Explicit _ -> true
                         | Unspecified | Disabled -> false)
                  then Unknown
                  else if stack_selected ~body location then Stack
                  else if Option.is_some exact_registers then
                    if
                      List.exists
                        (fun (symbol, _) ->
                          symbol == Frame.location_symbol location)
                        (Option.get exact_registers)
                    then Register_candidate
                    else Stack
                  else if
                    (not register_capacity_fits)
                    && Frame.location_register_selection location
                       = Request.Unspecified
                  then Unknown
                  else Register_candidate ))
              locations;
          exact_registers_ = exact_registers;
          shadowed_explicit_ = [];
          explicit_conflicts_ = [];
          runtime_ = runtime;
          exact_source_ =
            Option.bind exact (fun _ -> Exact_register_scores.stamp_source body);
        }

let complete_parameter_coverage ~frame =
  let module Headers = Sema.Function_type_resolution in
  let parameters = signature_parameters frame in
  let named_locations =
    List.filter
      (fun location -> Frame.location_kind location = Frame.Named_parameter)
      (Frame.function_locations frame)
  in
  (* PrsVar.HC:341-344,525-529 creates an original _anon_ member for every
     unnamed formal. Name lookup may select the first such member, but all of
     them must retain their own frame slot, binding and register request. *)
  List.length parameters = List.length named_locations
  && same_list
       (fun binding parameter ->
         Headers.parameter_binding_index binding
         = Headers.parameter_index parameter
         && Option.equal ( == )
              (Headers.parameter_binding_source binding)
              (Headers.parameter_source parameter))
       (Headers.function_parameter_bindings (Frame.function_header frame))
       parameters
  && List.for_all
       (fun (index, parameter) ->
         Headers.parameter_index parameter = index
         && Option.fold ~none:false
              ~some:(fun (source : Frontend.Ast.function_parameter) ->
                Headers.parameter_name parameter
                = Option.map
                    (fun (name : Frontend.Ast.identifier) -> name.spelling)
                    source.name)
              (Headers.parameter_source parameter))
       (List.mapi (fun index parameter -> (index, parameter)) parameters)
  (* Legacy isolated compilation and source-task compilation retain the same
     independent whole-list evidence, without inventing a parser receipt for
     the legacy path. Missing or omitted original children reject. *)
  && Option.fold ~none:false
       ~some:(fun sources ->
         same_list
           (fun (source : Frontend.Ast.function_parameter) parameter ->
             Option.equal ( == ) (Some source)
               (Headers.parameter_source parameter))
           sources parameters)
       (source_parameters frame)
  && List.for_all
       (fun parameter ->
         List.length
           (List.filter
              (fun location ->
                match source_requests ~frame location with
                | Some { owner = Parameter_owner original; _ } ->
                    original == parameter
                    && Sema.Symbol.name (Frame.location_symbol location)
                       = Headers.parameter_lookup_name parameter
                    && Sema.Symbol.origin (Frame.location_symbol location)
                       = Headers.parameter_lookup_origin parameter
                | _ -> false)
              named_locations)
         = 1)
       parameters

let source_owned_requests ~frame ~body (original : location_stamp) =
  let members =
    match original.kind with
    | Frame.Named_parameter -> Function_body.parameters body
    | Automatic_local -> Function_body.locals body
    | Variadic_argc | Variadic_argv | Static_local -> []
  in
  Option.is_some (Function_body.definition_source body)
  && Option.fold ~none:false
       ~some:(fun definition ->
         Sema.Function_resolution.resolved_declaration_site definition
         |> Sema.Function_resolution.declaration_site_function
         |> ( == ) (Frame.function_header frame))
       (Function_body.definition_declaration body)
  && List.exists
       (fun member -> Function_body.member_symbol member == original.symbol)
       members
  &&
  match original.source_requests with
  | None -> false
  | Some source -> (
      (* Every exclusion and reservation matters: authenticate all requests,
         not only the member whose physical register the caller requested. *)
      List.for_all
        (fun (request : source_request_stamp) ->
          (match request.origin with
            | Source_origin _ -> true
            | _ -> false)
          && request.spelling = Request.kind_name request.kind
          && Option.is_some request.selection)
        source.requests
      &&
      match List.rev source.requests with
      | [] -> original.request = Unspecified_request
      | request :: _ -> (
          request.selection = Some original.request
          && Option.equal ( == ) request.explicit_owner original.request_owner
          &&
          match original.request with
          | Explicit_request (spelling, number, Source_origin _) ->
              Request.canonical_u64_register_number spelling = Some number
          | Allocatable_request | Disabled_request -> true
          | Unspecified_request | Explicit_request _ -> false))

(* This proof keeps member selection separate from readable storage. The
   canonical graph retains every frame load/store and its runtime initialization
   checks. A register home for an uninitialized local is not an initialized value.
   Pointer homes contain private descriptor pointers, never language addresses. *)
let analyze_owned_members ?runtime ~frame ~body () =
  let locations = Frame.function_locations frame in
  if
    (not (supported_options body && supported_register_requests locations))
    || Int64.logand
         (Function_body.stored_flags body)
         (Int64.lognot Function_body.ordinary_calling_flag_mask)
       <> 0L
    || not
         (List.for_all
            (fun location ->
              Option.fold ~none:false
                ~some:(source_owned_requests ~frame ~body)
                (location_stamp ~frame location))
            locations)
  then None
  else
    Option.bind
      (Mixed_scalar_source.reconstruct ?runtime ~owned:true ~frame ~body ())
      (fun statements ->
        Option.bind
          (Exact_register_scores.analyze_members ~frame ~body ~statements)
          (fun members ->
            (* Explicit requests bypass OptPass3's balance filtering. Their
               stack/register coexistence is not represented: reject, never spill
               or substitute a different requested physical home. *)
            if
              List.exists
                (fun (member : Exact_register_scores.member_score) ->
                  member.lea_balance <> 0L
                  &&
                  match Frame.location_register_selection member.location with
                  | Request.Explicit _ -> true
                  | _ -> false)
                members
            then None
            else
              let resolution =
                List.mapi
                  (fun index (member : Exact_register_scores.member_score) ->
                    match Frame.location_register_selection member.location with
                    | Request.Explicit register ->
                        Some (index, Request.explicit_register_number register)
                    | _ -> None)
                  members
                |> List.filter_map Fun.id
                |> Scored_register_allocation.resolve_explicit
              in
              let symbol index =
                Frame.location_symbol (List.nth members index).location
              in
              let symbol_registers =
                List.map (fun (index, number) -> (symbol index, number))
              in
              let explicit = symbol_registers resolution.owners in
              let shadowed = symbol_registers resolution.shadowed in
              let conflicts =
                List.map
                  (fun (conflict : Scored_register_allocation.explicit_conflict)
                     ->
                    let location =
                      (List.nth members conflict.replacement_member).location
                    in
                    let origin =
                      match Frame.location_register_selection location with
                      | Request.Explicit register ->
                          Request.explicit_register_origin register
                      | _ -> assert false
                    in
                    {
                      register = conflict.register;
                      previous = symbol conflict.previous_member;
                      replacement = symbol conflict.replacement_member;
                      origin;
                    })
                  resolution.conflicts
              in
              let candidates =
                List.mapi
                  (fun index (member : Exact_register_scores.member_score) ->
                    let location = member.location in
                    let seed =
                      match Frame.location_register_selection location with
                      | Request.Allocatable -> Some (Int64.div Int64.max_int 2L)
                      | Unspecified when automatic_candidate location -> Some 0L
                      | _ -> None
                    in
                    Option.map
                      (fun seed ->
                        {
                          Scored_register_allocation.member = index;
                          score = Int64.add seed member.score;
                          lea_balance = member.lea_balance;
                          offset =
                            Frame.frame_slot_displacement
                              (Option.get (Frame.location_frame_slot location));
                          pointer_depth =
                            Sema.Type.pointer_depth
                              (Frame.location_checked_type location);
                          has_dimensions =
                            Frame.location_dimensions location <> [];
                          name =
                            Sema.Symbol.name (Frame.location_symbol location);
                        })
                      seed)
                  members
                |> List.filter_map Fun.id
              in
              let registers =
                explicit
                @
                if registers_disabled body then []
                else
                  List.map
                    (fun (index, number) ->
                      ( Frame.location_symbol (List.nth members index).location,
                        number ))
                    (Scored_register_allocation.allocate
                       ~reserved:(List.map snd explicit) candidates)
              in
              let blocks = snapshot body in
              let code = List.concat_map (fun (_, items, _) -> items) blocks in
              if
                not
                  (List.for_all
                     (fun item -> Option.is_some (instruction_stamp item))
                     code)
              then None
              else
                Some
                  {
                    frame_ = frame;
                    parameters_ = List.map Fun.id (signature_parameters frame);
                    parameter_sources_ =
                      Option.map (List.map Fun.id) (source_parameters frame);
                    body_ = body;
                    definition_ = Function_body.definition_declaration body;
                    source_ = Function_body.definition_source body;
                    options_ = Function_body.compiler_options body;
                    flags_ = Function_body.stored_flags body;
                    locations_ =
                      List.map
                        (fun location ->
                          Option.get (location_stamp ~frame location))
                        locations;
                    blocks_ = blocks;
                    stamps_ =
                      List.map
                        (fun item ->
                          (item, Option.get (instruction_stamp item)))
                        code;
                    selections_ =
                      List.map
                        (fun (member : Exact_register_scores.member_score) ->
                          let location = member.location in
                          let symbol = Frame.location_symbol location in
                          ( symbol,
                            if
                              List.exists
                                (fun (original, _) -> original == symbol)
                                shadowed
                            then Stack
                            else if
                              member.score = 0L && member.lea_balance = 0L
                              && List.exists
                                   (fun (original, number) ->
                                     original == symbol
                                     && List.exists
                                          (fun conflict ->
                                            conflict.register = number)
                                          conflicts)
                                   explicit
                            then Unknown
                            else if
                              List.exists
                                (fun (original, _) -> original == symbol)
                                registers
                            then Register_candidate
                            else if
                              stack_selected ~body location
                              || Frame.location_dimensions location <> []
                              || member.lea_balance <> 0L || member.score <> 0L
                              || Frame.location_register_selection location
                                 = Request.Allocatable
                            then Stack
                            else Unknown ))
                        members;
                    exact_registers_ = Some registers;
                    shadowed_explicit_ = shadowed;
                    explicit_conflicts_ = conflicts;
                    exact_source_ = Exact_register_scores.stamp_source body;
                    runtime_ = runtime;
                  }))

let analyze_with_context ?runtime ~frame ~body () =
  if
    (not (complete_parameter_coverage ~frame))
    || (not (Exact_register_scores.permits_automatic_analysis body))
    || (not (Exact_register_scores.source_requests_match ~frame ~body))
    || not
         (Option.fold ~none:false
            ~some:(fun source ->
              List.for_all
                (fun initialization ->
                  Sema.Function_call_resolution.initializer_assignment_valid
                    ~header:(Frame.function_header frame)
                    (Sema.Function_call_expression_result.initializer_source
                       initialization))
                (Sema.Function_call_expression_result.function_initializers
                   source))
            (Function_body.definition_source body))
  then None
  else
    let mixed =
      List.exists
        (fun location -> f64_type (Frame.location_checked_type location))
        (Frame.function_locations frame)
      || List.exists
           (fun (_, code, _) ->
             List.exists
               (fun (item : Seq.description) ->
                 Option.fold ~none:false ~some:f64_type item.target_type)
               code)
           (snapshot body)
    in
    let exact =
      if
        List.for_all
          (fun location ->
            Option.fold ~none:false
              ~some:(source_owned_requests ~frame ~body)
              (location_stamp ~frame location))
          (Frame.function_locations frame)
      then
        let scores =
          match Exact_register_scores.analyze ~frame ~body with
          | Some _ as scores -> scores
          | None ->
              Option.bind
                (Mixed_scalar_source.reconstruct ?runtime ~frame ~body ())
                (fun statements ->
                  Exact_register_scores.analyze_whole ~frame ~body ~statements)
        in
        Option.bind scores (fun scores ->
            analyze_integer_function ?runtime ~exact:scores ~mixed ~frame ~body
              ())
      else None
    in
    let extended =
      (* Only the complete owned-member ledger can resolve collisions. A failed
         reconstruction must not fall back to a uniqueness-blind proof. *)
      supported_register_requests (Frame.function_locations frame)
      && not (safe_register_requests (Frame.function_locations frame))
      || List.exists
           (fun location ->
             Sema.Type.pointer_depth (Frame.location_checked_type location) > 0
             || Frame.location_value_shape location = Frame.Array
             || Frame.location_dimensions location <> []
             || (match
                   Sema.Type.base (Frame.location_checked_type location)
                 with
               | Sema.Type.Primitive (_, Sema.Primitive_type.Bool) -> true
               | _ -> false)
             || Frame.location_kind location = Frame.Automatic_local
                && Option.fold ~none:false
                     ~some:(fun local ->
                       Option.is_none
                         (Sema.Local_type_resolution.local_initializer local))
                     (Frame.location_local_source location))
           (Frame.function_locations frame)
      || Option.fold ~none:false
           ~some:(fun source ->
             List.exists
               (fun initialization ->
                 Sema.Function_call_expression_result.initializer_target_symbol
                   initialization
                 != Sema.Local_type_resolution.local_symbol
                      (Sema.Function_call_resolution.initializer_local
                         (Sema.Function_call_expression_result
                          .initializer_source initialization)))
               (Sema.Function_call_expression_result.function_initializers
                  source))
           (Function_body.definition_source body)
      || List.exists
           (fun (_, items, _) ->
             List.exists
               (fun (item : Seq.description) -> item.opcode = O.Ic_addr)
               items)
           (snapshot body)
    in
    let exact =
      if extended then analyze_owned_members ?runtime ~frame ~body () else exact
    in
    match exact with
    | Some proof -> Some proof
    | None when extended -> None
    | None when mixed ->
        if
          List.for_all
            (fun location ->
              Option.fold ~none:false
                ~some:(source_owned_requests ~frame ~body)
                (location_stamp ~frame location))
            (Frame.function_locations frame)
        then analyze_integer_function ?runtime ~mixed:true ~frame ~body ()
        else None
    | None -> (
        match analyze_parameter ~frame ~body with
        | Some proof -> Some proof
        | None -> (
            match analyze_local ~frame ~body with
            | Some proof -> Some proof
            | None -> analyze_integer_function ~frame ~body ()))

let analyze ~frame ~body = analyze_with_context ~frame ~body ()

let analyze_with_runtime ~runtime ~frame ~body =
  analyze_with_context ~runtime ~frame ~body ()

let selection t ~frame ~body ~symbol =
  if
    t.frame_ != frame || t.body_ != body
    || (not (same_list ( == ) t.parameters_ (signature_parameters frame)))
    || (not
          (Option.equal (same_list ( == )) t.parameter_sources_
             (source_parameters frame)))
    || (not (Function_body.definition_matches_frame body frame))
    || (not
          (Option.equal ( == ) t.definition_
             (Function_body.definition_declaration body)))
    || (not
          (Option.equal ( == ) t.source_ (Function_body.definition_source body)))
    || t.options_ <> Function_body.compiler_options body
    || t.flags_ <> Function_body.stored_flags body
    || Option.is_some t.exact_source_
       && not
            (Option.equal Exact_register_scores.equal_source_stamp
               t.exact_source_
               (Exact_register_scores.stamp_source body))
    || (not
          (same_list
             (fun original location ->
               Option.fold ~none:false ~some:(equal_location original)
                 (location_stamp ~frame location))
             t.locations_
             (Frame.function_locations frame)))
    || (not (matches_blocks t.blocks_ (snapshot body)))
    || not
         (List.for_all
            (fun (item, stamp) ->
              Option.fold ~none:false ~some:(equal_stamp stamp)
                (instruction_stamp item))
            t.stamps_)
  then None
  else
    let find selections =
      List.find_map
        (fun (original, selection) ->
          if original == symbol then Some selection else None)
        selections
    in
    match
      ( find t.selections_,
        analyze_with_context ?runtime:t.runtime_ ~frame ~body () )
    with
    | Some expected, Some current
      when find current.selections_ = Some expected
           && Option.equal
                (same_list (fun (left, register) (right, other) ->
                     left == right && register = other))
                t.exact_registers_ current.exact_registers_
           && same_list
                (fun (left, register) (right, other) ->
                  left == right && register = other)
                t.shadowed_explicit_ current.shadowed_explicit_
           && same_list
                (fun left right ->
                  left.register = right.register
                  && left.previous == right.previous
                  && left.replacement == right.replacement
                  && origin_stamp left.origin = origin_stamp right.origin)
                t.explicit_conflicts_ current.explicit_conflicts_ ->
        Some expected
    | _ -> None

let shadowed_explicit_request t ~frame ~body ~symbol =
  match selection t ~frame ~body ~symbol with
  | Some Stack ->
      List.find_map
        (fun (original, number) ->
          if original == symbol then Some number else None)
        t.shadowed_explicit_
  | Some Register_candidate | Some Unknown | None -> None

let explicit_conflicts t ~frame ~body =
  match t.locations_ with
  | original :: _ ->
      Option.map
        (fun _ -> t.explicit_conflicts_)
        (selection t ~frame ~body ~symbol:original.symbol)
  | [] -> None

(* A reservation can be real without a materialized home. Prove its lack of
   accesses from the original complete source ledger, not absence of hosted
   loads: unused explicit members still reserve a bank slot before scoring. *)
let unused_explicit_reservation t ~frame ~body ~symbol =
  match selection t ~frame ~body ~symbol with
  | Some Unknown
    when (safe_register_requests (Frame.function_locations frame)
         || t.explicit_conflicts_ <> [])
         && List.for_all (source_owned_requests ~frame ~body) t.locations_ ->
      Option.bind
        (List.find_opt (fun original -> original.symbol == symbol) t.locations_)
        (fun original ->
          match original.request with
          | Explicit_request (_, number, _)
            when List.mem number variable_registers ->
              Option.bind
                (Mixed_scalar_source.reconstruct ?runtime:t.runtime_ ~owned:true
                   ~frame ~body ()) (fun statements ->
                  Option.bind
                    (Exact_register_scores.analyze_members ~frame ~body
                       ~statements) (fun members ->
                      List.find_map
                        (fun (member : Exact_register_scores.member_score) ->
                          if
                            Frame.location_symbol member.location == symbol
                            && member.score = 0L && member.lea_balance = 0L
                          then Some number
                          else None)
                        members))
          | _ -> None)
  | Some Register_candidate | Some Stack | Some Unknown | None -> None

let physical_register t ~frame ~body ~symbol =
  match selection t ~frame ~body ~symbol with
  | Some Register_candidate
    when List.for_all (source_owned_requests ~frame ~body) t.locations_ ->
      Option.bind
        (List.find_opt (fun original -> original.symbol == symbol) t.locations_)
        (fun original ->
          let supported number =
            List.mem number [ 6; 7; 10; 11; 12; 13; 14; 15 ]
          in
          match t.exact_registers_ with
          | Some registers ->
              List.find_map
                (fun (member, number) ->
                  if member == symbol && supported number then Some number
                  else None)
                registers
          | None -> (
              match original.request with
              | Explicit_request (_, number, _) when supported number ->
                  Some number
              | (Unspecified_request | Allocatable_request)
                when not (registers_disabled body) ->
                  (* OptPass3.HC:22-48 counts every unspecified integer member and
                 every REG_ALLOC, including unused members. Unknown selection
                 is not evidence of absence: specialized proofs intentionally
                 leave operands Unknown, and pressure proofs do likewise.

                 With at most six possible candidates, the R12/R13 bank cannot
                 open (557-582). A sole candidate, or sole REG_ALLOC whose
                 I64_MAX/2 seed dominates these bounded ordinary scores, is
                 therefore first. Do not predict QSort's order for any tie. *)
                  let candidates =
                    List.filter
                      (fun location ->
                        location.request = Allocatable_request
                        || location.request = Unspecified_request
                           &&
                           match location.checked_type with
                           | _, Sema.Primitive_type.F64, 0 -> false
                           | _ -> true)
                      t.locations_
                  in
                  let forced =
                    List.filter
                      (fun location -> location.request = Allocatable_request)
                      candidates
                  in
                  if
                    List.length candidates > 6
                    || not
                         (List.length candidates = 1
                         || original.request = Allocatable_request
                            && List.length forced = 1)
                  then None
                  else
                    let reserved =
                      List.filter_map
                        (fun location ->
                          match location.request with
                          | Explicit_request (_, number, _) -> Some number
                          | _ -> None)
                        t.locations_
                    in
                    (* CInit.HC:244-256 builds the map in ascending register order.
                   Explicit requests reserve slots before scoring even when
                   unused. Preserve the actual source register order: do not
                   skip an unsupported first free register for a later one. *)
                    Option.bind
                      (List.find_opt
                         (fun number -> not (List.mem number reserved))
                         general_variable_registers)
                      (fun number ->
                        if supported number then Some number else None)
              | _ -> None))
  | Some Register_candidate | Some Stack | Some Unknown | None -> None
