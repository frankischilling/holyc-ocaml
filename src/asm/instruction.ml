type error = { operand_index : int option; message : string }
type warning = Unspecified_size

type selection = {
  opcode : Opcode.resolved;
  mode : Operand.mode;
  operands : Operand.t list;
  form : Generated.Opcode_keywords.instruction;
  candidate_count : int;
  warnings : warning list;
  selection_bytes : int;
  encoded_bytes : int;
  operand_prefix : bool;
  address_prefix : bool;
  segment_prefix : int option;
  rex : int option;
  modrm : int option;
  sib : int option;
  immediate_bytes : int;
  displacement_bytes : int;
  unresolved : bool;
}

let warning_message = function
  | Unspecified_size ->
      "no size specified for an instruction with multiple matching forms"

let flag form mask = form.Generated.Opcode_keywords.flags land mask <> 0

let register_number register =
  Option.fold ~none:(-1) ~some:Register.number register

let reg1 operand = register_number (Operand.register1 operand)
let reg2 operand = register_number (Operand.register2 operand)

let floating_register operand =
  Option.fold ~none:false
    ~some:(fun register -> Register.kind register = Register.Float_stack)
    (Operand.register1 operand)

let value operand =
  match Operand.immediate operand with
  | Some { value = Some value; _ } -> value
  | _ -> 0L

let in_range lower upper value = lower <= value && value <= upper
let rm argument = in_range 16 23 argument
let qword argument = argument = 15 || argument = 19 || argument = 23
let immediate_kind argument = in_range 4 11 argument

type plan = {
  form_ : Generated.Opcode_keywords.instruction;
  mutable rex_ : int;
  mutable has_rex : bool;
  mutable modrm_ : int;
  mutable has_modrm : bool;
  mutable sib_ : int;
  mutable has_sib : bool;
  operand_prefix_ : bool;
  mutable address_prefix_ : bool;
  mutable displacement_bytes_ : int;
  mutable immediate_bytes_ : int;
  mutable rm_operand : Operand.t option;
  mutable other_operand : Operand.t option;
  mutable cost : int;
}

let same_registers plan =
  match (plan.rm_operand, plan.other_operand) with
  | Some left, Some right -> reg1 left = reg1 right
  | _ -> false

let emits_rex ~mode ~form ~rex ~has_rex ~same_registers ~modrm =
  mode = Operand.Use64
  && rex land 0x40 = 0x40
  && (rex <> 0x40 || has_rex)
  && (rex land 7 <> 0
     || not
          (flag form 0x080
          || (flag form 0x100 && same_registers && modrm land 0xC0 = 0xC0)))

let register_rex plan register extension =
  if register land 15 > 7 then plan.rex_ <- plan.rex_ lor extension;
  if register >= 20 then plan.has_rex <- true

let build_plan mode form operands =
  let mode16 = mode = Operand.Use16 in
  let mode64 = mode = Operand.Use64 in
  let plan =
    {
      form_ = form;
      rex_ = (if mode64 then if flag form 0x040 then 0x48 else 0x40 else 0);
      has_rex = false;
      modrm_ = 0;
      has_modrm = false;
      sib_ = 0;
      has_sib = false;
      operand_prefix_ =
        (mode16 && flag form 0x002) || ((not mode16) && flag form 0x001);
      address_prefix_ = false;
      displacement_bytes_ = 0;
      immediate_bytes_ =
        (match form.Generated.Opcode_keywords.opcode_modifier with
        | 1 | 5 -> 1
        | 2 | 6 -> 2
        | 3 | 7 -> 4
        | _ -> 0);
      rm_operand = List.nth_opt operands 0;
      other_operand = List.nth_opt operands 1;
      cost = 0;
    }
  in
  if form.slash_value < 8 then (
    plan.modrm_ <- form.slash_value lsl 3;
    plan.has_modrm <- true);
  let memory_shape operand ~single =
    let reg1 = reg1 operand and reg2 = reg2 operand in
    let indirect = Operand.indirect operand in
    let present = Operand.immediate_present operand in
    let complete = ref false in
    plan.has_modrm <- true;
    if mode16 then plan.address_prefix_ <- true;
    (* The single-argument path checks a displacement-only address before SIB
       construction; the two-argument path checks it afterward. *)
    if single && present && indirect && reg1 = -1 then (
      plan.modrm_ <- plan.modrm_ + 5;
      plan.displacement_bytes_ <- 4;
      complete := true)
    else (
      if reg2 = -1 && Operand.scale operand = 1L then (
        if single || reg1 <> -1 then (
          plan.modrm_ <- plan.modrm_ lor (reg1 land 7);
          register_rex plan reg1 1))
      else (
        plan.modrm_ <- plan.modrm_ lor 4;
        plan.has_sib <- true;
        plan.sib_ <-
          (match Operand.scale operand with
          | 2L -> 0x40
          | 4L -> 0x80
          | 8L -> 0xC0
          | _ -> 0);
        if reg2 = -1 then (
          complete := true;
          plan.sib_ <- plan.sib_ lor ((reg1 land 7) lsl 3) lor 5;
          register_rex plan reg1 2;
          plan.displacement_bytes_ <- 4)
        else (
          plan.sib_ <- plan.sib_ lor ((reg1 land 7) lsl 3) lor (reg2 land 7);
          register_rex plan reg1 2;
          register_rex plan reg2 1;
          if reg2 land 7 = 5 && (not present) && indirect then (
            plan.modrm_ <- plan.modrm_ lor 0x40;
            plan.displacement_bytes_ <- 1;
            complete := true)));
      if not !complete then
        if (not single) && present && indirect && reg1 = -1 then (
          plan.modrm_ <- (plan.modrm_ land 0xF8) + 5;
          plan.displacement_bytes_ <- 4)
        else if present then
          if
            (not (Operand.unresolved operand))
            && value operand >= -128L
            && value operand <= 127L
          then (
            plan.modrm_ <- plan.modrm_ lor 0x40;
            plan.displacement_bytes_ <- 1)
          else (
            plan.modrm_ <- plan.modrm_ lor 0x80;
            plan.displacement_bytes_ <- (if mode16 then 2 else 4))
        else if not indirect then (
          plan.address_prefix_ <- false;
          plan.modrm_ <- plan.modrm_ lor 0xC0)
        else if reg1 land 7 = 5 then (
          plan.modrm_ <- plan.modrm_ lor 0x40;
          plan.displacement_bytes_ <- 1))
  in
  (match operands with
  | [ operand ] ->
      if flag form 0x004 then
        if form.slash_value = 8 then register_rex plan (reg1 operand) 1
        else if floating_register operand then ();
      if qword form.argument1 then plan.rex_ <- plan.rex_ lor 8;
      if rm form.argument1 then memory_shape operand ~single:true
      else if in_range 1 3 form.argument1 || immediate_kind form.argument1 then
        if form.argument1 = 7 || form.argument2 = 11 then
          plan.rex_ <- plan.rex_ lor 8
  | [ first; second ] ->
      if flag form 0x004 then
        if form.slash_value = 8 then
          register_rex plan
            (if in_range 32 35 form.argument1 then reg1 second else reg1 first)
            1
        else if floating_register first || floating_register second then ();
      if qword form.argument1 || qword form.argument2 then
        plan.rex_ <- plan.rex_ lor 8;
      if rm form.argument1 || rm form.argument2 then (
        let memory, other =
          if rm form.argument2 then (second, first) else (first, second)
        in
        plan.rm_operand <- Some memory;
        plan.other_operand <- Some other;
        if form.slash_value = 8 then
          if Operand.just_segment other then
            plan.modrm_ <-
              plan.modrm_ lor (register_number (Operand.segment other) lsl 3)
          else if reg1 other = -1 then (
            plan.modrm_ <- plan.modrm_ lor ((reg1 memory land 7) lsl 3);
            register_rex plan (reg1 memory) 4)
          else (
            plan.modrm_ <- plan.modrm_ lor ((reg1 other land 7) lsl 3);
            register_rex plan (reg1 other) 4);
        memory_shape memory ~single:false)
      else if in_range 28 31 form.argument1 || in_range 28 31 form.argument2
      then (
        plan.displacement_bytes_ <- (if mode16 then 2 else 4);
        plan.address_prefix_ <- false)
      else if immediate_kind form.argument1 then (
        plan.immediate_bytes_ <-
          (match form.argument1 with
          | 4 | 8 -> 1
          | 5 | 9 -> 2
          | 6 | 10 -> 4
          | _ -> 8);
        if form.argument1 = 7 || form.argument1 = 11 then
          plan.rex_ <- plan.rex_ lor 8);
      if immediate_kind form.argument2 then (
        plan.immediate_bytes_ <-
          (match form.argument2 with
          | 4 | 8 -> 1
          | 5 | 9 -> 2
          | 6 | 10 -> 4
          | _ -> 8);
        if form.argument2 = 6 || form.argument2 = 10 then (
          if flag form 0x080 && form.argument2 = 10 then
            plan.rex_ <- plan.rex_ land lnot 8)
        else if form.argument2 = 7 || form.argument2 = 11 then
          plan.rex_ <- plan.rex_ lor 8)
  | [] -> ()
  | _ -> assert false);
  plan.cost <-
    (List.length form.opcode_bytes
    + plan.displacement_bytes_ + plan.immediate_bytes_
    + (if plan.has_modrm then 1 else 0)
    + (if plan.has_sib then 1 else 0)
    +
    if
      emits_rex ~mode ~form ~rex:plan.rex_ ~has_rex:plan.has_rex
        ~same_registers:(same_registers plan) ~modrm:plan.modrm_
    then 1
    else 0);
  plan

let select ~mode ~opcode ~operands =
  let reject ?operand_index message = Error { operand_index; message } in
  let canonical = Opcode.resolved_opcode opcode in
  let expected = Opcode.first_form_argument_count canonical in
  if List.length operands <> expected then
    reject
      (Printf.sprintf
         "opcode %s requires %d operands from its original first form"
         (Opcode.resolved_source_spelling opcode)
         expected)
  else
    match
      List.find_index (fun operand -> Operand.mode operand <> mode) operands
    with
    | Some operand_index ->
        reject ~operand_index "operand was parsed in a different assembler mode"
    | None -> (
        let masks = Array.of_list (List.map Operand.argument_mask operands) in
        let mask index =
          if index < Array.length masks then masks.(index) else 1L
        in
        let has mask argument =
          Int64.logand mask (Int64.shift_left 1L argument) <> 0L
        in
        let best = ref None and last = ref None in
        let candidate_count = ref 0
        and warning = ref false
        and second_seen = ref false in
        List.iter
          (fun (form : Generated.Opcode_keywords.instruction) ->
            if
              has (mask 0) form.argument1
              && has (mask 1) form.argument2
              && ((not (flag form 0x020)) || mode <> Operand.Use64)
            then (
              let plan = build_plan mode form operands in
              last := Some plan;
              (match !best with
              | Some previous
                when (not !second_seen)
                     && (not (flag previous.form_ 0x010))
                     && expected > 0 ->
                  second_seen := true;
                  if
                    List.for_all
                      (fun operand -> Operand.size operand = 0)
                      operands
                  then warning := true
              | _ -> ());
              if
                not
                  (flag form 0x008
                  && (plan.address_prefix_ || plan.operand_prefix_))
              then (
                incr candidate_count;
                match !best with
                | Some previous when previous.cost <= plan.cost -> ()
                | _ -> best := Some plan)))
          (Opcode.forms canonical);
        match !best with
        | Some plan when plan.form_.opcode_bytes = [] ->
            reject
              (Printf.sprintf
                 "opcode %s selects the original empty instruction form at \
                  OpCodes.DD:%d; no checked opcode byte is available"
                 (Opcode.resolved_source_spelling opcode)
                 plan.form_.source_line)
        | None ->
            reject
              (Printf.sprintf "invalid operand form for %s in USE%d"
                 (Opcode.resolved_source_spelling opcode)
                 (Operand.mode_bits mode))
        | Some plan ->
            let form = plan.form_ in
            let last = Option.value ~default:plan !last in
            (* Asm.HC:779 and :784 use the last considered candidate's SIB and
               operand roles while emitting the selected candidate. *)
            let address_prefix =
              plan.address_prefix_ || (mode = Operand.Use16 && last.has_sib)
            in
            let rex =
              if
                emits_rex ~mode ~form ~rex:plan.rex_ ~has_rex:plan.has_rex
                  ~same_registers:(same_registers last) ~modrm:plan.modrm_
              then Some plan.rex_
              else None
            in
            let selected_segment operand =
              if Operand.just_segment operand then None
              else Option.map Register.number (Operand.segment operand)
            in
            let segment_prefix =
              match operands with
              | [ first; second ] -> (
                  match selected_segment second with
                  | Some _ as segment -> segment
                  | None -> selected_segment first)
              | [ first ] -> selected_segment first
              | _ -> None
            in
            let encoded_bytes =
              List.length form.opcode_bytes
              + plan.displacement_bytes_ + plan.immediate_bytes_
              + (if plan.has_modrm then 1 else 0)
              + (if plan.has_sib then 1 else 0)
              + (if rex <> None then 1 else 0)
              + (if plan.operand_prefix_ then 1 else 0)
              + (if address_prefix then 1 else 0)
              + (if segment_prefix <> None then 1 else 0)
              + if flag form 0x400 then 1 else 0
            in
            Ok
              {
                opcode;
                mode;
                operands;
                form;
                candidate_count = !candidate_count;
                warnings = (if !warning then [ Unspecified_size ] else []);
                selection_bytes = plan.cost;
                encoded_bytes;
                operand_prefix = plan.operand_prefix_;
                address_prefix;
                segment_prefix;
                rex;
                modrm = (if plan.has_modrm then Some plan.modrm_ else None);
                sib = (if plan.has_sib then Some plan.sib_ else None);
                immediate_bytes = plan.immediate_bytes_;
                displacement_bytes = plan.displacement_bytes_;
                unresolved = List.exists Operand.unresolved operands;
              })
