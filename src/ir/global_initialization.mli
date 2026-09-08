type phase = Compile_initializer | Load_initializer
type region
type static_region
type storage_region
type t

type prepared_root = Initializer_publication.prepared_root =
  | Prepared_global of
      Sema.Function_call_expression_result.top_level_root_result
  | Prepared_static of
      Integer_globals.static_slot
      * Sema.Function_call_expression_result.initializer_result

type publication_description = Initializer_publication.description = {
  prepared_root : prepared_root;
  before : Instruction_sequence.Instruction_id.t;
}

type publication
type publication_evidence = Initializer_publication.t

val publications : t -> publication list
val publication_evidence : t -> publication_evidence option
val publication_before : publication -> Instruction_sequence.Instruction_id.t
val publication_storage : publication -> Integer_globals.storage_slot
val publication_cell_offset : publication -> int
val publication_payload : publication -> Integer_array_initializers.payload
val describe_publication : publication -> publication_description

type region_description = {
  root : Sema.Function_call_expression_result.top_level_root_result;
  first : Instruction_sequence.Instruction_id.t;
  last : Instruction_sequence.Instruction_id.t;
}

type static_region_description = {
  static_root : Sema.Function_call_expression_result.initializer_result;
  static_slot : Integer_globals.static_slot;
  first : Instruction_sequence.Instruction_id.t;
  last : Instruction_sequence.Instruction_id.t;
}

val create :
  ?static_descriptions:static_region_description list ->
  ?publications:publication_description list ->
  ?publication_evidence:publication_evidence ->
  span:Common.Span.t ->
  globals:Integer_globals.t ->
  entry:X87_stack.t ->
  region_description list ->
  (t, Common.Diagnostic.t list) result
(** Check complete, ordered, nonoverlapping declaration regions in the exact
    entry graph. Each region starts with its destination address and ends with
    its canonical initializer store and expression boundary. Entry instruction
    IDs must increase in physical order. Operands and call scopes are closed
    within each region; ordinary expressions cannot supply initializer values.
*)

val matches : t -> globals:Integer_globals.t -> entry:X87_stack.t -> bool
val globals : t -> Integer_globals.t
val regions : t -> region list
val static_regions : t -> static_region list

val static_root :
  static_region -> Sema.Function_call_expression_result.initializer_result

val static_slot : static_region -> Integer_globals.static_slot
val describe_static : static_region -> static_region_description
val static_phase : static_region -> phase
val storage_regions : t -> storage_region list

val find_storage :
  t -> Instruction_sequence.Instruction_id.t -> storage_region option

val storage_symbol : storage_region -> Sema.Symbol.t
val storage_phase : storage_region -> phase

val storage_frame :
  storage_region -> Sema.Function_frame_layout.function_layout option

val storage_first : storage_region -> Instruction_sequence.Instruction_id.t
val storage_last : storage_region -> Instruction_sequence.Instruction_id.t
val prepared_steps : t -> int
val find : t -> Instruction_sequence.Instruction_id.t -> region option
val root : region -> Sema.Function_call_expression_result.top_level_root_result
val describe : region -> region_description
val symbol : region -> Sema.Symbol.t
val phase : region -> phase
val phase_name : phase -> string
val human : t -> string
