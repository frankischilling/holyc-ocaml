module Ast = Frontend.Ast
module Parser = Frontend.Parser
module Visibility = Frontend.Symbol_visibility
module Records = Function_record_classification
module Functions = Function_resolution
module Headers = Function_type_resolution

type arm = Try_body | Catch_body

type original_header = {
  header_table : Symbol_table.t;
  header_source : Compiler_record.declared_function;
  checked_header : Headers.resolved_function;
}

module Original_headers = Ephemeron.K1.Make (struct
  type t = Functions.resolved_declaration

  let equal = ( == )

  let hash declaration =
    Functions.resolved_declaration_site declaration
    |> Functions.declaration_site_function |> Headers.function_symbol
    |> Symbol.id |> Symbol.Id.to_int
end)

let original_headers = Original_headers.create 32

let source_binding_kind (header : Parser.completed_function_header) =
  match header.function_publication.function_header.binding with
  | None -> Some Functions.Definition
  | Some binding -> (
      match (binding.kind, binding.spelling, binding.target) with
      | Ast.Extern, "extern", Ast.No_binding_target -> Some Functions.Extern
      | Ast.Extern, "_extern", Ast.Symbol_binding_target _ ->
          Some Functions.Bound_extern
      | Ast.Import, "import", Ast.No_binding_target
      | Ast.Import, "_import", Ast.Symbol_binding_target _ ->
          Some Functions.Import
      | Ast.Intern, "_intern", Ast.Expression_binding_target _ ->
          Some Functions.Intern
      | _ -> None)

let bind_headers ~table ~namespace ?selected_aggregate ~records sources =
  Records.declarations records
  |> List.iter (fun classified ->
      let declaration = Records.classified_declaration_source classified in
      let site = Functions.resolved_declaration_site declaration in
      let checked_header = Functions.declaration_site_function site in
      let original =
        List.find_opt
          (fun source ->
            let header = Compiler_record.declared_function_source source in
            Compiler_record.declared_function_owns_table source table
            && Compiler_record.declared_function_owns_namespace source namespace
            && Compiler_record.declared_function_symbol source
               == Headers.function_symbol checked_header
            && source_binding_kind header
               = Some (Functions.declaration_site_source_kind site)
            && Headers.source_function_matches ~table ~namespace
                 ?selected_aggregate header checked_header)
          sources
      in
      Option.iter
        (fun header_source ->
          match Original_headers.find_opt original_headers declaration with
          | Some _ -> ()
          | None ->
              Original_headers.add original_headers declaration
                { header_table = table; header_source; checked_header })
        original)

let original_header declaration =
  let site = Functions.resolved_declaration_site declaration in
  match Functions.declaration_site_header_source site with
  | Some source -> Some source
  | None -> (
      match Original_headers.find_opt original_headers declaration with
      | Some original
        when original.checked_header == Functions.declaration_site_function site
             && Compiler_record.declared_function_owns_table
                  original.header_source original.header_table ->
          Some original.header_source
      | _ -> None)

type t = {
  table_ : Symbol_table.t;
  records_ : Records.t;
  selection_ : Parser.exception_selection;
  source_ : Ast.try_catch_statement;
  ancestry_ : (Ast.try_catch_statement * arm) list;
  function_header_ : Headers.resolved_function option;
  try_declaration_ : Functions.resolved_declaration;
  untry_declaration_ : Functions.resolved_declaration;
}

let source region = region.source_
let ancestry region = region.ancestry_
let try_declaration region = region.try_declaration_
let untry_declaration region = region.untry_declaration_
let owns_table region table = region.table_ == table

let matches_header region header =
  match (region.function_header_, header) with
  | None, None -> true
  | Some original, Some actual -> original == actual
  | _ -> false

let matches_records region records =
  region.records_ == records
  &&
  match Parser.exception_selection region.source_ with
  | Some original -> original == region.selection_
  | None -> false

let original_ancestry ast source =
  let found = ref [] in
  let rec statement owner parents = function
    | Ast.Try_catch_statement original ->
        if original == source then found := (owner, parents) :: !found;
        statement owner ((original, Try_body) :: parents) original.try_body;
        statement owner ((original, Catch_body) :: parents) original.catch_body
    | Ast.Block_statement block ->
        List.iter (statement owner parents) block.block_statements
    | Ast.Sequence_statement sequence ->
        List.iter
          (fun element ->
            statement owner parents element.Ast.sequence_statement)
          sequence.sequence_elements
    | Ast.If_statement branch ->
        statement owner parents branch.if_then_branch;
        Option.iter
          (fun clause -> statement owner parents clause.Ast.else_branch)
          branch.if_else_clause
    | Ast.While_statement loop -> statement owner parents loop.while_body
    | Ast.Do_while_statement loop -> statement owner parents loop.do_body
    | Ast.For_statement loop ->
        statement owner parents loop.for_initializer;
        Option.iter (statement owner parents) loop.for_update;
        statement owner parents loop.for_body
    | Ast.Switch_statement switch ->
        elements owner parents switch.switch_elements
    | _ -> ()
  and elements owner parents source =
    List.iter
      (function
        | Ast.Switch_statement_element body -> statement owner parents body
        | Ast.Switch_subswitch_element region ->
            elements owner parents region.subswitch_elements
        | Ast.Switch_case_element _ | Ast.Switch_default_element _ -> ())
      source
  in
  List.iter
    (function
      | Ast.Top_level_statement body -> statement None [] body
      | Ast.Function_definition definition ->
          Option.iter (statement (Some definition) []) definition.body
      | _ -> ())
    ast.Ast.items;
  match !found with
  | [ original ] -> Some original
  | _ -> None

let primitive type_ depth expected =
  Type.pointer_depth type_ = depth
  &&
  match Type.base type_ with
  | Type.Primitive (_, actual) -> Primitive_type.equal actual expected
  | Type.Aggregate _ -> false

let rec selected_entry selected original =
  selected == original
  ||
  match Visibility.function_alias_original selected with
  | None -> false
  | Some previous -> selected_entry previous original

let record_candidates ?environment records =
  let retained =
    match environment with
    | None -> []
    | Some environment ->
        Outer_environment.tables environment
        |> List.concat_map (fun table ->
            Outer_environment.table_entries table
            |> List.rev
            |> List.filter_map (fun entry ->
                Option.map Outer_environment.function_classified_declaration
                  (Outer_environment.entry_function_metadata entry)))
  in
  List.rev (Records.declarations records) @ retained

let selected_record candidates selected =
  let seen = ref [] in
  let rec inspect record =
    if List.exists (( == ) record) !seen then None
    else (
      seen := record :: !seen;
      let declaration = Records.classified_declaration_source record in
      match original_header declaration with
      | Some original
        when selected_entry selected
               (Compiler_record.declared_function_source original)
                 .completed_entry -> Some record
      | _ ->
          Option.bind
            (Records.classified_declaration_retained_predecessor record)
            inspect)
  in
  List.find_map inspect candidates

let checked_header ~table ~candidates ~name ~parameters selected =
  match selected_record candidates selected with
  | None -> Error (name ^ " has no original checked source header")
  | Some classified ->
      let declaration = Records.classified_declaration_source classified in
      let site = Functions.resolved_declaration_site declaration in
      let record = Records.classified_declaration_record classified in
      let header = Functions.resolved_declaration_header declaration in
      let original = original_header declaration in
      let original_owned =
        match original with
        | Some original ->
            let completed = Compiler_record.declared_function_source original in
            Compiler_record.declared_function_owns_table original table
            && Compiler_record.declared_function_symbol original
               == Headers.function_symbol
                    (Functions.declaration_site_function site)
            && selected_entry selected completed.completed_entry
        | None -> false
      in
      let identity =
        Functions.resolved_declaration_identity_symbol declaration
      in
      let current_ordinary =
        candidates
        |> List.find_opt (fun current ->
            Functions.resolved_declaration_identity_symbol
              (Records.classified_declaration_source current)
            == identity)
        |> Option.fold ~none:false ~some:(fun current ->
            let current = Records.classified_declaration_record current in
            Records.is_extern current
            && (not (Records.is_internal current))
            && Records.import_name current = None)
      in
      let signature = Headers.function_signature header in
      let actual = Headers.signature_parameters signature in
      let expected_flags =
        if
          Function_flag.derives_ret1
            ~argument_count:(Int64.of_int (List.length parameters))
            ~variadic:false
        then Function_flag.Stored.to_mask Function_flag.Stored.Ret1
        else 0L
      in
      let parameter_matches parameter (depth, expected) =
        Headers.parameter_default parameter = None
        && Headers.parameter_register_requests parameter = []
        && primitive
             (Type_reference.resolved_type
                (Headers.parameter_type_reference parameter))
             depth expected
      in
      if
        (not original_owned)
        || Symbol.name (Headers.function_symbol header) <> name
        || Functions.declaration_site_source_kind site <> Functions.Extern
        || Functions.declaration_site_kind site <> Functions.Extern
        || Functions.declaration_site_state site <> Functions.Unresolved_extern
        || Functions.declaration_site_is_pending site
        || (not (Records.is_extern record))
        || Records.is_internal record
        || Records.import_name record <> None
        || Records.stored_flag_mask record <> expected_flags
        || (not current_ordinary)
        || (not
              (primitive
                 (Type_reference.resolved_type
                    (Headers.function_return_type header))
                 0 Primitive_type.U0))
        || Headers.signature_variadic_origin signature <> None
        || List.length actual <> List.length parameters
        || not (List.for_all2 parameter_matches actual parameters)
      then
        Error
          (name
         ^ " requires its original ordinary extern header with the standard \
            exception signature")
      else Ok declaration

let checked_function_header ~table candidates = function
  | None -> Ok None
  | Some definition -> (
      let original =
        List.find_map
          (fun classified ->
            let declaration =
              Records.classified_declaration_source classified
            in
            let site = Functions.resolved_declaration_site declaration in
            let header = Functions.declaration_site_function site in
            match original_header declaration with
            | Some original
              when Compiler_record.declared_function_owns_table original table
                   && Result.is_ok
                        (Parser.function_body_compiler_options
                           (Compiler_record.declared_function_source original)
                           definition) -> Some header
            | _ -> None)
          candidates
      in
      match original with
      | Some header -> Ok (Some header)
      | None -> Error "try has no original checked containing function header")

let create ?environment ~table ~records ~ast ~source () =
  let fail message =
    Error
      [
        Common.Diagnostic.make ~code:"HCRUN0004"
          ~severity:Common.Diagnostic.Error ~message
          ~primary:source.Ast.try_keyword.span ();
      ]
  in
  let environment_valid =
    Option.fold ~none:true
      ~some:(fun environment ->
        Outer_environment.owns_table environment table
        && Outer_environment.compilation_mode environment
           = Records.compilation_mode records)
      environment
  in
  let original_path selection parents =
    match Parser.exception_ancestry selection with
    | None -> false
    | Some original ->
        List.length original = List.length parents
        && List.for_all2
             (fun (original, arm) (actual, actual_arm) ->
               original == actual
               &&
               match (arm, actual_arm) with
               | Parser.Try_arm, Try_body | Parser.Catch_arm, Catch_body -> true
               | _ -> false)
             original parents
  in
  let candidates = record_candidates ?environment records in
  let original_function selection owner =
    match (Parser.exception_function selection, owner) with
    | Some None, None -> true
    | Some (Some original), Some actual -> original == actual
    | _ -> false
  in
  match (Parser.exception_selection source, original_ancestry ast source) with
  | Some selection, Some (owner, ancestry_)
    when environment_valid
         && Parser.exception_source selection == source
         && original_path selection ancestry_
         && original_function selection owner -> (
      match
        ( Parser.exception_try_header selection,
          Parser.exception_untry_header selection )
      with
      | Some try_header, Some untry_header -> (
          match
            ( checked_header ~table ~candidates ~name:"SysTry"
                ~parameters:[ (1, Primitive_type.U8); (1, Primitive_type.U8) ]
                try_header,
              checked_header ~table ~candidates ~name:"SysUntry" ~parameters:[]
                untry_header,
              checked_function_header ~table candidates owner )
          with
          | Error message, _, _ | _, Error message, _ | _, _, Error message ->
              fail message
          | Ok try_declaration_, Ok untry_declaration_, Ok function_header_ ->
              Ok
                {
                  table_ = table;
                  records_ = records;
                  selection_ = selection;
                  source_ = source;
                  ancestry_;
                  function_header_;
                  try_declaration_;
                  untry_declaration_;
                })
      | _ -> fail "try has no original SysTry and SysUntry header selections")
  | _ -> fail "try does not belong to its original parser source and module"
