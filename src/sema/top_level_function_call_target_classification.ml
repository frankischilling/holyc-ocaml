type t = {
  source_ : Function_call_expression_result.top_level_direct_call;
  declaration_ : Function_resolution.resolved_declaration;
  classified_declaration_ :
    Function_record_classification.classified_declaration;
  record_ : Function_record_classification.record;
  call_access_ : Function_record_classification.call_access;
}

type error_kind = Invalid_input of string
type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let source target = target.source_
let declaration target = target.declaration_
let classified_declaration target = target.classified_declaration_
let record target = target.record_
let call_access target = target.call_access_

let call_origin source =
  source |> Function_call_expression_result.top_level_direct_source
  |> Top_level_expression_tree.call_source
  |> Function_call_resolution.call_origin

let classify ~records source =
  let declaration =
    Function_call_expression_result.top_level_direct_declaration source
  in
  let classified =
    records |> Function_record_classification.declarations
    |> List.find_opt (fun classified ->
        Function_record_classification.classified_declaration_source classified
        == declaration)
  in
  match classified with
  | None ->
      Error
        {
          code = "HCSEMA0068";
          kind =
            Invalid_input
              "top-level direct-call declaration is not owned by the \
               function-record classification";
          origin = Some (call_origin source);
        }
  | Some classified_declaration ->
      let record =
        Function_record_classification.classified_declaration_record
          classified_declaration
      in
      Ok
        {
          source_ = source;
          declaration_ = declaration;
          classified_declaration_ = classified_declaration;
          record_ = record;
          call_access_ = Function_record_classification.call_access record;
        }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with
  | Invalid_input message -> message

let error_to_string error = error.code ^ ": " ^ error_message error
