module Record = Compiler_record

type scalar = {
  type_ : Type.t;
  value_type : Type.t;
  byte_size : int;
  owner : Symbol.t option;
  callback_source : Frontend.Ast.function_pointer_declarator option;
}

type scalar_evaluation = {
  before : scalar;
  dynamic : bool;
  settle : unit -> (scalar, string) result;
  domain : Domain.id;
  mutable finished : bool;
}

type t = {
  read : unit -> (view, string) result;
  capture_scalar : unit -> (scalar_evaluation, string) result;
  select_forwarding : unit -> (t, string) result;
}

and element = { type_ : t; dimensions : int64 list }
and view = Scalar of scalar | Aggregate of element list

let element_type (element : element) = element.type_
let element_dimensions element = element.dimensions
let scalar_type (scalar : scalar) = scalar.type_
let scalar_byte_size scalar = scalar.byte_size

let scalar_equal left right =
  left.byte_size = right.byte_size
  && Type.equal left.type_ right.type_
  && Type.equal left.value_type right.value_type
  && Option.equal ( == ) left.owner right.owner
  && Option.equal ( == ) left.callback_source right.callback_source

let view type_ = type_.read ()
let select_forwarding type_ = type_.select_forwarding ()
let ( let* ) = Result.bind
let invalid message = Error ("HCRUN0006: initializer type " ^ message)

let evaluation ?(dynamic = false) before settle =
  { before; dynamic; settle; domain = Domain.self (); finished = false }

let fixed_node read =
  let rec selected =
    {
      read;
      select_forwarding = (fun () -> Ok selected);
      capture_scalar =
        (fun () ->
          let* value = read () in
          match value with
          | Scalar before -> Ok (evaluation before (fun () -> Ok before))
          | Aggregate _ ->
              invalid "requires its original direct member traversal");
    }
  in
  selected

let begin_scalar_evaluation type_ = type_.capture_scalar ()
let scalar_evaluation_before phase = phase.before
let scalar_evaluation_may_change phase = phase.dynamic

let finish_scalar_evaluation phase =
  if phase.domain <> Domain.self () || phase.finished then
    invalid "scalar evaluation is foreign or was already settled"
  else (
    phase.finished <- true;
    phase.settle ())

let scalar ?owner ?value_type ?callback_source ~type_ bytes =
  if bytes < 0L || bytes > Int64.of_int Int.max_int then
    invalid "has an invalid scalar width"
  else
    Ok
      (Scalar
         {
           type_;
           value_type = Option.value value_type ~default:type_;
           byte_size = Int64.to_int bytes;
           owner;
           callback_source;
         })

let primitive type_ =
  if Type.pointer_depth type_ > 0 then
    scalar ~type_ (Int64.of_int Primitive_type.pointer_byte_size)
  else
    match Type.base type_ with
    | Type.Primitive (_, primitive) ->
        scalar ~type_ (Int64.of_int (Primitive_type.info primitive).byte_size)
    | _ -> invalid "requires its original selected class metadata"

let of_type type_ =
  let* value = primitive type_ in
  Ok (fixed_node (fun () -> Ok value))

let default_word =
  Type.make_primitive ~form:Type.Internal_storage ~primitive:Primitive_type.I64
    ~pointer_depth:0
  |> Result.get_ok

let scalar_value_type (scalar : scalar) =
  if scalar.byte_size = 0 then default_word else scalar.value_type

let callback ?type_ source =
  fixed_node (fun () ->
      let depth = List.length source.Frontend.Ast.indirection_layers in
      let* expected =
        Type.make_primitive ~form:Type.Internal_storage
          ~primitive:Primitive_type.I64 ~pointer_depth:depth
      in
      if
        depth = 0
        || Option.fold ~none:false
             ~some:(fun type_ -> not (Type.equal expected type_))
             type_
      then invalid "callback storage replaced its original pointer shape"
      else
        scalar ~type_:expected ~value_type:default_word ~callback_source:source
          (Int64.of_int Primitive_type.pointer_byte_size))

let of_record ?resolve ~type_ original =
  let resolve =
    Option.value resolve ~default:(Record.aggregate_initializer_record original)
  in
  let selected_record type_ saved =
    match (resolve type_, saved) with
    | Some current, _
      when Record.aggregate_record_matches ~owner:original ~type_ current ->
        Ok current
    | Some _, _ -> invalid "replaced its original class namespace or identity"
    | None, Some record
      when Record.aggregate_record_matches ~owner:original ~type_ record ->
        Ok record
    | _ -> invalid "has no original class record"
  in
  let rec node seen type_ saved =
    let rec selected =
      {
        select_forwarding =
          (fun () ->
            if Type.pointer_depth type_ > 0 then Ok selected
            else
              match Type.base type_ with
              | Type.Primitive _ -> Ok selected
              | Type.Aggregate symbol -> (
                  if List.exists (( == ) symbol) seen then
                    invalid "has cyclic class forwarding or members"
                  else
                    let* record = selected_record type_ saved in
                    match Record.aggregate_initializer_backing record with
                    | None -> invalid "has no original forwarding selection"
                    | Some None -> Ok selected
                    | Some (Some (backing, saved)) ->
                        select_forwarding
                          (node (symbol :: seen)
                             (Type_reference.resolved_type backing)
                             saved)));
        capture_scalar =
          (fun () ->
            if Type.pointer_depth type_ > 0 then
              begin_scalar_evaluation (fixed_node (fun () -> primitive type_))
            else
              match Type.base type_ with
              | Type.Primitive _ ->
                  begin_scalar_evaluation
                    (fixed_node (fun () -> primitive type_))
              | Type.Aggregate symbol -> (
                  if List.exists (( == ) symbol) seen then
                    invalid "has cyclic class forwarding or members"
                  else
                    let* record = selected_record type_ saved in
                    let seen = symbol :: seen in
                    match Record.aggregate_initializer_backing record with
                    | Some (Some (backing, saved)) ->
                        begin_scalar_evaluation
                          (node seen
                             (Type_reference.resolved_type backing)
                             saved)
                    | None -> invalid "has no original forwarding selection"
                    | Some None -> (
                        if
                          (not
                             (Record.aggregate_initializer_has_members record))
                          || Record.aggregate_fields record <> []
                        then
                          invalid
                            "requires its original direct member traversal"
                        else
                          let* before =
                            scalar ~owner:symbol ~type_:default_word
                              (Record.aggregate_initializer_size record)
                          in
                          match before with
                          | Aggregate _ -> assert false
                          | Scalar before ->
                              Ok
                                (evaluation
                                   ~dynamic:
                                     (not
                                        (Record
                                         .aggregate_initializer_is_complete
                                           record))
                                   before
                                   (fun () ->
                                     let* current =
                                       selected_record type_ (Some record)
                                     in
                                     let* settled =
                                       scalar ~owner:symbol ~type_:default_word
                                         (Record.aggregate_initializer_size
                                            current)
                                     in
                                     match settled with
                                     | Scalar settled -> Ok settled
                                     | Aggregate _ -> assert false)))));
        read =
          (fun () ->
            if Type.pointer_depth type_ > 0 then primitive type_
            else
              match Type.base type_ with
              | Type.Primitive _ -> primitive type_
              | Type.Aggregate symbol -> (
                  if List.exists (( == ) symbol) seen then
                    invalid "has cyclic class forwarding or members"
                  else
                    let* record = selected_record type_ saved in
                    let seen = symbol :: seen in
                    match Record.aggregate_initializer_backing record with
                    | None -> invalid "has no original forwarding selection"
                    | Some (Some (backing, saved)) ->
                        view
                          (node seen
                             (Type_reference.resolved_type backing)
                             saved)
                    | Some None
                      when not (Record.aggregate_initializer_is_complete record)
                      ->
                        invalid
                          "requires a completed selected class; live forward \
                           completion is not yet supported"
                    | Some None -> (
                        if not (Record.aggregate_initializer_has_members record)
                        then invalid "has no original direct member list"
                        else
                          match List.rev (Record.aggregate_fields record) with
                          | [] ->
                              scalar ~owner:symbol ~type_:default_word
                                (Record.aggregate_initializer_size record)
                          | fields ->
                              Ok
                                (Aggregate
                                   (List.map
                                      (fun field ->
                                        {
                                          type_ =
                                            (match
                                               Record.aggregate_field_callback
                                                 field
                                             with
                                            | None ->
                                                node seen
                                                  (Record.aggregate_field_type
                                                     field)
                                                  (Record.aggregate_field_record
                                                     field)
                                            | Some source ->
                                                callback
                                                  ~type_:
                                                    (Record.aggregate_field_type
                                                       field)
                                                  source);
                                          dimensions =
                                            Record.aggregate_field_counts field;
                                        })
                                      fields)))));
      }
    in
    selected
  in
  if not (Record.aggregate_record_matches ~owner:original ~type_ original) then
    invalid "root disagrees with its selected class record"
  else Ok (node [] type_ (Some original))

let of_members ~headers ~members ~before_item_index ~type_ =
  let module Members = Aggregate_member_index in
  let module Headers = Aggregate_header_resolution in
  let callback_field field =
    fixed_node (fun () ->
        match
          Option.bind
            (Members.member_function_pointer field)
            Function_type_resolution.function_pointer_source
        with
        | Some source -> view (callback source)
        | None -> invalid "callback has no original source header")
  in
  let rec node seen type_ =
    fixed_node (fun () ->
        if Type.pointer_depth type_ > 0 then primitive type_
        else
          match Type.base type_ with
          | Type.Primitive _ -> primitive type_
          | Type.Aggregate symbol -> (
              if List.exists (( == ) symbol) seen then
                invalid "has cyclic class forwarding or members"
              else
                match Members.find_aggregate members symbol with
                | Some aggregate
                  when Members.owns_symbol members symbol
                       && Members.aggregate_symbol aggregate == symbol
                       && Members.aggregate_item_index aggregate
                          <= before_item_index -> (
                    let* header =
                      match
                        List.find_opt
                          (fun header ->
                            Headers.header_symbol header == symbol
                            && Headers.header_item_index header
                               = Members.aggregate_item_index aggregate)
                          (Headers.headers headers)
                      with
                      | Some header -> Ok header
                      | None -> invalid "has no exact original class header"
                    in
                    let seen = symbol :: seen in
                    match Headers.header_backing header with
                    | Some backing ->
                        view (node seen (Headers.backing_type backing))
                    | None -> (
                        match aggregate.Members.direct_members with
                        | [] ->
                            scalar ~owner:symbol ~type_:default_word
                              (Members.aggregate_size aggregate)
                        | fields ->
                            Ok
                              (Aggregate
                                 (List.map
                                    (fun field ->
                                      let layout =
                                        Members.member_layout field
                                      in
                                      {
                                        type_ =
                                          (if
                                             Members.member_is_function_pointer
                                               field
                                           then callback_field field
                                           else
                                             node seen
                                               (Members.member_type field));
                                        dimensions =
                                          layout.Aggregate_layout.dimensions;
                                      })
                                    fields))))
                | _ ->
                    invalid
                      "requires the original completed class before its \
                       consumer"))
  in
  let result = node [] type_ in
  let* _ = view result in
  Ok result
