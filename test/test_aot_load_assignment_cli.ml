open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let c = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in c)
    (fun () -> really_input_string c (in_channel_length c))

let with_file suffix f =
  let p = Filename.temp_file "holyc aot assignment " suffix in
  Fun.protect ~finally:(fun () -> Sys.remove p) (fun () -> f p)

let compiler = Sys.argv.(1)
let native = Array.length Sys.argv = 3 && Sys.argv.(2) = "--native"
let reports = ref 0

let invoke target mode source options =
  with_file ".hc" (fun input ->
      let c = open_out_bin input in
      output_string c source;
      close_out c;
      with_file ".out" (fun output ->
          with_file ".err" (fun errors ->
              let out_fd =
                Unix.openfile output [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              and err_fd =
                Unix.openfile errors [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              in
              let args =
                [
                  compiler;
                  "run";
                  "--format=json";
                  "--target=" ^ target;
                  "--mode=" ^ mode;
                  "--code-byte-limit=524288";
                ]
                @ options @ [ input ]
              in
              let pid =
                Fun.protect
                  ~finally:(fun () ->
                    Unix.close out_fd;
                    Unix.close err_fd)
                  (fun () ->
                    Unix.create_process compiler (Array.of_list args) Unix.stdin
                      out_fd err_fd)
              in
              let _, status = Unix.waitpid [] pid in
              require (read errors = "") "JSON diagnostics escaped to stderr";
              let report = Yojson.Safe.from_string (read output) in
              incr reports;
              require
                (member "target" report = `String target)
                "requested executor";
              require
                (member "mode" report = `String mode)
                "requested outer mode";
              require
                (member "command_error" report = `Null)
                (Yojson.Safe.to_string report);
              require
                (member "output_hex" report = `String "")
                (Yojson.Safe.to_string report);
              (status, report))))

let success ?(expected = 42L) ?(warnings = 0) target mode source options =
  let status, report = invoke target mode source options in
  require
    (status = Unix.WEXITED 0 && member "outcome" report = `String "success")
    (Yojson.Safe.to_string report);
  require
    (report |> member "final_value" |> member "value"
    = `String (Int64.to_string expected))
    (Yojson.Safe.to_string report);
  require
    (let diagnostics = report |> member "diagnostics" |> to_list in
     List.length diagnostics = warnings
     && List.for_all
          (fun d ->
            member "code" d = `String "HCSEMA0079"
            && member "severity" d = `String "warning")
          diagnostics)
    (Yojson.Safe.to_string report);
  report

let rejected ?code target mode source options =
  let status, report = invoke target mode source options in
  require
    (status = Unix.WEXITED 1 && member "outcome" report = `String "error")
    (Yojson.Safe.to_string report);
  require
    (member "diagnostics" report <> `List [])
    "rejection requires diagnostics";
  List.iter
    (fun diagnostic ->
      let code = diagnostic |> member "code" |> to_string in
      require
        (not
           (String.starts_with ~prefix:"HCPARSE" code
           || String.starts_with ~prefix:"HCLEX" code))
        "unsupported U0 use must reach its semantic/execution boundary")
    (report |> member "diagnostics" |> to_list);
  Option.iter
    (fun expected ->
      require
        (report |> member "diagnostics" |> to_list
        |> List.exists (fun diagnostic ->
            member "code" diagnostic = `String expected))
        (Yojson.Safe.to_string report))
    code

let words =
  [
    ( "I64 Get(){return 7;}class Box{U0 zero;I64 value;};Box \
       object={Get(),0x112233445566772a};I64 F(){return object.value;}F();",
      0x112233445566772aL,
      0x1122334400000007L );
    ( "I64 Get(){return 7;}class Box{U0 zero;I64 value;};I64 F(){static Box \
       object={Get(),0x112233445566772a};return object.value;}F();",
      0x112233445566772aL,
      0x1122334400000007L );
    ( "I64 Get(){return 7;}class Empty{};class Box{Empty empty;I64 value;};Box \
       object={Get(),42};I64 F(){return object.value;}F();",
      42L,
      7L );
    ( "I64 Get(){return 7;}class Base{U8 bytes[3];};class Leaf:Base{};class \
       Box{Leaf leaf;U8 tail[5];};Box object={Get(),{5,4,3,2,1}};I64 \
       F(){return *(&object)(I64*);}F();",
      0x0102030405000007L,
      7L );
    ( "I64 Get(){return 7;}class Box{U8 prefix[3];U0 zero;I64 value;};Box \
       object={{1,2,3},Get(),0x112233445566772a};I64 \
       F(){if(object.prefix[0]!=1||object.prefix[2]!=3)return -1;return \
       object.value;}F();",
      0x112233445566772aL,
      0x1122334400000007L );
    ( "I64 Get(){return 7;}class Box{U8 prefix[3];U0 zero;U8 tail[4];};I64 \
       F(){static Box object={{1,2,3},Get(),{42,2,3,4}};return \
       object.tail[0];}F();",
      42L,
      7L );
  ]

let widths =
  List.concat_map
    (fun (type_, value, expected) ->
      let source =
        Printf.sprintf
          "%s Get(){return %s;}class Box{U0 zero;I64 value;};Box \
           object={Get(),0x112233445566772a};I64 F(){return object.value;}F();"
          type_ value
      in
      [ (source, 0x112233445566772aL, expected) ])
    ([ "Bool"; "I8"; "U8"; "I16"; "U16"; "I32"; "U32"; "I64"; "U64" ]
    |> List.map (fun t -> (t, "7", 0x1122334400000007L)))
  @ List.map
      (fun type_ ->
        ( Printf.sprintf
            "%s Get(){return -1;}class Box{U0 zero;I64 value;};Box \
             object={Get(),0x112233445566772a};I64 F(){return \
             object.value;}F();"
            type_,
          0x112233445566772aL,
          0x11223344ffffffffL ))
      [ "I8"; "I16"; "I32"; "I64" ]

let callback =
  "I64 Fn(){return 99;}union Box{U0 zero;I64 (*p)();};I64 Init(Box \
   *b){b->p=&Fn;return 7;}I64 Observe(Box *b){return b->p();}Box \
   object={Init(&object),Observe(&object)};I64 F(){return \
   *(&object)(I64*);}F();"

let pointer =
  "I64 target=99;union Box{U0 zero;I64 *p;};I64 Init(Box \
   *b){b->p=&target;return 7;}I64 *Observe(Box *b){return b->p;}Box \
   object={Init(&object),Observe(&object)};I64 F(){return *object.p;}F();"

let bounds =
  [
    "public _intern 0xA9 I64 Abs(I64 n);class Box{U0 zero;I64 value;};Box \
     object={Abs(-7),42};I64 F(){return object.value;}F();";
    "I64 Get(){return 7;}class Box{U8 prefix[3];U0 zero;U8 tail[3];};Box \
     object={{1,2,3},Get(),{42,2,3}};42;";
    "I64 Get(){return 7;}class Box{U8 prefix[3];U0 zero;U8 tail[3];};I64 \
     F(){static Box object={{1,2,3},Get(),{42,2,3}};return \
     object.tail[0];}F();";
    "I64 Get(){return 7;}class Empty{};Empty object=Get();I64 \
     neighbor=42;neighbor;";
    "U0 Get(){}class Box{U0 zero;I64 value;};Box object={Get(),42};42;";
  ]

let () =
  List.iter
    (fun target ->
      List.iter
        (fun (source, jit, aot) ->
          ignore (success ~expected:aot target "aot" source []);
          if target <> "host-jit" then
            ignore (success ~expected:jit target "jit" source []))
        (words @ widths);
      List.iter
        (fun source -> rejected ~code:"HCRUN0006" target "aot" source [])
        bounds;
      List.iter
        (fun (source, code) ->
          rejected ~code target "aot" source [];
          if target <> "host-jit" then
            ignore (success ~expected:99L target "jit" source []))
        [ (callback, "HCIRVM0024"); (pointer, "HCIRVM0018") ];
      let source, _, expected = List.hd words in
      let baseline = success ~expected target "aot" source [] in
      let steps = baseline |> member "executed_steps" |> to_int in
      let preparation =
        baseline |> member "compiled_initializer_steps" |> to_int
      in
      require
        (steps > 1 && preparation > 1)
        "actual load stores and constant image preparation";
      ignore
        (success ~expected target "aot" source
           [
             "--step-limit=" ^ string_of_int steps;
             "--initializer-step-limit=" ^ string_of_int preparation;
             "--global-byte-limit=8";
           ]);
      rejected ~code:"HCIRVM0007" target "aot" source
        [ "--step-limit=" ^ string_of_int (steps - 1) ];
      rejected ~code:"HCIRVM0007" target "aot" source
        [ "--initializer-step-limit=" ^ string_of_int (preparation - 1) ];
      rejected target "aot" source [ "--global-byte-limit=7" ])
    (if native then [ "host-jit-task"; "host-jit" ] else [ "ir" ]);
  Printf.printf "AOT assignment CLI passed: %d reports\n" !reports
