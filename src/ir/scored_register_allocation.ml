type candidate = {
  member : int;
  score : int64;
  offset : int64;
  lea_balance : int64;
  pointer_depth : int;
  has_dimensions : bool;
  name : string;
}

let sort candidates =
  let members = Array.of_list candidates in
  (* COptMemberVar is 40 bytes, hence QSort dispatches to QSort2b, not the
     pointer-sized specialization. The pivot is a copied record. Translating
     byte pointers to array indices preserves every equal-score swap. *)
  let rec qsort base count =
    if count > 1 then (
      let left = ref base and right = ref (base + count - 1) in
      let pivot = members.(base + (count / 2)) in
      while !left <= !right do
        while Int64.compare members.(!left).score pivot.score > 0 do
          incr left
        done;
        while Int64.compare members.(!right).score pivot.score < 0 do
          decr right
        done;
        if !left <= !right then (
          let temporary = members.(!right) in
          members.(!right) <- members.(!left);
          members.(!left) <- temporary;
          incr left;
          decr right)
      done;
      let lower = 1 + !right - base in
      if lower > 1 && lower < count then qsort base lower;
      let upper = count + base - !left in
      if upper > 1 && upper < count then qsort !left upper)
  in
  qsort 0 (Array.length members);
  Array.to_list members

let allocate ~reserved candidates =
  (* OptPass3.HC:538-553 sorts before removing zeros. Removing them first
     changes the unstable equal-score order of the remaining members. *)
  let rec trim = function
    | member :: rest when member.score = 0L -> trim rest
    | members -> members
  in
  let candidates =
    sort candidates |> List.rev |> trim |> List.rev
    |> List.filter (fun member ->
        member.lea_balance = 0L && member.offset <> 0L)
  in
  let used = ref reserved and assigned = ref [] in
  let assign registers eligible members =
    let registers =
      ref (List.filter (fun number -> not (List.mem number !used)) registers)
    in
    List.iter
      (fun member ->
        if eligible member && not (List.mem_assoc member.member !assigned) then
          match !registers with
          | [] -> ()
          | number :: rest ->
              registers := rest;
              used := number :: !used;
              assigned := (member.member, number) :: !assigned)
      members
  in
  (* OptPass3.HC:557-595 counts pointers and dimensioned members in j, but
     scans past them for R12/R13. Parameter arrays can use the general bank
     unless their original name is argv. Explicit reservations do not lower j. *)
  if List.length candidates > 6 then
    assign [ 12; 13 ]
      (fun member -> member.pointer_depth = 0 && not member.has_dimensions)
      candidates;
  assign [ 6; 7; 10; 11; 14; 15 ]
    (fun member ->
      (not member.has_dimensions)
      || (member.offset > 0L && member.name <> "argv"))
    candidates;
  List.rev !assigned
