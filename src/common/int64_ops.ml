let mul_add ~base value ~digit =
  Int64.add (Int64.mul value base) (Int64.of_int digit)

let shift_add ~bits value ~digit =
  Int64.logor (Int64.shift_left value bits) (Int64.of_int digit)

let of_digits ~base ~digit text =
  if base < 2 then invalid_arg "integer base must be at least two";
  let rec loop index value =
    if index = String.length text then Some value
    else
      match digit text.[index] with
      | Some item when item < base ->
          loop (index + 1) (mul_add ~base:(Int64.of_int base) value ~digit:item)
      | _ -> None
  in
  if String.length text = 0 then None else loop 0 0L
