let input = open_in "inputs/day2.txt" in
let rec is_safe_op b binop l =
  match l with
  | x :: y :: tl ->
      is_safe_op (b && binop x y && Int.abs (y - x) <= 3) binop (y :: tl)
  | _ ->
      b
in
let is_safe l = is_safe_op true ( < ) l || is_safe_op true ( > ) l in
try
  let lines = In_channel.input_lines input in
  let parse_line x =
    List.map int_of_string
    @@ List.filter (fun x -> x <> "")
    @@ String.split_on_char ' ' @@ String.trim x
  in
  let result =
    List.length
    @@ List.filter (Bool.equal true)
    @@ List.map is_safe @@ List.map parse_line lines
  in
  let () = print_int result in
  let () = print_newline () in
  close_in input
with e -> close_in_noerr input ; raise e
