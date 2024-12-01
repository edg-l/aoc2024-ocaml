let input = open_in "inputs/day1_p1.txt" in
try
  let lines = In_channel.input_lines input in
  let parse_line x =
    List.map int_of_string
    @@ List.filter (fun x -> x <> "")
    @@ String.split_on_char ' ' @@ String.trim x
  in
  let values = List.map parse_line lines in
  let lhs, rhs =
    List.split
    @@ List.filter_map
         (fun value ->
           match value with [x; y] -> Option.Some (x, y) | _ -> Option.None )
         values
  in
  let lhs = List.fast_sort Int.compare lhs in
  let rhs = List.fast_sort Int.compare rhs in
  let result =
    List.map (fun (x, y) -> Int.abs (y - x)) @@ List.combine lhs rhs
  in
  let result = List.fold_left ( + ) 0 result in
  let () = print_int result in
  let () = print_newline () in
  close_in input
with e -> close_in_noerr input ; raise e
