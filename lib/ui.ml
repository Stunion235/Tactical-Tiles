open Board

(**For best results, g has at least one row and one column and is rectangular
   (all rows have the same number of columns)*)
let grid_to_string g =
  let ints = Board.to_intarrayarray g in
  let h = Array.length ints in
  let w = Array.length ints.(0) in
  let acc = ref "" in
  acc := !acc ^ "*";
  for c = 0 to w - 1 do
    acc := !acc ^ "---*"
  done;
  acc := !acc ^ "\n";
  for r = 0 to h - 1 do
    acc := !acc ^ "|";
    for c = 0 to Array.length ints.(r) - 1 do
      let s = string_of_int ints.(r).(c) in
      acc :=
        if ints.(r).(c) < 0 then !acc ^ "   |"
        else
          !acc
          ^ (match String.length s with
            | 0 -> "   "
            | 1 -> " " ^ s ^ " "
            | 2 -> " " ^ s
            | 3 -> s
            | _ -> String.sub s 0 3)
          ^ "|"
    done;
    acc := !acc ^ "\n";
    acc := !acc ^ "*";
    for c = 0 to w - 1 do
      acc := !acc ^ "---*"
    done;
    acc := !acc ^ "\n"
  done;
  !acc

let print_grid g = print_endline (grid_to_string g)

let simulate_solution ?(delay = 1.0) ?(debug = false) board shuffle_moves =
  while not (Stack.is_empty shuffle_moves) do
    let move = Stack.pop shuffle_moves in
    let inverse_move =
      match move with
      | "w" | "W" -> "s"
      | "a" | "A" -> "d"
      | "s" | "S" -> "w"
      | "d" | "D" -> "a"
      | _ -> failwith "Invalid move"
    in
    move_tile board inverse_move;
    if not debug then print_grid board;
    if delay > 0.0 then Unix.sleepf delay
  done
