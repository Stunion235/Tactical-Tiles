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

(**Return the correct style to use given a tile number and the grid size it
   belongs to.*)
let ansi_style_from_tile_and_size t s =
  ANSITerminal.(
    if t <= s || t mod s = 1 then red
    else if t <= s * 2 || t mod s = 2 then yellow
    else if t <= s * 3 || t mod s = 3 then green
    else if t <= s * 4 || t mod s = 4 then cyan
    else if t <= s * 5 || t mod s = 5 then blue
    else if t <= s * 6 || t mod s = 6 then magenta
    else white)

let print_grid_styled g =
  let ints = Board.to_intarrayarray g in
  let h = Array.length ints in
  let w = Array.length ints.(0) in
  print_char '*';
  for c = 0 to w - 1 do
    print_string "---*"
  done;
  print_newline ();
  for r = 0 to h - 1 do
    print_char '|';
    for c = 0 to Array.length ints.(r) - 1 do
      let s = string_of_int ints.(r).(c) in
      (if ints.(r).(c) < 0 then print_string "   "
       else
         match String.length s with
         | 0 -> print_string "   "
         | 1 ->
             ANSITerminal.print_string
               [
                 ansi_style_from_tile_and_size ints.(r).(c) h;
                 ANSITerminal.on_default;
               ]
               (" " ^ s ^ " ")
         | 2 ->
             ANSITerminal.print_string
               [
                 ansi_style_from_tile_and_size ints.(r).(c) h;
                 ANSITerminal.on_default;
               ]
               (" " ^ s)
         | 3 ->
             ANSITerminal.print_string
               [
                 ansi_style_from_tile_and_size ints.(r).(c) h;
                 ANSITerminal.on_default;
               ]
               s
         | _ ->
             ANSITerminal.print_string
               [
                 ansi_style_from_tile_and_size ints.(r).(c) h;
                 ANSITerminal.on_default;
               ]
               (String.sub s 0 3));
      print_char '|'
    done;
    print_string "\n*";
    for c = 0 to w - 1 do
      print_string "---*"
    done;
    print_newline ()
  done

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
