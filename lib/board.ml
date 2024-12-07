type tile =
  | Empty
  | Number of int

type grid = tile array array

let rec find_empty_tile_aux board i j =
  if i >= Array.length board then (-1, -1)
  else if j >= Array.length board.(i) then find_empty_tile_aux board (i + 1) 0
  else
    match board.(i).(j) with
    | Empty -> (i, j)
    | _ -> find_empty_tile_aux board i (j + 1)

let rec find_empty board = find_empty_tile_aux board 0 0

(**Negative ints are empty tiles.*)
let create_tile value = if value < 0 then Empty else Number value

let tile_to_string value =
  match value with
  | Empty -> ""
  | Number x -> string_of_int x

let of_intarrayarray i : grid = Array.map (Array.map create_tile) i

let to_intarrayarray (g : grid) =
  Array.map
    (Array.map (fun t ->
         match t with
         | Empty -> -1
         | Number n -> n))
    g

let initialize_board size =
  Array.init size (fun _ -> Array.init size (fun _ -> create_tile (-1)))

let fill_board currBoard =
  let value = ref 1 in
  for row = 0 to Array.length currBoard - 1 do
    let rowLength = Array.length currBoard.(row) in
    for col = 0 to rowLength - 1 do
      let currTile = create_tile !value in
      currBoard.(row).(col) <- currTile;
      incr value
    done
  done;
  currBoard.(Array.length currBoard - 1).(Array.length currBoard - 1) <-
    create_tile (-1)

let in_bound row column board =
  row < Array.length board
  && row >= 0
  && column < Array.length board.(0)
  && column >= 0

let is_move_valid board move =
  let ey, ex = find_empty board in
  match move with
  | "W" | "w" -> in_bound (ey + 1) ex board
  | "A" | "a" -> in_bound ey (ex + 1) board
  | "S" | "s" -> in_bound (ey - 1) ex board
  | "D" | "d" -> in_bound ey (ex - 1) board
  | _ -> false

let move_tile grid direction =
  let ey, ex = find_empty grid in
  match direction with
  | "W" | "w" ->
      if is_move_valid grid direction then (
        grid.(ey).(ex) <- grid.(ey + 1).(ex);
        grid.(ey + 1).(ex) <- Empty)
      else ()
  | "A" | "a" ->
      if is_move_valid grid direction then (
        grid.(ey).(ex) <- grid.(ey).(ex + 1);
        grid.(ey).(ex + 1) <- Empty)
      else ()
  | "S" | "s" ->
      if is_move_valid grid direction then (
        grid.(ey).(ex) <- grid.(ey - 1).(ex);
        grid.(ey - 1).(ex) <- Empty)
      else ()
  | "D" | "d" ->
      if is_move_valid grid direction then (
        grid.(ey).(ex) <- grid.(ey).(ex - 1);
        grid.(ey).(ex - 1) <- Empty)
      else ()
  | _ -> print_endline "This is an invalid input. Type w, a, s, or d."

let to_intlistlist (g : grid) =
  to_intarrayarray g |> Array.map Array.to_list |> Array.to_list

let shuffle board difficulty =
  let direction = [ "W"; "A"; "S"; "D" ] in
  let opposite_move = function
    | "W" -> "S"
    | "S" -> "W"
    | "A" -> "D"
    | "D" -> "A"
    | "" -> ""
    | _ -> failwith "Invalid move"
  in
  let difficulty = String.lowercase_ascii difficulty in
  let shuffle_moves = Stack.create () in
  let rec shuffle_aux n last_move =
    match n with
    | 0 -> ()
    | _ ->
        let rec get_next_move () =
          let move = List.nth direction (Random.int 4) in
          if
            (last_move = "" && is_move_valid board move)
            || (move <> opposite_move last_move && is_move_valid board move)
          then move
          else get_next_move ()
        in
        let move = get_next_move () in

        move_tile board move;
        Stack.push move shuffle_moves;
        shuffle_aux (n - 1) move
  in
  match difficulty with
  | "easy" ->
      shuffle_aux (Array.length board * Array.length board) "";
      shuffle_moves
  | "medium" ->
      shuffle_aux (Array.length board * Array.length board * 4) "";
      shuffle_moves
  | "hard" ->
      shuffle_aux (Array.length board * Array.length board * 8) "";
      shuffle_moves
  | _ ->
      print_endline "Enter a valid difficulty";
      shuffle_moves

let shuffle2 board difficulty =
  let direction = [ "W"; "A"; "S"; "D" ] in
  let opposite_move = function
    | "W" -> "S"
    | "S" -> "W"
    | "A" -> "D"
    | "D" -> "A"
    | "" -> ""
    | _ -> failwith "Invalid move"
  in
  let difficulty = String.lowercase_ascii difficulty in
  let shuffle_moves = Stack.create () in
  let rec shuffle_aux n last_move =
    match n with
    | 0 -> ()
    | _ ->
        let rec get_next_move () =
          let move = List.nth direction (Random.int 4) in
          if
            (last_move = "" && is_move_valid board move)
            || (move <> opposite_move last_move && is_move_valid board move)
          then move
          else get_next_move ()
        in
        let move = get_next_move () in

        move_tile board move;
        Stack.push move shuffle_moves;
        shuffle_aux (n - 1) move
  in
  match difficulty with
  | "easy" -> shuffle_aux (Array.length board * Array.length board) ""
  | "medium" -> shuffle_aux (Array.length board * Array.length board * 4) ""
  | "hard" -> shuffle_aux (Array.length board * Array.length board * 8) ""
  | _ -> print_endline "Enter a valid difficulty"

let check_correct_board board =
  let size = Array.length board in
  let acc = ref 1 in
  try
    Array.iteri
      (fun i row ->
        Array.iteri
          (fun j tile ->
            match tile with
            | Empty -> if (i, j) <> (size - 1, size - 1) then raise Exit
            | Number n -> if n <> !acc then raise Exit else acc := !acc + 1)
          row)
      board;
    true
  with Exit -> false

let copy_board (board : grid) : grid =
  Array.map (fun row -> Array.map (fun tile -> tile) row) board
