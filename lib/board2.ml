(** AF: A value of type [grid] represents the state of a 2048 game board. The
    game board is a two-dimensional array of tiles, where each tile is either
    [Empty] or [Number n] representing a tile on the board with a value n.
    Conceptually, [grid.(i).(j)] corresponds to the cell at row i and column j
    on the 2048 board.

    RI:
    - [grid] must form a proper square: it must have at least one row, and the
      number of rows must equal the number of columns.
    - The board must be 4x4.
    - For any [Number n] tile, n must be a positive integer power of 2 (e.g., 2,
      4, 8, 16, ...).
    - Any number of tiles (including zero) may be [Empty]. *)
type tile =
  | Empty
  | Number of int

type grid = tile array array

let create_tile value = if value < 0 then Empty else Number value

let add_new board =
  let loc_row = ref (Random.int 4) in
  let loc_col = ref (Random.int 4) in
  while board.(!loc_row).(!loc_col) <> Empty do
    loc_row := Random.int 4;
    loc_col := Random.int 4
  done;
  board.(!loc_row).(!loc_col) <- Number 2;
  board

let make_empty_board () =
  Array.init 4 (fun _ -> Array.init 4 (fun _ -> create_tile (-1)))

let make_board () =
  let curr = make_empty_board () in
  add_new (add_new curr)

let turn board =
  let new_board = make_empty_board () in
  for row = 0 to 3 do
    for col = 0 to 3 do
      let currTile = board.(row).(col) in
      new_board.(col).(row) <- currTile
    done
  done;
  new_board

let compress board direction =
  let compress_row row start_index step =
    let next_pos = ref start_index in
    for i = start_index to start_index + (3 * step) do
      match row.(i) with
      | Empty -> ()
      | _ ->
          if !next_pos <> i then (
            row.(!next_pos) <- row.(i);
            row.(i) <- Empty);
          next_pos := !next_pos + step
    done
  in

  match direction with
  | "left" ->
      for row = 0 to 3 do
        compress_row board.(row) 0 1
      done;
      board
  | "right" ->
      for row = 3 downto 0 do
        compress_row board.(row) 3 (-1)
      done;
      board
  | "up" ->
      for col = 0 to 3 do
        let next_pos = ref 0 in
        for row = 0 to 3 do
          match board.(row).(col) with
          | Empty -> ()
          | _ ->
              if !next_pos <> row then (
                board.(!next_pos).(col) <- board.(row).(col);
                board.(row).(col) <- Empty);
              incr next_pos
        done
      done;
      board
  | "down" ->
      for col = 0 to 3 do
        let next_pos = ref 3 in
        for row = 3 downto 0 do
          match board.(row).(col) with
          | Empty -> ()
          | _ ->
              if !next_pos <> row then (
                board.(!next_pos).(col) <- board.(row).(col);
                board.(row).(col) <- Empty);
              decr next_pos
        done
      done;
      board
  | _ -> failwith "Invalid direction. Must be 'left', 'right', 'up', or 'down'"

let merge board direction =
  let merge_row row start_index step =
    let skip_next = ref false in
    for i = start_index to start_index + (2 * step) do
      if !skip_next then skip_next := false
      else
        match (row.(i), row.(i + step)) with
        | Number x, Number y when x = y ->
            row.(i) <- Number (2 * x);
            row.(i + step) <- Empty;
            skip_next := true
        | _ -> ()
    done
  in

  match direction with
  | "left" ->
      for row = 0 to 3 do
        merge_row board.(row) 0 1
      done;
      board
  | "right" ->
      for row = 3 downto 1 do
        merge_row board.(row) 3 (-1)
      done;
      board
  | "up" ->
      for col = 0 to 3 do
        let skip_next = ref false in
        for row = 0 to 2 do
          if !skip_next then skip_next := false
          else
            match (board.(row).(col), board.(row + 1).(col)) with
            | Number x, Number y when x = y ->
                board.(row).(col) <- Number (2 * x);
                board.(row + 1).(col) <- Empty;
                skip_next := true
            | _ -> ()
        done
      done;
      board
  | "down" ->
      for col = 0 to 3 do
        let skip_next = ref false in
        for row = 3 downto 1 do
          if !skip_next then skip_next := false
          else
            match (board.(row).(col), board.(row - 1).(col)) with
            | Number x, Number y when x = y ->
                board.(row).(col) <- Number (2 * x);
                board.(row - 1).(col) <- Empty;
                skip_next := true
            | _ -> ()
        done
      done;
      board
  | _ -> failwith "Invalid direction. Must be 'left', 'right', 'up', or 'down'"

let reverse board =
  let new_board = make_empty_board () in
  for row = 0 to 3 do
    for col = 0 to 3 do
      let currTile = board.(row).(col) in
      new_board.(row).(3 - col) <- currTile
    done
  done;
  new_board

let move_left board = compress (merge (compress board "left") "left") "left"
let move_up board = compress (merge (compress board "up") "up") "up"
let move_right board = compress (merge (compress board "right") "right") "right"
let move_down board = compress (merge (compress board "down") "down") "down"

let get_value board (i, j) =
  match board.(i).(j) with
  | Empty -> -1
  | Number x -> x

let curr_state board =
  let won = ref false in
  let empty = ref false in
  for i = 0 to 3 do
    for j = 0 to 3 do
      if get_value board (i, j) = 2048 then won := true
      else if get_value board (i, j) = -1 then empty := true
    done
  done;
  if (not !empty) && not !won then
    for i = 0 to 2 do
      for j = 0 to 2 do
        if
          get_value board (i, j) = get_value board (i + 1, j)
          || get_value board (i, j) = get_value board (i, j + 1)
        then empty := true
      done
    done;
  for i = 0 to 2 do
    if get_value board (i, 3) = get_value board (i + 1, 3) then empty := true
  done;
  for j = 0 to 2 do
    if get_value board (3, j) = get_value board (3, j + 1) then empty := true
  done;
  if !won = true then "WON"
  else if !empty = true then "GAME NOT OVER"
  else "LOST"

let make_move board = function
  | "W" | "w" -> board |> move_up |> add_new
  | "A" | "a" -> board |> move_left |> add_new
  | "S" | "s" -> board |> move_down |> add_new
  | "D" | "d" -> board |> move_right |> add_new
  | _ ->
      print_endline "This is an invalid input. Type w, a, s, or d";
      board

let to_intarrayarray (g : grid) =
  Array.map
    (Array.map (fun t ->
         match t with
         | Empty -> -1
         | Number n -> n))
    g

let to_intlistlist (g : grid) =
  to_intarrayarray g |> Array.map Array.to_list |> Array.to_list

let of_intarrayarray i : grid = Array.map (Array.map create_tile) i

let to_intarrayarray (g : grid) =
  Array.map
    (Array.map (fun t ->
         match t with
         | Empty -> -1
         | Number n -> n))
    g
