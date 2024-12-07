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

let compress board =
  for row = 0 to 3 do
    let empty_pos = ref 0 in
    for col = 0 to 3 do
      if board.(row).(col) = Empty then empty_pos := !empty_pos + 1
      else if !empty_pos <> 0 then (
        board.(row).(col - !empty_pos) <- board.(row).(col);
        board.(row).(col) <- Empty;
        empty_pos := 1)
    done
  done;
  board

let merge board =
  for row = 0 to 3 do
    for col = 0 to 2 do
      if board.(row).(col) = board.(row).(col + 1) && board.(row).(col) <> Empty
      then (
        let value = function
          | Number x -> x
          | Empty -> -1
        in
        board.(row).(col) <- Number (value board.(row).(col) * 2);
        board.(row).(col + 1) <- Empty)
      else ()
    done
  done;
  board

let reverse board =
  let new_board = make_empty_board () in
  for row = 0 to 3 do
    for col = 0 to 3 do
      let currTile = board.(row).(col) in
      new_board.(row).(3 - col) <- currTile
    done
  done;
  new_board

let move_left board = compress (merge (compress board))
let move_up board = turn (move_left (turn board))
let move_right board = reverse (move_left (reverse board))
let move_down board = turn (move_right (turn board))
let curr_state board = failwith ""

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
