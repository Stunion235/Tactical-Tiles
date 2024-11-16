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

let initialize_board difficulty size =
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
  row <= Array.length board && column <= Array.length board.(0)

let move_tile grid direction =
  let ex, ey = find_empty grid in
  match direction with
  | "W" | "w" ->
      if in_bound ex (ey + 1) grid then (
        grid.(ex).(ey) <- grid.(ex).(ey + 1);
        grid.(ex).(ey + 1) <- Empty)
      else print_endline "Unable to move block upwards."
  | "A" | "a" ->
      if in_bound (ex + 1) ey grid then (
        grid.(ex).(ey) <- grid.(ex + 1).(ey);
        grid.(ex + 1).(ey) <- Empty)
      else print_endline "Unable to move to the left."
  | "S" | "s" ->
      if in_bound ex (ey - 1) grid then (
        grid.(ex).(ey) <- grid.(ex).(ey - 1);
        grid.(ex).(ey - 1) <- Empty)
      else print_endline "Unable to move the block downwards."
  | "D" | "d" ->
      if in_bound (ex - 1) ey grid then (
        grid.(ex).(ey) <- grid.(ex - 1).(ey);
        grid.(ex - 1).(ey) <- Empty)
      else print_endline "Unable to move the block to the right."
  | _ -> print_endline "This is an invalid input. Please try again" 


let to_intlistlist (g : grid) =
  to_intarrayarray g |> Array.map Array.to_list |> Array.to_list
