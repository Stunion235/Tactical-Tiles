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

let move_tile grid direction =
  let ey, ex = find_empty grid in
  match direction with
  | "W" | "w" ->
      if in_bound (ey + 1) ex grid then (
        grid.(ey).(ex) <- grid.(ey + 1).(ex);
        grid.(ey + 1).(ex) <- Empty)
      else ()
  | "A" | "a" ->
      if in_bound ey (ex + 1) grid then (
        grid.(ey).(ex) <- grid.(ey).(ex + 1);
        grid.(ey).(ex + 1) <- Empty)
      else ()
  | "S" | "s" ->
      if in_bound (ey - 1) ex grid then (
        grid.(ey).(ex) <- grid.(ey - 1).(ex);
        grid.(ey - 1).(ex) <- Empty)
      else ()
  | "D" | "d" ->
      if in_bound ey (ex - 1) grid then (
        grid.(ey).(ex) <- grid.(ey).(ex - 1);
        grid.(ey).(ex - 1) <- Empty)
      else ()
  | _ -> print_endline "This is an invalid input. Type w, a, s, or d."

let to_intlistlist (g : grid) =
  to_intarrayarray g |> Array.map Array.to_list |> Array.to_list

let shuffle board difficulty =
  let direction = [ "W"; "A"; "S"; "D" ] in
  let difficulty = String.lowercase_ascii difficulty in
  let rec shuffle_aux n =
    match n with
    | 0 -> ()
    | _ ->
        move_tile board (List.nth direction (Random.int 4));
        shuffle_aux (n - 1)
  in
  match difficulty with
  | "easy" -> shuffle_aux (Array.length board * Array.length board)
  | "medium" -> shuffle_aux (Array.length board * Array.length board * 4)
  | "hard" -> shuffle_aux (Array.length board * Array.length board * 8)
  | _ -> print_endline "Enter a valid difficulty"
