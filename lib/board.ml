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
let create_tile value = if value < 0 then Empty else Number value

let tile_to_string value =
  match value with
  | Empty -> ""
  | Number x -> string_of_int x
