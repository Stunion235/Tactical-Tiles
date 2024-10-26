type tile_value =
  | Empty
  | Number of int

type grid = tile_value array array

let int_to_tile_value i = if i < 0 then Empty else Number i
let intarray_to_grid i = Array.map (Array.map int_to_tile_value) i

let print_grid (g : tile_value array array) =
  let h = Array.length g in
  let w = Array.length g.(0) in
  print_char '*';
  for c = 0 to w - 1 do
    print_string "---*"
  done;
  print_endline "";
  for r = 0 to h - 1 do
    print_char '|';
    for c = 0 to w - 1 do
      match g.(r).(c) with
      | Number n ->
          let s = string_of_int n in
          print_string
            ((match String.length s with
             | 0 -> "   "
             | 1 -> " " ^ s ^ " "
             | 2 -> " " ^ s
             | _ -> s)
            ^ "|")
      | Empty -> print_string "|   |"
    done;
    print_endline "";
    print_char '*';
    for c = 0 to w - 1 do
      print_string "---*"
    done;
    print_endline ""
  done
